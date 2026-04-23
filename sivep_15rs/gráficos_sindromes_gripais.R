# ==============================================================================
# ANÁLISE SIVEP-GRIPE — SÍNDROME GRIPAL (SG) — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# ==============================================================================
# Autor       : Valentim Sala Junior
# Atualizado  : 2025
# Descrição   : Script modular para análise epidemiológica de Síndrome Gripal
#               (atendimentos ambulatoriais em unidades sentinela).
#               Complementa o script de SRAG hospitalar.
#               Fonte: SIVEP-Gripe — base SGHOSP (SG sentinela)
# ==============================================================================
#
# ATENÇÃO — DIFERENÇA ENTRE AS BASES:
#   - SRAGHOSP####.dbf → hospitalizações por SRAG (script principal)
#   - SGHOSP####.dbf   → atendimentos ambulatoriais por Síndrome Gripal (este script)
#   Ambas fazem parte do SIVEP-Gripe mas têm populações e objetivos distintos.
# ==============================================================================


# ==============================================================================
# BLOCO 0 — CONFIGURAÇÃO GLOBAL (EDITE AQUI)
# ==============================================================================

# --- 0.1 Seleção de Ano(s) ---
# Para um único ano:   ANO_ANALISE <- 2026
# Para múltiplos:      ANO_ANALISE <- c(2024, 2025)
# Para todos:          ANO_ANALISE <- NULL
#
# Curva comparativa (Bloco 6):
#   Ano único  → inclui automaticamente os 2 anos anteriores
#   Múltiplos  → usa os próprios anos
#   NULL       → todos os anos disponíveis
ANO_ANALISE <- 2026

# --- 0.2 Seleção de Município ---
# Use o nome exatamente como aparece no arquivo IBGE.
# NULL → toda a 15ª Regional
MUNICIPIO_ANALISE <- NULL

# --- 0.3 Caminhos de arquivo ---
# Base SG: arquivos nomeados SGHOSP####.dbf (diferente de SRAGHOSP####.dbf)
DIRETORIO_DBF <- "C:/SIVEPGRIPE/BaseDBF"

ARQUIVO_IBGE  <- "C:/Users/valentim.junior/OneDrive/Área de Trabalho/sivep_15rs/ibge_cnv_pop.csv"


# ==============================================================================
# BLOCO A — DIRETÓRIO DE SAÍDA
# ==============================================================================

DIR_GRAFICOS <- "C:/SIVEPGRIPE/Graficos_boletim/SG"

if (!dir.exists(DIR_GRAFICOS)) dir.create(DIR_GRAFICOS, recursive = TRUE)
message("Diretório de gráficos: ", DIR_GRAFICOS)


# ==============================================================================
# BLOCO 1 — PACOTES
# ==============================================================================

pkgs <- c("sf", "foreign", "dplyr", "ggplot2", "scales", "tidyr", "readr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(sf); library(foreign); library(dplyr)
library(ggplot2); library(scales); library(tidyr); library(readr)


# ==============================================================================
# BLOCO 2 — IBGE
# ==============================================================================

ibge_raw <- readr::read_delim(
  ARQUIVO_IBGE, delim = ";", skip = 6,
  locale    = readr::locale(encoding = "latin1"),
  col_names = c("municipio_raw", "populacao"),
  col_types = readr::cols(municipio_raw = "c", populacao = "d"),
  show_col_types = FALSE
)

ibge_15rs <- ibge_raw %>%
  filter(!is.na(populacao),
         !grepl("^Total", municipio_raw, ignore.case = TRUE),
         !grepl("^\\s*$", municipio_raw)) %>%
  mutate(
    codigo_ibge    = as.integer(trimws(substr(municipio_raw, 1, 6))),
    nome_municipio = trimws(substr(municipio_raw, 8, nchar(municipio_raw))),
    populacao      = as.integer(populacao)
  ) %>%
  select(codigo_ibge, nome_municipio, populacao) %>%
  filter(!is.na(codigo_ibge))

POPULACAO_15RS_TOTAL <- sum(ibge_15rs$populacao)
message("Municípios IBGE carregados: ", nrow(ibge_15rs))


# ==============================================================================
# BLOCO 3 — FUNÇÕES AUXILIARES
# ==============================================================================

# --- Carregamento DBF (tenta SGHOSP####, fallback sghosp####) ---
carregar_base_sg <- function(ano, diretorio) {
  nomes <- c(
    paste0("SG", ano, ".dbf"),
    paste0("sg", ano, ".dbf"),
    paste0("SG", ano, ".DBF")
  )
  caminho <- NULL
  for (n in nomes) {
    c_ <- file.path(diretorio, n)
    if (file.exists(c_)) { caminho <- c_; break }
  }
  if (is.null(caminho)) {
    message("  [aviso] SG não encontrado para o ano ", ano); return(NULL)
  }
  base <- tryCatch({
    df <- foreign::read.dbf(caminho, as.is = TRUE)
    message("  [OK] foreign::read.dbf | ", caminho, " | ", nrow(df), " registros")
    df
  }, error = function(e) NULL)
  if (is.null(base)) {
    base <- tryCatch({
      df <- sf::st_read(caminho, stringsAsFactors = FALSE, quiet = TRUE)
      if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      message("  [OK-sf] ", caminho, " | ", nrow(df), " registros")
      df
    }, error = function(e) { message("  [ERRO] ", e$message); NULL })
  }
  if (!is.null(base)) base$ANO_BASE <- ano
  base
}

# --- Filtro 15ª RS por COD_MUNRES (código IBGE de residência) ---
aplicar_filtros_sg <- function(base, municipio = NULL) {
  r <- base %>%
    filter(as.integer(substr(as.character(COD_MUNRES), 1, 6)) %in% ibge_15rs$codigo_ibge)
  message("Registros SG da 15ª RS: ", nrow(r))
  if (nrow(r) == 0) {
    warning("Nenhum registro SG para a 15ª RS. Verifique COD_MUNRES.")
    return(r)
  }
  if (!is.null(municipio) && nzchar(trimws(municipio))) {
    mu  <- toupper(trimws(municipio))
    cod <- ibge_15rs %>% filter(toupper(nome_municipio) == mu) %>% pull(codigo_ibge)
    if (length(cod) > 0)
      r <- r %>% filter(as.integer(substr(as.character(COD_MUNRES), 1, 6)) == cod[1])
  }
  r
}

# --- Semana epidemiológica a partir de DT_PRISINT (data de início dos sintomas) ---
extrair_semana <- function(df) {
  df %>% mutate(
    DT_SINT_D = as.Date(DT_PRISINT, format = "%d/%m/%Y"),
    SE_SINT   = as.integer(format(DT_SINT_D, "%V")),
    ANO_SINT  = as.integer(format(DT_SINT_D, "%Y"))
  )
}

# --- Converte campo binário SIVEP (1=Sim, 2=Não, 9/NA=Ignorado) ---
sim_nao <- function(x) {
  dplyr::case_when(
    x == "1" | x == 1 ~ "Sim",
    x == "2" | x == 2 ~ "Não",
    TRUE               ~ "Ignorado"
  )
}

# --- Faixa etária (mesmo padrão COD_IDADE do script SRAG) ---
criar_faixa_etaria <- function(df) {
  df %>% mutate(faixa_etaria = case_when(
    is.na(COD_IDADE)  ~ "Em branco/Ignorado",
    COD_IDADE <= 2005 ~ "0-6 meses",
    COD_IDADE <= 2011 ~ "6-11 meses",
    COD_IDADE <= 3004 ~ "1-4 anos",
    COD_IDADE <= 3009 ~ "5-9 anos",
    COD_IDADE <= 3014 ~ "10-14 anos",
    COD_IDADE <= 3019 ~ "15-19 anos",
    COD_IDADE <= 3029 ~ "20-29 anos",
    COD_IDADE <= 3039 ~ "30-39 anos",
    COD_IDADE <= 3049 ~ "40-49 anos",
    COD_IDADE <= 3059 ~ "50-59 anos",
    COD_IDADE >= 3060 ~ "60 anos e mais",
    TRUE              ~ "Erro/Outro"
  ))
}

ORDEM_FAIXAS <- c(
  "0-6 meses", "6-11 meses", "1-4 anos", "5-9 anos",
  "10-14 anos", "15-19 anos", "20-29 anos", "30-39 anos",
  "40-49 anos", "50-59 anos", "60 anos e mais",
  "Em branco/Ignorado", "Erro/Outro"
)

gerar_rodape <- function() {
  paste0("Fonte: SIVEP-Gripe (SG sentinela) | Dados extraídos em: ",
         format(Sys.Date(), "%d/%m/%Y"))
}
texto_rodape <- gerar_rodape()

salvar_grafico <- function(grafico, nome_arquivo) {
  caminho <- file.path(DIR_GRAFICOS, paste0(nome_arquivo, ".png"))
  ggsave(filename = caminho, plot = grafico,
         width = 12, height = 7, units = "in", dpi = 300, bg = "white")
  message("Gráfico salvo: ", caminho)
}


# ==============================================================================
# BLOCO 4 — CARREGAMENTO DAS BASES
# ==============================================================================

anos_disponiveis <- 2019:2026

anos_carregar <- if (!is.null(ANO_ANALISE)) intersect(ANO_ANALISE, anos_disponiveis) else anos_disponiveis

anos_curva <- if (is.null(ANO_ANALISE)) anos_disponiveis else
  if (length(ANO_ANALISE) == 1) intersect(seq(ANO_ANALISE - 2, ANO_ANALISE), anos_disponiveis) else
    intersect(ANO_ANALISE, anos_disponiveis)

anos_a_carregar <- sort(union(anos_carregar, anos_curva))

message("\nAnos da análise principal : ", paste(anos_carregar, collapse = ", "))
message("Anos da curva comparativa : ", paste(anos_curva,    collapse = ", "))

lista_bases <- Filter(Negate(is.null),
                      lapply(anos_a_carregar, carregar_base_sg, diretorio = DIRETORIO_DBF))

if (length(lista_bases) == 0) {
  stop(
    "\n[ERRO] Nenhuma base SG carregada.",
    "\nVerifique se os arquivos SG", ANO_ANALISE, ".dbf existem em: ", DIRETORIO_DBF
  )
}

base_completa <- dplyr::bind_rows(lista_bases)
message("Total registros SG combinados: ", format(nrow(base_completa), big.mark = "."))


# ==============================================================================
# BLOCO 5 — FILTROS PRINCIPAIS
# ==============================================================================

base_ano_principal <- base_completa %>% filter(ANO_BASE %in% anos_carregar)
base_filtrada      <- aplicar_filtros_sg(base_ano_principal, municipio = MUNICIPIO_ANALISE)

if (nrow(base_filtrada) == 0) stop("Nenhum registro SG após filtros.")

# Base para curva comparativa (inclui anos de contexto)
base_curva_sg <- base_completa %>%
  filter(ANO_BASE %in% anos_curva) %>%
  aplicar_filtros_sg(municipio = MUNICIPIO_ANALISE)

escopo_titulo <- if (!is.null(MUNICIPIO_ANALISE) && nzchar(MUNICIPIO_ANALISE))
  paste0(tools::toTitleCase(tolower(MUNICIPIO_ANALISE)), " — 15ª RS Maringá") else
    "15ª Regional de Saúde de Maringá"

n_total <- nrow(base_filtrada)
message("\nEscopo   : ", escopo_titulo)
message("Registros: ", format(n_total, big.mark = "."))


# ==============================================================================
# BLOCO 6 — CURVA EPIDÊMICA COMPARATIVA (SG)
# ==============================================================================

casos_sg_semana <- base_curva_sg %>%
  extrair_semana() %>%
  filter(!is.na(DT_SINT_D)) %>%
  mutate(Ano = as.character(ANO_SINT)) %>%
  group_by(Ano, Semana_Epi = SE_SINT) %>%
  summarise(Total = n(), .groups = "drop")

if (nrow(casos_sg_semana) > 0) {
  
  anos_destaque <- as.character(anos_carregar)
  anos_contexto <- setdiff(unique(casos_sg_semana$Ano), anos_destaque)
  todos_anos    <- sort(unique(casos_sg_semana$Ano))
  
  paleta_base <- c("#E63946", "#F4A261", "#2A9D8F", "#457B9D",
                   "#6A0572", "#E9C46A", "#264653", "#A8DADC")
  cores_curva     <- setNames(paleta_base[seq_along(todos_anos)], todos_anos)
  espessuras_curva <- setNames(ifelse(todos_anos %in% anos_destaque, 2.2, 0.9), todos_anos)
  
  n_por_ano_sg <- casos_sg_semana %>%
    group_by(Ano) %>% summarise(n_ano = sum(Total), .groups = "drop")
  rotulos_legenda <- setNames(
    paste0(n_por_ano_sg$Ano, "  (N = ", format(n_por_ano_sg$n_ano, big.mark = "."), ")"),
    n_por_ano_sg$Ano
  )
  
  g06 <- ggplot(casos_sg_semana,
                aes(x = Semana_Epi, y = Total, group = Ano, color = Ano, linewidth = Ano)) +
    geom_line(alpha = 0.9) +
    geom_point(size = 1.2, alpha = 0.85) +
    scale_color_manual(values = cores_curva, labels = rotulos_legenda) +
    scale_linewidth_manual(values = espessuras_curva, guide = "none") +
    scale_x_continuous(breaks = seq(1, 52, by = 4), limits = c(1, 53)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste0("Curva Epidêmica Comparativa — Síndrome Gripal — ", escopo_titulo),
      subtitle = paste0("Ano(s) em análise: ", paste(anos_carregar, collapse = ", "),
                        " | Contexto: ", paste(anos_contexto, collapse = ", ")),
      x = "Semana Epidemiológica", y = "Atendimentos SG",
      color = "Ano", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "right")
  
  print(g06)
  salvar_grafico(g06, "SG06_curva_epidemica_comparativa")
}


# ==============================================================================
# BLOCO 7 — ATENDIMENTOS POR SEMANA EPIDEMIOLÓGICA
# ==============================================================================

sg_semana <- base_filtrada %>%
  extrair_semana() %>%
  filter(!is.na(SE_SINT)) %>%
  group_by(SE_SINT) %>%
  summarise(total = n(), .groups = "drop")

n_sg <- sum(sg_semana$total)

g07 <- ggplot(sg_semana, aes(x = factor(SE_SINT), y = total)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), vjust = -0.5, size = 3.2) +
  labs(
    title   = paste0("Síndrome Gripal por Semana Epidemiológica — ", escopo_titulo,
                     " (N = ", format(n_sg, big.mark = "."), ")"),
    x = "Semana Epidemiológica", y = "Atendimentos", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g07)
salvar_grafico(g07, "SG07_atendimentos_semana")


# ==============================================================================
# BLOCO 8 — PREVALÊNCIA DE SINTOMAS
# Nota: febre, tosse e dor de garganta são critérios de definição de caso SG,
# portanto sua frequência isolada tem interpretação limitada.
# O valor analítico maior está na comparação entre subgrupos (ver blocos 11 e 12).
# ==============================================================================

sintomas_cols <- c(FEBRE = "Febre", TOSSE = "Tosse", DOR_GARGAN = "Dor de Garganta")

sint_freq <- purrr::map_dfr(names(sintomas_cols), function(col) {
  if (!col %in% names(base_filtrada)) return(NULL)
  base_filtrada %>%
    mutate(valor = sim_nao(.data[[col]])) %>%
    filter(valor == "Sim") %>%
    summarise(n = n()) %>%
    mutate(
      Sintoma  = sintomas_cols[[col]],
      pct      = round(n / n_total * 100, 1)
    )
})

if (nrow(sint_freq) > 0) {
  g08 <- ggplot(sint_freq, aes(x = reorder(Sintoma, n), y = n)) +
    geom_col(fill = "#0057A3") +
    geom_text(aes(label = paste0(format(n, big.mark = "."), " (", pct, "%)")),
              hjust = -0.08, size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(sint_freq$n) * 1.25)) +
    labs(
      title   = paste0("Prevalência de Sintomas — ", escopo_titulo,
                       " (N = ", format(n_total, big.mark = "."), ")"),
      subtitle = "Febre, tosse e dor de garganta fazem parte da definição de caso SG — interpretar com cautela isoladamente",
      x = "Sintoma", y = "Casos com o Sintoma", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9, color = "gray40"))
  
  print(g08)
  salvar_grafico(g08, "SG08_prevalencia_sintomas")
}


# ==============================================================================
# BLOCO 9 — TENDÊNCIA SEMANAL DE SINTOMAS (proporção por semana)
# ==============================================================================

sint_semana_base <- base_filtrada %>%
  extrair_semana() %>%
  filter(!is.na(SE_SINT)) %>%
  mutate(
    febre      = as.integer(FEBRE      == "1" | FEBRE      == 1),
    tosse      = as.integer(TOSSE      == "1" | TOSSE      == 1),
    dor_gargan = as.integer(DOR_GARGAN == "1" | DOR_GARGAN == 1)
  )

sint_semana <- sint_semana_base %>%
  group_by(Semana = SE_SINT) %>%
  summarise(
    Total      = n(),
    Febre      = sum(febre,      na.rm = TRUE),
    Tosse      = sum(tosse,      na.rm = TRUE),
    `Dor de Garganta` = sum(dor_gargan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Febre (%)`           = round(Febre / Total * 100, 1),
    `Tosse (%)`           = round(Tosse / Total * 100, 1),
    `Dor de Garganta (%)` = round(`Dor de Garganta` / Total * 100, 1)
  ) %>%
  pivot_longer(cols = ends_with("(%)"), names_to = "Sintoma", values_to = "Proporcao")

n_sint_semana <- sum(sint_semana_base %>% group_by(SE_SINT) %>% summarise(n=n()) %>% pull(n))

g09 <- ggplot(sint_semana, aes(x = Semana, y = Proporcao, color = Sintoma, group = Sintoma)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1, 52, by = 2)) +
  scale_color_manual(values = c(
    "Febre (%)"           = "#E63946",
    "Tosse (%)"           = "#0057A3",
    "Dor de Garganta (%)" = "#2A9D8F"
  )) +
  labs(
    title    = paste0("Proporção Semanal de Sintomas — ", escopo_titulo,
                      " (N = ", format(n_total, big.mark = "."), ")"),
    subtitle = "Percentual de casos com cada sintoma por semana epidemiológica",
    x = "Semana Epidemiológica", y = "% de Casos com o Sintoma",
    color = "Sintoma", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(g09)
salvar_grafico(g09, "SG09_tendencia_semanal_sintomas")


# ==============================================================================
# BLOCO 10 — PREVALÊNCIA DE COMORBIDADES
# ==============================================================================

comorbidades_cols <- c(
  DIABETES    = "Diabetes",
  CARDIOVASC  = "Doença Cardiovascular",
  PNEUMOPAT   = "Pneumopatia",
  NEUROLOGIC  = "Doença Neurológica",
  RENAL       = "Doença Renal",
  HEPATICA    = "Doença Hepática",
  IMUNODEPRE  = "Imunodeficiência",
  OBESIDADE   = "Obesidade",
  HEMATOLOGI  = "Doença Hematológica",
  ASMA        = "Asma",
  TABAG       = "Tabagismo",
  SIND_DOWN   = "Síndrome de Down"
)

comor_freq <- purrr::map_dfr(names(comorbidades_cols), function(col) {
  if (!col %in% names(base_filtrada)) return(NULL)
  n_sim <- sum(base_filtrada[[col]] == "1" | base_filtrada[[col]] == 1, na.rm = TRUE)
  tibble::tibble(
    Comorbidade = comorbidades_cols[[col]],
    n           = n_sim,
    pct         = round(n_sim / n_total * 100, 1)
  )
}) %>% arrange(desc(n))

if (nrow(comor_freq) > 0) {
  g10 <- ggplot(comor_freq, aes(x = reorder(Comorbidade, n), y = n)) +
    geom_col(fill = "#6A0572") +
    geom_text(aes(label = paste0(format(n, big.mark = "."), " (", pct, "%)")),
              hjust = -0.08, size = 3.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(comor_freq$n) * 1.3)) +
    labs(
      title   = paste0("Comorbidades nos Casos de SG — ", escopo_titulo,
                       " (N = ", format(n_total, big.mark = "."), ")"),
      x = "Comorbidade", y = "Casos", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(g10)
  salvar_grafico(g10, "SG10_comorbidades")
}


# ==============================================================================
# BLOCO 11 — SINTOMAS POR VÍRUS IDENTIFICADO (RT-PCR)
# Principal gráfico analítico: compara perfil clínico entre agentes etiológicos.
# ==============================================================================

virus_map <- c(
  POS_PCRFLU = "Influenza",
  PCR_SARS2  = "Covid-19",
  PCR_VRS    = "VSR",
  PCR_RINO   = "Rinovírus",
  PCR_METAP  = "Metapneumovírus",
  PCR_ADENO  = "Adenovírus"
)

sint_map <- c(
  febre      = "Febre",
  tosse      = "Tosse",
  dor_gargan = "Dor de Garganta"
)

base_pcr <- base_filtrada %>%
  mutate(
    febre      = as.integer(FEBRE      == "1" | FEBRE      == 1),
    tosse      = as.integer(TOSSE      == "1" | TOSSE      == 1),
    dor_gargan = as.integer(DOR_GARGAN == "1" | DOR_GARGAN == 1)
  )

# Para cada vírus, calcula proporção de casos com cada sintoma
sint_por_virus <- purrr::map_dfr(names(virus_map), function(vcol) {
  if (!vcol %in% names(base_pcr)) return(NULL)
  sub <- base_pcr %>% filter(.data[[vcol]] == "1" | .data[[vcol]] == 1)
  if (nrow(sub) == 0) return(NULL)
  purrr::map_dfr(names(sint_map), function(scol) {
    tibble::tibble(
      Vírus   = virus_map[[vcol]],
      Sintoma = sint_map[[scol]],
      n_virus = nrow(sub),
      pct     = round(sum(sub[[scol]], na.rm = TRUE) / nrow(sub) * 100, 1)
    )
  })
}) %>% filter(!is.na(pct))

if (nrow(sint_por_virus) > 0) {
  n_virus_total <- sint_por_virus %>%
    distinct(Vírus, n_virus) %>%
    mutate(rotulo = paste0(Vírus, "\n(n=", n_virus, ")"))
  sint_por_virus <- sint_por_virus %>%
    left_join(n_virus_total %>% select(Vírus, rotulo), by = "Vírus")
  
  g11 <- ggplot(sint_por_virus, aes(x = rotulo, y = pct, fill = Sintoma)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_dodge(width = 0.75),
              vjust = -0.4, size = 3) +
    scale_fill_manual(values = c(
      "Febre"          = "#E63946",
      "Tosse"          = "#0057A3",
      "Dor de Garganta" = "#2A9D8F"
    )) +
    scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
    labs(
      title    = paste0("Perfil de Sintomas por Vírus Identificado (RT-PCR) — ", escopo_titulo),
      subtitle = "Proporção de casos com cada sintoma entre os positivos para cada vírus",
      x = "Vírus", y = "% com o Sintoma",
      fill = "Sintoma", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  
  print(g11)
  salvar_grafico(g11, "SG11_sintomas_por_virus")
}


# ==============================================================================
# BLOCO 12 — SINTOMAS POR FAIXA ETÁRIA
# ==============================================================================

sint_faixa <- base_filtrada %>%
  criar_faixa_etaria() %>%
  mutate(
    febre      = as.integer(FEBRE      == "1" | FEBRE      == 1),
    tosse      = as.integer(TOSSE      == "1" | TOSSE      == 1),
    dor_gargan = as.integer(DOR_GARGAN == "1" | DOR_GARGAN == 1),
    faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS)
  ) %>%
  filter(!faixa_etaria %in% c("Em branco/Ignorado", "Erro/Outro")) %>%
  group_by(faixa_etaria) %>%
  summarise(
    Total            = n(),
    `Febre (%)`      = round(sum(febre,      na.rm = TRUE) / n() * 100, 1),
    `Tosse (%)`      = round(sum(tosse,      na.rm = TRUE) / n() * 100, 1),
    `Dor Garganta (%)` = round(sum(dor_gargan, na.rm = TRUE) / n() * 100, 1),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = ends_with("(%)"), names_to = "Sintoma", values_to = "Proporcao")

if (nrow(sint_faixa) > 0) {
  g12 <- ggplot(sint_faixa,
                aes(x = faixa_etaria, y = Proporcao, color = Sintoma, group = Sintoma)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    scale_color_manual(values = c(
      "Febre (%)"        = "#E63946",
      "Tosse (%)"        = "#0057A3",
      "Dor Garganta (%)" = "#2A9D8F"
    )) +
    labs(
      title    = paste0("Proporção de Sintomas por Faixa Etária — ", escopo_titulo,
                        " (N = ", format(n_total, big.mark = "."), ")"),
      subtitle = "Percentual de casos com cada sintoma em cada faixa etária",
      x = "Faixa Etária", y = "% com o Sintoma",
      color = "Sintoma", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  print(g12)
  salvar_grafico(g12, "SG12_sintomas_por_faixa_etaria")
}


# ==============================================================================
# BLOCO 13 — POSITIVIDADE VIRAL POR SINTOMA
# Entre os que têm cada sintoma, qual a distribuição dos vírus identificados?
# ==============================================================================

posit_sint <- purrr::map_dfr(names(sint_map), function(scol) {
  sub <- base_filtrada %>%
    filter(.data[[scol]] == "1" | .data[[scol]] == 1)
  if (nrow(sub) == 0) return(NULL)
  purrr::map_dfr(names(virus_map), function(vcol) {
    if (!vcol %in% names(sub)) return(NULL)
    n_pos <- sum(sub[[vcol]] == "1" | sub[[vcol]] == 1, na.rm = TRUE)
    tibble::tibble(
      Sintoma     = sint_map[[scol]],
      Vírus       = virus_map[[vcol]],
      n_sintoma   = nrow(sub),
      n_positivos = n_pos,
      pct         = round(n_pos / nrow(sub) * 100, 1)
    )
  })
}) %>% filter(n_positivos > 0)

if (nrow(posit_sint) > 0) {
  g13 <- ggplot(posit_sint,
                aes(x = reorder(Vírus, pct), y = pct, fill = Sintoma)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_dodge(width = 0.75),
              hjust = -0.1, size = 3) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(posit_sint$pct) * 1.3),
                       labels = function(x) paste0(x, "%")) +
    scale_fill_manual(values = c(
      "Febre"           = "#E63946",
      "Tosse"           = "#0057A3",
      "Dor de Garganta" = "#2A9D8F"
    )) +
    labs(
      title    = paste0("Positividade Viral por Sintoma — ", escopo_titulo,
                        " (N = ", format(n_total, big.mark = "."), ")"),
      subtitle = "% de casos positivos para cada vírus entre os que apresentam cada sintoma",
      x = "Vírus", y = "% de Positivos",
      fill = "Sintoma com que se apresentou", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  print(g13)
  salvar_grafico(g13, "SG13_positividade_viral_por_sintoma")
}


# ==============================================================================
# BLOCO 14 — CO-OCORRÊNCIA DE SINTOMAS (HEATMAP)
# ==============================================================================

sint_cols_bin <- c("febre", "tosse", "dor_gargan")
sint_labels   <- c(febre = "Febre", tosse = "Tosse", dor_gargan = "Dor de Garganta")

base_sint_bin <- base_filtrada %>%
  mutate(
    febre      = as.integer(FEBRE      == "1" | FEBRE      == 1),
    tosse      = as.integer(TOSSE      == "1" | TOSSE      == 1),
    dor_gargan = as.integer(DOR_GARGAN == "1" | DOR_GARGAN == 1)
  )

coocorrencia <- expand.grid(S1 = sint_cols_bin, S2 = sint_cols_bin,
                            stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    n   = sum(base_sint_bin[[S1]] == 1 & base_sint_bin[[S2]] == 1, na.rm = TRUE),
    pct = round(n / n_total * 100, 1),
    L1  = sint_labels[S1],
    L2  = sint_labels[S2]
  ) %>%
  ungroup()

if (nrow(coocorrencia) > 0) {
  g14 <- ggplot(coocorrencia, aes(x = L1, y = L2, fill = pct)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = paste0(pct, "%\n(n=", format(n, big.mark = "."), ")")),
              size = 3.5, color = "white", fontface = "bold") +
    scale_fill_gradient(low = "#AED6F1", high = "#0057A3",
                        name = "% do Total") +
    labs(
      title   = paste0("Co-ocorrência de Sintomas — ", escopo_titulo,
                       " (N = ", format(n_total, big.mark = "."), ")"),
      subtitle = "Diagonal: frequência isolada de cada sintoma | Fora da diagonal: combinações",
      x = NULL, y = NULL, caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.text  = element_text(size = 11))
  
  print(g14)
  salvar_grafico(g14, "SG14_coocorrencia_sintomas")
}


# ==============================================================================
# BLOCO 15 — VACINAÇÃO PARA INFLUENZA E USO DE ANTIVIRAL
# ==============================================================================

vacinacao <- base_filtrada %>%
  mutate(
    Vacinado   = sim_nao(VACINA),
    Antiviral  = sim_nao(ANTIVIRAL)
  ) %>%
  filter(Vacinado != "Ignorado") %>%
  count(Vacinado, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1))

n_vac <- sum(vacinacao$n)

if (nrow(vacinacao) > 0) {
  g15a <- ggplot(vacinacao, aes(x = Vacinado, y = n, fill = Vacinado)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(format(n, big.mark = "."), " (", pct, "%)")),
              vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("Sim" = "#27AE60", "Não" = "#E63946")) +
    scale_y_continuous(limits = c(0, max(vacinacao$n) * 1.2)) +
    labs(
      title   = paste0("Vacinação para Influenza — ", escopo_titulo,
                       " (N informados = ", format(n_vac, big.mark = "."), ")"),
      x = "Vacinado contra Influenza", y = "Casos", caption = texto_rodape
    ) +
    theme_minimal() + theme(legend.position = "none")
  
  print(g15a)
  salvar_grafico(g15a, "SG15a_vacinacao_influenza")
}

antiviral <- base_filtrada %>%
  mutate(Antiviral = sim_nao(ANTIVIRAL)) %>%
  filter(Antiviral != "Ignorado") %>%
  count(Antiviral, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1))

n_antiv <- sum(antiviral$n)

if (nrow(antiviral) > 0) {
  g15b <- ggplot(antiviral, aes(x = Antiviral, y = n, fill = Antiviral)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(format(n, big.mark = "."), " (", pct, "%)")),
              vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("Sim" = "#F4A261", "Não" = "#457B9D")) +
    scale_y_continuous(limits = c(0, max(antiviral$n) * 1.2)) +
    labs(
      title   = paste0("Uso de Antiviral — ", escopo_titulo,
                       " (N informados = ", format(n_antiv, big.mark = "."), ")"),
      x = "Recebeu Antiviral", y = "Casos", caption = texto_rodape
    ) +
    theme_minimal() + theme(legend.position = "none")
  
  print(g15b)
  salvar_grafico(g15b, "SG15b_uso_antiviral")
}


# ==============================================================================
# BLOCO 16 — ANTIVIRAL POR VÍRUS IDENTIFICADO
# Monitora se o uso está concentrado nos confirmados de Influenza
# ==============================================================================

antiviral_virus <- base_filtrada %>%
  mutate(usou_antiviral = ANTIVIRAL == "1" | ANTIVIRAL == 1) %>%
  pivot_longer(cols = any_of(names(virus_map)),
               names_to = "campo_virus", values_to = "pos_virus") %>%
  filter(pos_virus == "1" | pos_virus == 1) %>%
  mutate(Vírus = dplyr::recode(campo_virus, !!!virus_map)) %>%
  group_by(Vírus) %>%
  summarise(
    Total_Pos     = n(),
    Com_Antiviral = sum(usou_antiviral, na.rm = TRUE),
    pct           = round(Com_Antiviral / Total_Pos * 100, 1),
    .groups = "drop"
  ) %>%
  filter(Total_Pos > 0) %>%
  arrange(desc(pct))

if (nrow(antiviral_virus) > 0) {
  g16 <- ggplot(antiviral_virus,
                aes(x = reorder(paste0(Vírus, "\n(n=", Total_Pos, ")"), pct),
                    y = pct)) +
    geom_col(fill = "#F4A261") +
    geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
    labs(
      title    = paste0("Uso de Antiviral entre Casos Positivos por Vírus — ", escopo_titulo),
      subtitle = "% dos positivos para cada vírus que receberam antiviral",
      x = "Vírus", y = "% com Antiviral", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(g16)
  salvar_grafico(g16, "SG16_antiviral_por_virus")
}


# ==============================================================================
# BLOCO 17 — CIRCULAÇÃO VIRAL (SG sentinela)
# ==============================================================================

viral_sg <- base_filtrada %>%
  select(any_of(names(virus_map))) %>%
  pivot_longer(everything(), names_to = "campo", values_to = "resultado") %>%
  filter(resultado == "1" | resultado == 1) %>%
  mutate(Vírus = dplyr::recode(campo, !!!virus_map)) %>%
  group_by(Vírus) %>%
  summarise(Positivos = n(), .groups = "drop") %>%
  mutate(pct = round(Positivos / sum(Positivos) * 100, 1)) %>%
  arrange(desc(Positivos))

n_viral_sg <- sum(viral_sg$Positivos)

if (nrow(viral_sg) > 0) {
  g17 <- ggplot(viral_sg, aes(x = reorder(Vírus, Positivos), y = Positivos)) +
    geom_col(fill = "#0057A3") +
    geom_text(aes(label = paste0(Positivos, " (", pct, "%)")),
              hjust = -0.08, size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(viral_sg$Positivos) * 1.25)) +
    labs(
      title   = paste0("Vírus Identificados por RT-PCR — SG Sentinela — ", escopo_titulo,
                       " (N = ", format(n_viral_sg, big.mark = "."), ")"),
      x = "Vírus", y = "Casos Positivos", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(g17)
  salvar_grafico(g17, "SG17_circulacao_viral")
}


# ==============================================================================
# BLOCO 18 — TENDÊNCIA VIRAL SEMANAL (SG)
# ==============================================================================

viral_semana_sg <- base_filtrada %>%
  extrair_semana() %>%
  filter(!is.na(SE_SINT)) %>%
  select(SE_SINT, any_of(names(virus_map))) %>%
  pivot_longer(cols = -SE_SINT, names_to = "campo", values_to = "resultado") %>%
  filter(resultado == "1" | resultado == 1) %>%
  mutate(Vírus = dplyr::recode(campo, !!!virus_map)) %>%
  group_by(Semana = SE_SINT, Vírus) %>%
  summarise(Positivos = n(), .groups = "drop")

n_viral_sem_sg <- sum(viral_semana_sg$Positivos)

if (nrow(viral_semana_sg) > 0) {
  g18 <- ggplot(viral_semana_sg,
                aes(x = Semana, y = Positivos, color = Vírus, group = Vírus)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = seq(1, 52, by = 2)) +
    labs(
      title    = paste0("Tendência Semanal de Vírus Respiratórios — SG Sentinela — ",
                        escopo_titulo,
                        " (N = ", format(n_viral_sem_sg, big.mark = "."), ")"),
      x = "Semana Epidemiológica", y = "Casos Positivos",
      color = "Vírus", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  print(g18)
  salvar_grafico(g18, "SG18_tendencia_viral_semanal")
}


# ==============================================================================
# BLOCO 19 — RESUMO FINAL (CONSOLE)
# ==============================================================================

n_febre    <- sum(base_filtrada$FEBRE      == "1" | base_filtrada$FEBRE      == 1, na.rm = TRUE)
n_tosse    <- sum(base_filtrada$TOSSE      == "1" | base_filtrada$TOSSE      == 1, na.rm = TRUE)
n_gargan   <- sum(base_filtrada$DOR_GARGAN == "1" | base_filtrada$DOR_GARGAN == 1, na.rm = TRUE)
n_vacinado <- sum(base_filtrada$VACINA     == "1" | base_filtrada$VACINA     == 1, na.rm = TRUE)
n_antivir  <- sum(base_filtrada$ANTIVIRAL  == "1" | base_filtrada$ANTIVIRAL  == 1, na.rm = TRUE)

message("\n", strrep("=", 60))
message("RESUMO — SÍNDROME GRIPAL (SG SENTINELA)")
message(strrep("=", 60))
message("Escopo              : ", escopo_titulo)
message("Ano(s) analisados   : ", paste(anos_carregar, collapse = ", "))
message("Curva comparativa   : ", paste(anos_curva,    collapse = ", "))
message("Total de registros  : ", format(n_total,    big.mark = "."))
message("Com febre           : ", format(n_febre,    big.mark = "."),
        " (", round(n_febre / n_total * 100, 1), "%)")
message("Com tosse           : ", format(n_tosse,    big.mark = "."),
        " (", round(n_tosse / n_total * 100, 1), "%)")
message("Com dor de garganta : ", format(n_gargan,   big.mark = "."),
        " (", round(n_gargan / n_total * 100, 1), "%)")
message("Vacinados influenza : ", format(n_vacinado, big.mark = "."),
        " (", round(n_vacinado / n_total * 100, 1), "%)")
message("Uso de antiviral    : ", format(n_antivir,  big.mark = "."),
        " (", round(n_antivir / n_total * 100, 1), "%)")
if (nrow(viral_sg) > 0) {
  message("Positivos RT-PCR    : ", format(n_viral_sg, big.mark = "."))
  message("Vírus predominante  : ", viral_sg$Vírus[1],
          " (n=", viral_sg$Positivos[1], ", ", viral_sg$pct[1], "%)")
}
message(strrep("=", 60))
message("Análise SG concluída. Gráficos salvos em: ", DIR_GRAFICOS)