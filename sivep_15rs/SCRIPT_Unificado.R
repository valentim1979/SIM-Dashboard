# ==============================================================================
# VIGILÂNCIA EPIDEMIOLÓGICA — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# Script unificado: Gráficos SRAG + Mapas por município e bairro
# Autor   : Valentim Sala Junior
# Saída   : pasta graficos/ do projeto GitHub Pages
# ==============================================================================


# ==============================================================================
# BLOCO 0 — CONFIGURAÇÃO GLOBAL (EDITE AQUI)
# ==============================================================================

# --- 0.1 Anos de análise ---
# Um único ano:   ANO_ANALISE <- 2025
# Múltiplos:      ANO_ANALISE <- c(2024, 2025)
# Todos:          ANO_ANALISE <- NULL
ANO_ANALISE <- 2026

# --- 0.2 Município ---
# NULL = toda a 15ª RS
# Ex.: MUNICIPIO_ANALISE <- "MARINGA"
MUNICIPIO_ANALISE <- NULL

# --- 0.3 Caminhos ---
DIRETORIO_DBF <- "/Users/valentimsalajunior/Documents/DBF_SIVEP"
ARQUIVO_IBGE  <- "/Users/valentimsalajunior/Documents/Documentos_2026/sivep_15rs/ibge_cnv_pop.csv"

CAMINHO_SHP_MUNICIPIOS <- "/Users/valentimsalajunior/Documents/PR_Municipios_2024/PR_Municipios_2024.shp"
CAMINHO_SHP_MARINGA    <- "/Users/valentimsalajunior/Documents/Bairros.shp"
CAMINHO_SHP_SARANDI    <- "/Users/valentimsalajunior/Documents/bairros_sarandi/Bairros_loteamentos.shp"

# --- 0.4 Parâmetros dos mapas de bairro ---
CORTE_NOME_BAIRRO     <- 5   # mínimo de casos para exibir nome no mapa — Maringá
CORTE_NOME_BAIRRO_SAR <- 5   # idem para Sarandi

# --- 0.5 Pasta de saída (pasta graficos/ do projeto GitHub Pages) ---
# Ajuste para o caminho completo do seu projeto, ex.:
DIR_GRAFICOS <- "/Users/valentimsalajunior/Documents/Documentos_2026/sivep_15rs/graficos"

# --- 0.6 Data de extração dos dados do SIVEP-Gripe ---
# Atualize manualmente a cada nova extração
DATA_EXTRACAO <- as.Date("2026-04-05")

# ==============================================================================
# BLOCO 1 — PACOTES
# ==============================================================================

pacotes <- c(
  "sf", "foreign", "dplyr", "ggplot2", "scales", "tidyr",
  "readr", "stringr", "lubridate", "forcats", "tmap", "writexl"
)

for (pkg in pacotes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# ==============================================================================
# BLOCO 2 — MUNICÍPIOS E POPULAÇÕES DA 15ª RS
# ==============================================================================

municipios_15rs <- tibble::tribble(
  ~codigo_ibge_6, ~municipio,                     ~populacao_2025,
  410115,         "ANGULO",                        3357,
  410210,         "ASTORGA",                       26203,
  410220,         "ATALAIA",                       4046,
  410590,         "COLORADO",                      23313,
  410730,         "DOUTOR CAMARGO",                6517,
  410780,         "FLORAI",                        4805,
  410790,         "FLORESTA",                      11522,
  410810,         "FLORIDA",                       2711,
  411000,         "IGUARACU",                      5693,
  411090,         "ITAGUAJE",                      4530,
  411110,         "ITAMBE",                        6228,
  411160,         "IVATUBA",                       2685,
  411360,         "LOBATO",                        4707,
  411410,         "MANDAGUACU",                    34521,
  411420,         "MANDAGUARI",                    38313,
  411480,         "MARIALVA",                      44749,
  411520,         "MARINGA",                       429660,
  411630,         "MUNHOZ DE MELO",                4057,
  411640,         "NOSSA SENHORA DAS GRACAS",      3669,
  411690,         "NOVA ESPERANCA",                27142,
  411740,         "OURIZONA",                      3193,
  411750,         "PAICANDU",                      48695,
  411810,         "PARANACITY",                    9549,
  412040,         "PRESIDENTE CASTELO BRANCO",     4304,
  412340,         "SANTA FE",                      11730,
  412360,         "SANTA INES",                    1745,
  412450,         "SANTO INACIO",                  6463,
  412530,         "SAO JORGE DO IVAI",             5170,
  412625,         "SARANDI",                       128106,
  412830,         "UNIFLOR",                       2106
)

POPULACAO_15RS_TOTAL <- sum(municipios_15rs$populacao_2025)

message("Municípios: ", nrow(municipios_15rs),
        " | Pop. total: ", format(POPULACAO_15RS_TOTAL, big.mark = "."))


# ==============================================================================
# BLOCO 3 — FUNÇÕES AUXILIARES
# ==============================================================================

# Cria pasta de saída se não existir
if (!dir.exists(DIR_GRAFICOS)) dir.create(DIR_GRAFICOS, recursive = TRUE)
message("Saída: ", DIR_GRAFICOS)

texto_rodape <- paste0(
  "Fonte: SIVEP-Gripe | Dados: ", format(DATA_EXTRACAO, "%d/%m/%Y"),
  " | Atualizado: ", format(Sys.Date(), "%d/%m/%Y")
)

salvar_grafico <- function(grafico, nome_arquivo, width = 12, height = 7) {
  caminho <- file.path(DIR_GRAFICOS, paste0(nome_arquivo, ".png"))
  ggsave(
    filename = caminho, plot = grafico,
    width = width, height = height, units = "in", dpi = 150, bg = "white"
  )
  message("Salvo: ", caminho)
}

carregar_base <- function(ano, diretorio) {
  nomes <- c(
    paste0("SRAGHOSP", ano, ".dbf"),
    paste0("sraghosp", ano, ".dbf"),
    paste0("SRAGHOSP", ano, ".DBF")
  )
  caminho <- NULL
  for (n in nomes) {
    c <- file.path(diretorio, n)
    if (file.exists(c)) { caminho <- c; break }
  }
  if (is.null(caminho)) {
    message("  [aviso] Não encontrado para o ano ", ano); return(NULL)
  }
  df <- tryCatch(
    { d <- foreign::read.dbf(caminho, as.is = TRUE)
    message("  [OK] ", basename(caminho), " | ", nrow(d), " registros"); d },
    error = function(e) NULL
  )
  if (!is.null(df)) df$ANO_BASE <- ano
  df
}

parseia_data <- function(x) {
  if (inherits(x, "Date")) return(x)
  d <- as.Date(as.character(x), format = "%d/%m/%Y")
  if (sum(!is.na(d)) > 0) return(d)
  as.Date(as.character(x), format = "%Y-%m-%d")
}

criar_faixa_etaria <- function(df) {
  df %>% mutate(
    faixa_etaria = case_when(
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
    )
  )
}

padronizar_sexo <- function(df) {
  df %>% mutate(
    sexo = case_when(
      CS_SEXO %in% c("M", "m", "1", 1) ~ "Masculino",
      CS_SEXO %in% c("F", "f", "2", 2) ~ "Feminino",
      TRUE                              ~ "Ignorado"
    )
  )
}

ORDEM_FAIXAS <- c(
  "0-6 meses", "6-11 meses", "1-4 anos", "5-9 anos",
  "10-14 anos", "15-19 anos", "20-29 anos", "30-39 anos",
  "40-49 anos", "50-59 anos", "60 anos e mais",
  "Em branco/Ignorado", "Erro/Outro"
)

normaliza_bairro <- function(x) {
  x %>%
    str_to_upper() %>% str_trim() %>% str_squish() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^A-Z0-9 ]", "") %>%
    str_squish()
}


# ==============================================================================
# BLOCO 4 — CARREGAMENTO E LIMPEZA DOS DADOS
# ==============================================================================

anos_disponiveis <- 2019:2026

anos_carregar <- if (!is.null(ANO_ANALISE)) intersect(ANO_ANALISE, anos_disponiveis) else anos_disponiveis

anos_curva <- if (is.null(ANO_ANALISE)) {
  anos_disponiveis
} else if (length(ANO_ANALISE) == 1) {
  intersect(seq(ANO_ANALISE - 2, ANO_ANALISE), anos_disponiveis)
} else {
  intersect(ANO_ANALISE, anos_disponiveis)
}

anos_a_carregar <- sort(union(anos_carregar, anos_curva))

message("\nCarregando anos: ", paste(anos_a_carregar, collapse = ", "))

lista_bases <- Filter(Negate(is.null),
                      lapply(anos_a_carregar, carregar_base, diretorio = DIRETORIO_DBF))

if (length(lista_bases) == 0) stop("Nenhuma base carregada. Verifique DIRETORIO_DBF.")

base_completa <- bind_rows(lista_bases)
names(base_completa) <- toupper(names(base_completa))

message("Total de registros: ", format(nrow(base_completa), big.mark = "."))

# --- Limpeza e padronização ---
base_completa <- base_completa %>%
  mutate(
    CO_MUN_RES     = as.integer(CO_MUN_RES),
    DT_NOTIFIC_DT  = parseia_data(DT_NOTIFIC),
    ANO            = lubridate::year(DT_NOTIFIC_DT),
    SEM_EPI        = as.integer(SEM_NOT),
    BAIRRO         = NM_BAIRRO %>%
      str_to_upper() %>% str_trim() %>% str_squish() %>%
      str_replace_all("[^A-ZÁÉÍÓÚÂÊÎÔÛÃÕÀÈÌÒÙÇ0-9 ]", "") %>%
      na_if("") %>% na_if("NAO INFORMADO") %>%
      na_if("IGNORADO") %>% na_if("SEM INFORMACAO") %>% na_if("SEM BAIRRO"),
    CLASSIFICACAO  = case_when(
      CLASSI_FIN == 1 ~ "Influenza",
      CLASSI_FIN == 2 ~ "Outro vírus respiratório",
      CLASSI_FIN == 3 ~ "Outro agente etiológico",
      CLASSI_FIN == 4 ~ "SRAG não especificada",
      CLASSI_FIN == 5 ~ "COVID-19",
      TRUE            ~ "Não classificado"
    ),
    OBITO_SRAG   = EVOLUCAO == 2,
    OBITO_OUTRAS = EVOLUCAO == 3,
    UTI_SIM      = UTI == 1
  )

# --- Filtro geográfico ---
# Usa CO_MUN_RES para garantir só municípios da 15ª RS
base_15rs_completa <- base_completa %>%
  filter(CO_MUN_RES %in% municipios_15rs$codigo_ibge_6)

# Base do ano principal para os gráficos 06–21
base_ano_principal <- base_15rs_completa %>% filter(ANO_BASE %in% anos_carregar)

# Filtro por município (se configurado)
if (!is.null(MUNICIPIO_ANALISE) && nzchar(trimws(MUNICIPIO_ANALISE))) {
  cod_mun <- municipios_15rs %>%
    filter(toupper(municipio) == toupper(trimws(MUNICIPIO_ANALISE))) %>%
    pull(codigo_ibge_6)
  base_filtrada <- base_ano_principal %>% filter(CO_MUN_RES == cod_mun)
  POPULACAO_ESCOPO <- municipios_15rs %>%
    filter(codigo_ibge_6 == cod_mun) %>% pull(populacao_2025)
  escopo_titulo <- paste0(tools::toTitleCase(tolower(MUNICIPIO_ANALISE)),
                          " — 15ª RS Maringá")
} else {
  base_filtrada    <- base_ano_principal
  POPULACAO_ESCOPO <- POPULACAO_15RS_TOTAL
  escopo_titulo    <- "15ª Regional de Saúde de Maringá"
}

anos_contexto <- setdiff(as.character(anos_curva), as.character(anos_carregar))

message("Escopo    : ", escopo_titulo)
message("Registros : ", format(nrow(base_filtrada), big.mark = "."))
message("Pop. IBGE : ", format(POPULACAO_ESCOPO, big.mark = "."))


# ==============================================================================
# BLOCO 5 — AGREGAÇÕES PARA MAPAS
# ==============================================================================

# Por município
casos_municipio <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_carregar) %>%
  group_by(CO_MUN_RES) %>%
  summarise(
    casos         = n(),
    obitos_srag   = sum(OBITO_SRAG,   na.rm = TRUE),
    obitos_outras = sum(OBITO_OUTRAS, na.rm = TRUE),
    uti           = sum(UTI_SIM,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(municipios_15rs, by = c("CO_MUN_RES" = "codigo_ibge_6")) %>%
  mutate(
    incidencia_100k  = round(casos       / populacao_2025 * 100000, 1),
    mortalidade_100k = round(obitos_srag / populacao_2025 * 100000, 1),
    letalidade_pct   = round(obitos_srag / casos          * 100,    1)
  ) %>%
  arrange(desc(incidencia_100k))

# Por bairro — Maringá
cod_maringa <- 411520

casos_bairro <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_carregar, CO_MUN_RES == cod_maringa, !is.na(BAIRRO)) %>%
  group_by(BAIRRO) %>%
  summarise(
    casos       = n(),
    obitos_srag = sum(OBITO_SRAG, na.rm = TRUE),
    uti         = sum(UTI_SIM,    na.rm = TRUE),
    letalidade  = round(obitos_srag / casos * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(casos))

casos_bairro_sem <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_carregar, CO_MUN_RES == cod_maringa,
         !is.na(BAIRRO), !is.na(SEM_EPI)) %>%
  group_by(BAIRRO, SEM_EPI) %>%
  summarise(casos = n(), .groups = "drop")

# Por bairro — Sarandi
cod_sarandi <- 412625

casos_bairro_sar <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_carregar, CO_MUN_RES == cod_sarandi, !is.na(BAIRRO)) %>%
  group_by(BAIRRO) %>%
  summarise(
    casos       = n(),
    obitos_srag = sum(OBITO_SRAG, na.rm = TRUE),
    uti         = sum(UTI_SIM,    na.rm = TRUE),
    letalidade  = round(obitos_srag / casos * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(casos))

casos_bairro_sem_sar <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_carregar, CO_MUN_RES == cod_sarandi,
         !is.na(BAIRRO), !is.na(SEM_EPI)) %>%
  group_by(BAIRRO, SEM_EPI) %>%
  summarise(casos = n(), .groups = "drop")

casos_semana_class <- base_filtrada %>%
  filter(!is.na(SEM_EPI)) %>%
  group_by(SEM_EPI, CLASSIFICACAO) %>%
  summarise(casos = n(), .groups = "drop") %>%
  arrange(SEM_EPI)


# ==============================================================================
# GRÁFICO 06 — CURVA EPIDÊMICA COMPARATIVA
# ==============================================================================

base_curva <- base_15rs_completa %>%
  filter(ANO_BASE %in% anos_curva) %>%
  { if (!is.null(MUNICIPIO_ANALISE)) filter(., CO_MUN_RES == cod_mun) else . }

casos_semana_ano <- base_curva %>%
  mutate(
    Ano       = as.character(ANO_BASE),
    Semana    = as.integer(SEM_NOT)
  ) %>%
  filter(!is.na(Semana)) %>%
  group_by(Ano, Semana) %>%
  summarise(Total = n(), .groups = "drop")

if (nrow(casos_semana_ano) > 0) {
  todos_anos    <- sort(unique(casos_semana_ano$Ano))
  anos_destaque <- as.character(anos_carregar)
  paleta        <- c("#E63946","#F4A261","#2A9D8F","#457B9D","#6A0572","#E9C46A","#264653","#A8DADC")
  cores_curva   <- setNames(paleta[seq_along(todos_anos)], todos_anos)
  espessuras    <- setNames(ifelse(todos_anos %in% anos_destaque, 2.2, 0.9), todos_anos)
  
  n_por_ano     <- casos_semana_ano %>% group_by(Ano) %>% summarise(n = sum(Total), .groups = "drop")
  rotulos       <- setNames(paste0(n_por_ano$Ano, "  (N = ", format(n_por_ano$n, big.mark = "."), ")"),
                            n_por_ano$Ano)
  
  g06 <- ggplot(casos_semana_ano,
                aes(x = Semana, y = Total, group = Ano, color = Ano, linewidth = Ano)) +
    geom_line(alpha = 0.9) +
    geom_point(size = 1.2, alpha = 0.85) +
    scale_color_manual(values = cores_curva, labels = rotulos) +
    scale_linewidth_manual(values = espessuras, guide = "none") +
    scale_x_continuous(breaks = seq(1, 52, by = 4), limits = c(1, 53)) +
    labs(
      title    = paste0("Curva Epidêmica Comparativa — ", escopo_titulo),
      subtitle = paste0("Ano(s) em análise: ", paste(anos_carregar, collapse = ", "),
                        " | Contexto histórico: ", paste(anos_contexto, collapse = ", ")),
      x = "Semana Epidemiológica", y = "Casos Notificados",
      color = "Ano", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "right")
  
  salvar_grafico(g06, "06_curva_epidemica_comparativa")
}


# ==============================================================================
# GRÁFICO 07 — NOTIFICAÇÕES POR SEMANA EPIDEMIOLÓGICA
# ==============================================================================

casos_semana <- base_filtrada %>%
  group_by(SEM_NOT) %>%
  summarise(total = n(), .groups = "drop")

n_semana   <- sum(casos_semana$total)
incid_100k <- round(n_semana / POPULACAO_ESCOPO * 100000, 1)

g07 <- ggplot(casos_semana, aes(x = factor(SEM_NOT), y = total)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), vjust = -0.5, size = 3.2) +
  labs(
    title    = paste0("SRAG por Semana Epidemiológica — ", escopo_titulo,
                      " (N = ", format(n_semana, big.mark = "."), ")"),
    subtitle = paste0("N = ", format(n_semana, big.mark = "."),
                      " | Taxa: ", incid_100k, " por 100.000 hab.",
                      " | Pop. IBGE 2025: ", format(POPULACAO_ESCOPO, big.mark = ".")),
    x = "Semana Epidemiológica", y = "Notificações", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salvar_grafico(g07, "07_notificacoes_semana_epi")


# ==============================================================================
# GRÁFICO 08 — CONFIRMADOS POR SEMANA EPIDEMIOLÓGICA
# ==============================================================================

confirmados_semana <- base_filtrada %>%
  filter(CLASSI_FIN %in% c(1, 2, 3, 5)) %>%
  group_by(SEM_NOT) %>%
  summarise(total = n(), .groups = "drop")

n_conf <- sum(confirmados_semana$total)

g08 <- ggplot(confirmados_semana, aes(x = factor(SEM_NOT), y = total)) +
  geom_col(fill = "#A30000") +
  geom_text(aes(label = total), vjust = -0.5, size = 3.2) +
  labs(
    title    = paste0("SRAG Confirmado por Semana Epidemiológica — ", escopo_titulo,
                      " (N = ", format(n_conf, big.mark = "."), ")"),
    subtitle = paste0("N = ", format(n_conf, big.mark = ".")),
    x = "Semana Epidemiológica", y = "Casos Confirmados", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salvar_grafico(g08, "08_confirmados_semana_epi")


# ==============================================================================
# GRÁFICO 09 — INCIDÊNCIA POR MUNICÍPIO
# ==============================================================================

g09 <- casos_municipio %>%
  mutate(
    municipio = fct_reorder(str_to_title(municipio), incidencia_100k),
    rotulo    = paste0(incidencia_100k, " /100k  (n=", casos, ")")
  ) %>%
  ggplot(aes(x = incidencia_100k, y = municipio)) +
  geom_col(fill = "#1A5C38") +
  geom_text(aes(label = rotulo), hjust = -0.05, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = paste0("Taxa de Incidência de SRAG por Município — 15ª RS Maringá",
                      " (N = ", format(sum(casos_municipio$casos), big.mark = "."), ")"),
    subtitle = paste0("Por 100.000 habitantes | Pop. IBGE 2025",
                      " | Pop. total: ", format(POPULACAO_15RS_TOTAL, big.mark = "."),
                      " | Ano(s): ", paste(anos_carregar, collapse = ", ")),
    x = "Incidência por 100.000 hab.", y = "Município", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g09, "09_incidencia_por_municipio", height = 10)


# ==============================================================================
# GRÁFICO 10 — NOTIFICAÇÕES POR REGIONAL DE SAÚDE (PARANÁ)
# ==============================================================================

# ==============================================================================
# GRÁFICO 10 — NOTIFICAÇÕES POR REGIONAL DE SAÚDE (PARANÁ)
# ==============================================================================

base_pr <- bind_rows(lista_bases) %>%
  mutate(ID_REGIONA = toupper(trimws(ID_REGIONA))) %>%
  filter(!is.na(ID_REGIONA), ANO_BASE %in% anos_carregar) %>%
  mutate(
    num_regional = as.integer(str_extract(ID_REGIONA, "^[0-9]+"))
  ) %>%
  filter(!is.na(num_regional), num_regional >= 1, num_regional <= 22)

if ("ID_REGIONA" %in% names(base_pr) && nrow(base_pr) > 0) {
  casos_regional <- base_pr %>%
    group_by(ID_REGIONA) %>%
    summarise(total = n(), .groups = "drop") %>%
    arrange(desc(total))
  
  n_pr <- sum(casos_regional$total)
  
  g10 <- ggplot(casos_regional,
                aes(x = total, y = fct_reorder(ID_REGIONA, total))) +
    geom_col(fill = "#0057A3") +
    geom_text(aes(label = format(total, big.mark = ".")), hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = paste0("Notificações de SRAG por Regional de Saúde — Paraná",
                        " (N = ", format(n_pr, big.mark = "."), ")"),
      subtitle = paste(anos_carregar, collapse = ", "),
      x = "Total de Notificações", y = "Regional de Saúde", caption = texto_rodape
    ) +
    theme_minimal()
  
  salvar_grafico(g10, "10_notificacoes_regionais_pr", height = 10)
}

# ==============================================================================
# GRÁFICO 11 — DISTRIBUIÇÃO POR SEXO
# ==============================================================================

casos_sexo <- base_filtrada %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado") %>%
  group_by(sexo) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(pct = round(total / sum(total) * 100, 1))

n_sexo <- sum(casos_sexo$total)

g11 <- ggplot(casos_sexo, aes(x = total, y = sexo, fill = sexo)) +
  geom_col() +
  geom_text(aes(label = paste0(total, " (", pct, "%)")), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("Masculino" = "#0057A3", "Feminino" = "#E91E8C")) +
  labs(
    title    = paste0("Distribuição por Sexo — ", escopo_titulo,
                      " (N = ", format(n_sexo, big.mark = "."), ")"),
    x = "Notificações", y = NULL, fill = "Sexo", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

salvar_grafico(g11, "11_distribuicao_sexo", height = 4)


# ==============================================================================
# GRÁFICO 12 — CLASSIFICAÇÃO FINAL
# ==============================================================================

casos_class <- base_filtrada %>%
  mutate(
    class_label = case_when(
      CLASSI_FIN == 1 ~ "SRAG por Influenza",
      CLASSI_FIN == 2 ~ "SRAG por Outro Vírus Respiratório",
      CLASSI_FIN == 3 ~ "SRAG por Outro Agente Etiológico",
      CLASSI_FIN == 4 ~ "SRAG Não Especificado",
      CLASSI_FIN == 5 ~ "SRAG por Covid-19",
      TRUE            ~ "Em Investigação"
    )
  ) %>%
  group_by(class_label) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))

n_class <- sum(casos_class$total)

g12 <- ggplot(casos_class,
              aes(x = total, y = fct_reorder(class_label, total))) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste0("Classificação Final — ", escopo_titulo,
                      " (N = ", format(n_class, big.mark = "."), ")"),
    x = "Total de Notificações", y = "Classificação", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g12, "12_classificacao_final")


# ==============================================================================
# GRÁFICO 13 — CIRCULAÇÃO VIRAL TOTAL (RT-PCR)
# ==============================================================================

# Converte o formato wide (uma coluna por vírus) para long
viral_wide <- base_filtrada %>%
  filter(PCR_RESUL == 1 | POS_PCRFLU == 1 | POS_PCROUT == 1) %>%
  mutate(
    Influenza      = POS_PCRFLU == 1,
    VSR            = PCR_VSR    == 1,
    Rinovírus      = PCR_RINO   == 1,
    Adenovírus     = PCR_ADENO  == 1,
    Metapneumovírus = PCR_METAP == 1,
    `Parainfluenza 1` = PCR_PARA1 == 1,
    `Parainfluenza 2` = PCR_PARA2 == 1,
    `Parainfluenza 3` = PCR_PARA3 == 1,
    `Parainfluenza 4` = PCR_PARA4 == 1,
    `Covid-19`     = PCR_SARS2  == 1
  )

circulacao_viral <- viral_wide %>%
  summarise(across(Influenza:`Covid-19`, ~ sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(everything(), names_to = "virus", values_to = "total") %>%
  filter(total > 0) %>%
  arrange(desc(total))

n_viral <- sum(circulacao_viral$total)

g13 <- ggplot(circulacao_viral,
              aes(x = total, y = fct_reorder(virus, total))) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste0("Vírus Identificados por RT-PCR — ", escopo_titulo,
                      " (N = ", format(n_viral, big.mark = "."), ")"),
    x = "Casos Positivos", y = "Vírus", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g13, "13_circulacao_viral_total")


# ==============================================================================
# GRÁFICO 14 — TENDÊNCIA SEMANAL DE VÍRUS RESPIRATÓRIOS
# ==============================================================================

virus_semanal <- base_filtrada %>%
  filter(!is.na(SEM_NOT)) %>%
  mutate(
    Influenza        = POS_PCRFLU == 1,
    VSR              = PCR_VSR    == 1,
    Rinovírus        = PCR_RINO   == 1,
    Adenovírus       = PCR_ADENO  == 1,
    Metapneumovírus  = PCR_METAP  == 1,
    `Covid-19`       = PCR_SARS2  == 1
  ) %>%
  tidyr::pivot_longer(
    cols      = c(Influenza, VSR, Rinovírus, Adenovírus, Metapneumovírus, `Covid-19`),
    names_to  = "virus",
    values_to = "positivo"
  ) %>%
  filter(positivo == TRUE) %>%
  group_by(SEM_NOT, virus) %>%
  summarise(total = n(), .groups = "drop")

if (nrow(virus_semanal) > 0) {
  n_semanal <- nrow(base_filtrada %>% filter(POS_PCRFLU == 1 | POS_PCROUT == 1))
  
  g14 <- ggplot(virus_semanal,
                aes(x = as.integer(SEM_NOT), y = total,
                    color = virus, group = virus)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_x_continuous(breaks = seq(1, 53, by = 4)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = paste0("Tendência Semanal de Vírus Respiratórios — ", escopo_titulo,
                        " (N = ", format(n_semanal, big.mark = "."), ")"),
      subtitle = "Influenza, Covid-19, VSR, Rinovírus, Adenovírus, Metapneumovírus",
      x = "Semana Epidemiológica", y = "Casos Positivos",
      color = "Vírus", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  salvar_grafico(g14, "14_tendencia_viral_semanal")
}


# ==============================================================================
# GRÁFICO 15 — TIPOS E LINHAGENS DE INFLUENZA
# ==============================================================================
# TP_FLU_PCR: 1 = Influenza A, 2 = Influenza B
# PCR_FLUASU: subtipo A (1 = H1N1pdm09, 2 = H3N2, 3 = não subtipado, 4 = não subtipável)
# PCR_FLUBLI: linhagem B (1 = Vitória, 2 = Yamagata, 3 = não realizado)

influenza_tipos <- base_filtrada %>%
  filter(POS_PCRFLU == 1) %>%
  mutate(
    tipo_label = case_when(
      TP_FLU_PCR == 1 & PCR_FLUASU == 1 ~ "Influenza A(H1N1)pdm09",
      TP_FLU_PCR == 1 & PCR_FLUASU == 2 ~ "Influenza A(H3N2)",
      TP_FLU_PCR == 1 & PCR_FLUASU == 3 ~ "Influenza A não subtipado",
      TP_FLU_PCR == 1 & PCR_FLUASU == 4 ~ "Influenza A não subtipável",
      TP_FLU_PCR == 1                    ~ "Influenza A não subtipado",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 1 ~ "Influenza B – Vitória",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 2 ~ "Influenza B – Yamagata",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 3 ~ "Influenza B – Não realizado",
      TP_FLU_PCR == 2                    ~ "Influenza B – Não classificado",
      TRUE                               ~ "Ignorado / Não classificado"
    )
  ) %>%
  group_by(tipo_label) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))

n_inf <- sum(influenza_tipos$total)

if (nrow(influenza_tipos) > 0) {
  g15 <- ggplot(influenza_tipos,
                aes(x = total, y = fct_reorder(tipo_label, total))) +
    geom_col(fill = "#0057A3") +
    geom_text(aes(label = total), hjust = -0.1, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = paste0("Tipos e Linhagens de Influenza (RT-PCR) — ", escopo_titulo,
                        " (N = ", format(n_inf, big.mark = "."), ")"),
      x = "Total de Casos", y = "Classificação", caption = texto_rodape
    ) +
    theme_minimal()
  
  salvar_grafico(g15, "15_tipos_linhagens_influenza")
}

# ==============================================================================
# GRÁFICO 16 — FAIXA ETÁRIA: NOTIFICADOS vs CONFIRMADOS
# ==============================================================================

notif_faixa <- base_filtrada %>%
  criar_faixa_etaria() %>%
  group_by(faixa_etaria) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(status = "Notificados")

conf_faixa <- base_filtrada %>%
  filter(CLASSI_FIN %in% c(1, 2, 3, 5)) %>%
  criar_faixa_etaria() %>%
  group_by(faixa_etaria) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(status = "Confirmados")

faixa_combinada <- bind_rows(notif_faixa, conf_faixa) %>%
  filter(faixa_etaria %in% ORDEM_FAIXAS) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS))

n_notif_fe <- sum(notif_faixa$n)
n_conf_fe  <- sum(conf_faixa$n)

g16 <- ggplot(faixa_combinada,
              aes(x = n, y = faixa_etaria, fill = status)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9),
            hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Notificados" = "#0057A3", "Confirmados" = "#1A5C38")) +
  labs(
    title    = paste0("Faixa Etária: Notificados vs Confirmados — ", escopo_titulo,
                      " (Notif.: ", format(n_notif_fe, big.mark = "."),
                      " | Conf.: ", format(n_conf_fe,  big.mark = "."), ")"),
    x = "Quantidade", y = "Faixa Etária", fill = "Status", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

salvar_grafico(g16, "16_faixa_etaria_notif_confirmados", height = 8)


# ==============================================================================
# GRÁFICO 17 — PIRÂMIDE ETÁRIA (NOTIFICADOS)
# ==============================================================================

piramide_notif <- base_filtrada %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado", faixa_etaria %in% ORDEM_FAIXAS) %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS),
    value        = ifelse(sexo == "Masculino", -n, n)
  )

n_piramide_notif <- sum(piramide_notif$n)

g17 <- ggplot(piramide_notif, aes(x = faixa_etaria, y = value, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = n, hjust = ifelse(sexo == "Masculino", 1.15, -0.15)), size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c("Masculino" = "#0057A3", "Feminino" = "#E91E8C")) +
  labs(
    title   = paste0("Pirâmide Etária — Notificados — ", escopo_titulo,
                     " (N = ", format(n_piramide_notif, big.mark = "."), ")"),
    x = "Faixa Etária", y = "Número de Casos", fill = "Sexo", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

salvar_grafico(g17, "17_piramide_etaria_notificados", height = 8)


# ==============================================================================
# GRÁFICO 18 — PIRÂMIDE ETÁRIA (ÓBITOS)
# ==============================================================================

piramide_obitos <- base_filtrada %>%
  filter(EVOLUCAO == 2) %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado", faixa_etaria %in% ORDEM_FAIXAS) %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS),
    value        = ifelse(sexo == "Masculino", -n, n)
  )

if (nrow(piramide_obitos) > 0) {
  n_piramide_obitos <- sum(piramide_obitos$n)
  
  g18 <- ggplot(piramide_obitos, aes(x = faixa_etaria, y = value, fill = sexo)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_text(aes(label = n, hjust = ifelse(sexo == "Masculino", 1.15, -0.15)), size = 3.5) +
    coord_flip() +
    scale_y_continuous(labels = function(x) abs(x)) +
    scale_fill_manual(values = c("Masculino" = "#0057A3", "Feminino" = "#E91E8C")) +
    labs(
      title   = paste0("Pirâmide Etária — Óbitos — ", escopo_titulo,
                       " (N = ", format(n_piramide_obitos, big.mark = "."), ")"),
      x = "Faixa Etária", y = "Número de Óbitos", fill = "Sexo", caption = texto_rodape
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  salvar_grafico(g18, "18_piramide_etaria_obitos", height = 8)
}


# ==============================================================================
# GRÁFICO 19 — EVOLUÇÃO (DESFECHO)
# ==============================================================================

casos_evolucao <- base_filtrada %>%
  mutate(
    evolucao_label = case_when(
      EVOLUCAO == 1 ~ "Cura",
      EVOLUCAO == 2 ~ "Óbito por SRAG",
      EVOLUCAO == 3 ~ "Óbito por Outras Causas",
      EVOLUCAO == 9 ~ "Ignorado",
      TRUE          ~ "Em investigação"
    )
  ) %>%
  group_by(evolucao_label) %>%
  summarise(total = n(), .groups = "drop")

n_evol <- sum(casos_evolucao$total)

g19 <- ggplot(casos_evolucao,
              aes(x = total, y = fct_reorder(evolucao_label, total))) +
  geom_col(fill = "#A30000") +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title   = paste0("Casos por Evolução (Desfecho) — ", escopo_titulo,
                     " (N = ", format(n_evol, big.mark = "."), ")"),
    x = "Número de Casos", y = "Evolução", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g19, "19_evolucao_desfecho")


# ==============================================================================
# GRÁFICO 20 — RAÇA / COR
# ==============================================================================

casos_raca <- base_filtrada %>%
  mutate(
    raca_label = case_when(
      CS_RACA == 1 ~ "Branca",
      CS_RACA == 2 ~ "Preta",
      CS_RACA == 3 ~ "Amarela",
      CS_RACA == 4 ~ "Parda",
      CS_RACA == 5 ~ "Indígena",
      TRUE         ~ "Ignorado/Outros"
    )
  ) %>%
  group_by(raca_label) %>%
  summarise(total = n(), .groups = "drop")

n_raca <- sum(casos_raca$total)

g20 <- ggplot(casos_raca,
              aes(x = total, y = fct_reorder(raca_label, total))) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title   = paste0("Casos por Raça/Cor — ", escopo_titulo,
                     " (N = ", format(n_raca, big.mark = "."), ")"),
    x = "Número de Casos", y = "Raça/Cor", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g20, "20_raca_cor")


# ==============================================================================
# GRÁFICO 21 — ESCOLARIDADE (> 18 ANOS)
# ==============================================================================

casos_escol <- base_filtrada %>%
  filter(NU_IDADE_N > 18) %>%
  mutate(
    escolaridade_label = case_when(
      CS_ESCOL_N == "0" ~ "Analfabeto",
      CS_ESCOL_N == "1" ~ "Fund. 1º Ciclo (1ª–5ª série)",
      CS_ESCOL_N == "2" ~ "Fund. 2º Ciclo (6ª–9ª série)",
      CS_ESCOL_N == "3" ~ "Ensino Médio",
      CS_ESCOL_N == "4" ~ "Ensino Superior",
      CS_ESCOL_N == "5" ~ "Não se aplica",
      CS_ESCOL_N == "9" ~ "Ignorado",
      TRUE              ~ "Não Registrado"
    )
  ) %>%
  group_by(escolaridade_label) %>%
  summarise(total = n(), .groups = "drop")

n_escol <- sum(casos_escol$total)

g21 <- ggplot(casos_escol,
              aes(x = total, y = fct_reorder(escolaridade_label, total))) +
  geom_col(fill = "#FF8C00") +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title   = paste0("Casos por Escolaridade (> 18 anos) — ", escopo_titulo,
                     " (N = ", format(n_escol, big.mark = "."), ")"),
    x = "Número de Casos", y = "Escolaridade", caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g21, "21_escolaridade")


# ==============================================================================
# GRÁFICOS DE BAIRRO — TOP 20 MARINGÁ E SARANDI
# ==============================================================================

titulo_ano <- paste(anos_carregar, collapse = "/")

# Top 20 bairros — Maringá
g_bairro_mar <- casos_bairro %>%
  head(20) %>%
  mutate(BAIRRO = fct_reorder(str_to_title(BAIRRO), casos)) %>%
  ggplot(aes(x = casos, y = BAIRRO)) +
  geom_col(fill = "#2166ac") +
  geom_text(aes(label = casos), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste("Top 20 Bairros — Casos de SRAG (Maringá/PR) |", titulo_ano),
    subtitle = "Fonte: SIVEP-GRIPE | Residência do paciente",
    x = "Casos notificados", y = NULL, caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(g_bairro_mar,
               paste0("srag_top20_bairros_maringa_", paste(anos_carregar, collapse = "_")))

# Top 20 bairros — Sarandi
if (nrow(casos_bairro_sar) > 0) {
  g_bairro_sar <- casos_bairro_sar %>%
    head(20) %>%
    mutate(BAIRRO = fct_reorder(str_to_title(BAIRRO), casos)) %>%
    ggplot(aes(x = casos, y = BAIRRO)) +
    geom_col(fill = "#1b7837") +
    geom_text(aes(label = casos), hjust = -0.2, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = paste("Top 20 Bairros — Casos de SRAG (Sarandi/PR) |", titulo_ano),
      subtitle = "Fonte: SIVEP-GRIPE | Residência do paciente",
      x = "Casos notificados", y = NULL, caption = texto_rodape
    ) +
    theme_minimal()
  
  salvar_grafico(g_bairro_sar,
                 paste0("srag_top20_bairros_sarandi_", paste(anos_carregar, collapse = "_")))
}

# Heatmap bairro x semana — Maringá
top15 <- head(casos_bairro$BAIRRO, 15)

g_heat_mar <- casos_bairro_sem %>%
  filter(BAIRRO %in% top15) %>%
  left_join(
    casos_bairro %>% select(BAIRRO, total_bairro = casos),
    by = "BAIRRO"
  ) %>%
  mutate(LABEL = fct_reorder(
    paste0(str_to_title(BAIRRO), " (", total_bairro, ")"),
    total_bairro,
    sum
  )) %>%
  ggplot(aes(x = SEM_EPI, y = LABEL, fill = casos)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    title    = paste("SRAG por Bairro e Semana Epidemiológica — Maringá |", titulo_ano),
    subtitle = "Top 15 bairros | Total acumulado entre parênteses",
    x = "Semana epidemiológica", y = NULL, fill = "Casos\nna semana",
    caption = texto_rodape
  ) +
  theme_minimal()
salvar_grafico(g_heat_mar,
               paste0("srag_heatmap_bairro_semana_maringa_", paste(anos_carregar, collapse = "_")),
               width = 14, height = 6)

# Heatmap bairro x semana — Sarandi
g_heat_sar <- casos_bairro_sem_sar %>%
  filter(BAIRRO %in% head(casos_bairro_sar$BAIRRO, 15)) %>%
  left_join(
    casos_bairro_sar %>% select(BAIRRO, total_bairro = casos),
    by = "BAIRRO"
  ) %>%
  mutate(LABEL = fct_reorder(
    paste0(str_to_title(BAIRRO), " (", total_bairro, ")"),
    total_bairro,
    sum
  )) %>%
  ggplot(aes(x = SEM_EPI, y = LABEL, fill = casos)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    title    = paste("SRAG por Bairro e Semana Epidemiológica — Sarandi |", titulo_ano),
    subtitle = "Top 15 bairros | Total acumulado entre parênteses",
    x = "Semana epidemiológica", y = NULL, fill = "Casos\nna semana",
    caption = texto_rodape
  ) +
  theme_minimal()

salvar_grafico(
  g_heat_sar,
  paste0("srag_heatmap_bairro_semana_sarandi_", paste(anos_carregar, collapse = "_")),
  width = 14, height = 6
)



# Série por classificação etiológica
g_class <- casos_semana_class %>%
  ggplot(aes(x = SEM_EPI, y = casos, color = CLASSIFICACAO, group = CLASSIFICACAO)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = paste("SRAG por Classificação Etiológica e Semana |", titulo_ano),
    subtitle = paste0(escopo_titulo, " | Fonte: SIVEP-GRIPE"),
    x = "Semana epidemiológica", y = "Casos notificados",
    color = NULL, caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

salvar_grafico(g_class,
               paste0("srag_serie_classificacao_", paste(anos_carregar, collapse = "_")))


# ==============================================================================
# MAPAS GEOGRÁFICOS (tmap)
# ==============================================================================

tmap_mode("plot")

# --- Mapa por município ---
if (file.exists(CAMINHO_SHP_MUNICIPIOS)) {
  message("\nGerando mapa por município...")
  malha_pr <- sf::st_read(CAMINHO_SHP_MUNICIPIOS, quiet = TRUE)
  
  malha_15rs <- malha_pr %>%
    mutate(CO_MUN_6 = floor(as.integer(CD_MUN) / 10)) %>%
    filter(CO_MUN_6 %in% municipios_15rs$codigo_ibge_6) %>%
    left_join(casos_municipio, by = c("CO_MUN_6" = "CO_MUN_RES"))
  
  mapa_casos <- tm_shape(malha_15rs) +
    tm_polygons(
      fill        = "casos",
      fill.scale  = tm_scale_continuous(values = "brewer.blues"),
      fill.legend = tm_legend(title = "Casos de SRAG"),
      col = "white", lwd = 0.5
    ) +
    tm_text("NM_MUN", size = 0.45, col = "grey20") +
    tm_title(paste("SRAG — Casos por Município\n15ª RS Maringá/PR |", titulo_ano)) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  tmap_save(mapa_casos,
            file.path(DIR_GRAFICOS, paste0("mapa_srag_casos_", paste(anos_carregar, collapse = "_"), ".png")),
            width = 2400, height = 2000)
  
  mapa_incid <- tm_shape(malha_15rs) +
    tm_polygons(
      fill        = "incidencia_100k",
      fill.scale  = tm_scale_continuous(values = "brewer.yl_or_rd"),
      fill.legend = tm_legend(title = "Casos/100 mil hab."),
      col = "white", lwd = 0.5
    ) +
    tm_text("NM_MUN", size = 0.45, col = "grey20") +
    tm_title(paste("SRAG — Incidência\n15ª RS Maringá/PR |", titulo_ano, "| Pop. IBGE 2025")) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  tmap_save(mapa_incid,
            file.path(DIR_GRAFICOS, paste0("mapa_srag_incidencia_", paste(anos_carregar, collapse = "_"), ".png")),
            width = 2400, height = 2000)
  
  message("Mapas por município salvos.")
} else {
  warning("Shapefile de municípios não encontrado: ", CAMINHO_SHP_MUNICIPIOS)
}


# --- Função de mapa por bairro ---
gera_mapa_bairro <- function(bairros_geo, casos_bairro_mun, col_nome,
                             de_para, corte, nome_municipio, ano) {
  bairros_geo <- bairros_geo %>% mutate(JOIN_KEY = normaliza_bairro(.data[[col_nome]]))
  casos_join  <- casos_bairro_mun %>% mutate(JOIN_KEY = normaliza_bairro(BAIRRO))
  
  lookup <- bind_rows(
    casos_join %>% rename(SHP_KEY = JOIN_KEY) %>% select(SHP_KEY, casos, obitos_srag, uti, letalidade),
    de_para %>%
      left_join(casos_join %>% rename(SIVEP_KEY = JOIN_KEY), by = "SIVEP_KEY") %>%
      select(SHP_KEY, casos, obitos_srag, uti, letalidade)
  ) %>%
    group_by(SHP_KEY) %>%
    summarise(
      casos       = sum(casos,       na.rm = TRUE),
      obitos_srag = sum(obitos_srag, na.rm = TRUE),
      uti         = sum(uti,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      letalidade = round(obitos_srag / casos * 100, 1),
      casos      = if_else(casos == 0L, NA_integer_, casos)
    )
  
  mapa_dados    <- bairros_geo %>% left_join(lookup, by = c("JOIN_KEY" = "SHP_KEY"))
  bairros_nome  <- mapa_dados %>% filter(!is.na(casos) & casos >= corte)
  
  mapa <- tm_shape(mapa_dados) +
    tm_polygons(
      fill        = "casos",
      fill.scale  = tm_scale_continuous(values = "brewer.yl_or_rd", value.na = "grey90"),
      fill.legend = tm_legend(title = "Casos de SRAG"),
      col = "grey40", lwd = 0.5
    ) +
    tm_title(paste0("SRAG por Bairro — ", nome_municipio, "/PR | ", ano,
                    "\nNomes exibidos: >= ", corte, " casos")) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  if (nrow(bairros_nome) > 0) {
    mapa <- mapa +
      tm_shape(bairros_nome) +
      tm_text(text = col_nome, size = 0.35, col = "grey10",
              fontface = "bold", remove.overlap = TRUE)
  }
  mapa
}

# --- Mapa de bairros — Maringá ---
de_para_maringa <- tibble::tribble(
  ~SHP_KEY,                                                ~SIVEP_KEY,
  "JARDIM ALVORADA I PARTE",                               "JARDIM ALVORADA",
  "JARDIM ALVORADA II PARTE",                              "JARDIM ALVORADA",
  "SUB LT 77A71 JARDIM ALVORADA III",                      "JARDIM ALVORADA",
  "CONJUNTO HABITACIONAL REQUIAO I 1 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 2 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 3 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 4 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "PARQUE ITAIPU I PARTE",                                 "JARDIM ITAIPU",
  "PARQUE ITAIPU II PARTE",                                "JARDIM ITAIPU",
  "PARQUE HORTENCIA I PARTE",                              "PARQUE HORTENCIA",
  "PARQUE HORTENCIA II PARTE",                             "PARQUE HORTENCIA",
  "LOTEAMENTO LIBERDADE I PARTE",                          "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE II PARTE",                         "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE III PARTE",                        "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE IV PARTE",                         "JARDIM LIBERDADE",
  "CONJUNTO CIDADE ALTA",                                  "CONJUNTO RESIDENCIAL CIDADE AL"
)

if (file.exists(CAMINHO_SHP_MARINGA)) {
  message("\nGerando mapa de bairros — Maringá...")
  bairros_mar <- sf::st_read(CAMINHO_SHP_MARINGA, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)
  
  mapa_mar <- gera_mapa_bairro(
    bairros_geo      = bairros_mar,
    casos_bairro_mun = casos_bairro,
    col_nome         = "NOME",
    de_para          = de_para_maringa,
    corte            = CORTE_NOME_BAIRRO,
    nome_municipio   = "Maringá",
    ano              = titulo_ano
  )
  
  tmap_save(mapa_mar,
            file.path(DIR_GRAFICOS, paste0("mapa_srag_bairro_maringa_", paste(anos_carregar, collapse = "_"), ".png")),
            width = 2400, height = 2400)
  message("Mapa de Maringá salvo.")
} else {
  warning("Shapefile de bairros de Maringá não encontrado: ", CAMINHO_SHP_MARINGA)
}

# --- Mapa de bairros — Sarandi ---
de_para_sarandi <- tibble::tribble(
  ~SHP_KEY, ~SIVEP_KEY
  # Adicione correspondências conforme necessário
)

if (file.exists(CAMINHO_SHP_SARANDI)) {
  message("\nGerando mapa de bairros — Sarandi...")
  bairros_sar <- sf::st_read(CAMINHO_SHP_SARANDI, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)
  
  mapa_sar <- gera_mapa_bairro(
    bairros_geo      = bairros_sar,
    casos_bairro_mun = casos_bairro_sar,
    col_nome         = "Bairro",
    de_para          = de_para_sarandi,
    corte            = CORTE_NOME_BAIRRO_SAR,
    nome_municipio   = "Sarandi",
    ano              = titulo_ano
  )
  
  tmap_save(mapa_sar,
            file.path(DIR_GRAFICOS, paste0("mapa_srag_bairro_sarandi_", paste(anos_carregar, collapse = "_"), ".png")),
            width = 2400, height = 2400)
  message("Mapa de Sarandi salvo.")
} else {
  warning("Shapefile de bairros de Sarandi não encontrado: ", CAMINHO_SHP_SARANDI)
}


# ==============================================================================
# EXPORTAÇÃO EXCEL (opcional)
# ==============================================================================

dir_tabelas <- file.path(dirname(DIR_GRAFICOS), "tabelas")
if (!dir.exists(dir_tabelas)) dir.create(dir_tabelas, recursive = TRUE)

writexl::write_xlsx(
  list(
    "municipios_15rs"         = municipios_15rs,
    "casos_municipio"         = casos_municipio,
    "casos_bairro_maringa"    = casos_bairro,
    "casos_bairro_sarandi"    = casos_bairro_sar,
    "serie_temporal"          = casos_semana,
    "serie_por_classificacao" = casos_semana_class
  ),
  file.path(dir_tabelas, paste0("srag_15rs_", paste(anos_carregar, collapse = "_"), ".xlsx"))
)


# ==============================================================================
# RESUMO FINAL
# ==============================================================================

n_obitos   <- sum(base_filtrada$EVOLUCAO == 2, na.rm = TRUE)
n_pcr_pos  <- sum(base_filtrada$PCR_RESUL == 1, na.rm = TRUE)
letalidade <- round(n_obitos / nrow(base_filtrada) * 100, 2)

message("\n", strrep("=", 60))
message("RESUMO")
message(strrep("=", 60))
message("Escopo              : ", escopo_titulo)
message("Ano(s) analisados   : ", paste(anos_carregar, collapse = ", "))
message("Pop. IBGE 2025      : ", format(POPULACAO_ESCOPO, big.mark = "."))
message("Total notificações  : ", format(nrow(base_filtrada), big.mark = "."))
message("Tx notif. /100k hab.: ", round(nrow(base_filtrada) / POPULACAO_ESCOPO * 100000, 1))
message("Confirmados PCR     : ", format(n_pcr_pos, big.mark = "."))
message("Óbitos (EVOLUCAO=2) : ", format(n_obitos, big.mark = "."))
message("Letalidade          : ", letalidade, "%")
message("Saída               : ", DIR_GRAFICOS)
message(strrep("=", 60))
message("Concluído. Rode quarto render e git push para atualizar o site.")