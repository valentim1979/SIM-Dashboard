# ==============================================================================
# ANÁLISE SIVEP-GRIPE — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# ==============================================================================
# Autor       : Valentim Sala Junior
# Atualizado  : 2025
# Descrição   : Script modular para análise epidemiológica de SRAG.
#               Os municípios e populações são carregados automaticamente
#               do arquivo IBGE_15rs (ibge_cnv_pop.csv).
#               Selecione o ano e o município no bloco de configuração abaixo.
# ==============================================================================


# ==============================================================================
# BLOCO 0 — CONFIGURAÇÃO GLOBAL (EDITE AQUI)
# ==============================================================================

# --- 0.1 Seleção de Ano(s) ---
# Para um único ano:   ANO_ANALISE <- 2026
# Para múltiplos:      ANO_ANALISE <- c(2024, 2025)
# Para todos:          ANO_ANALISE <- NULL
#
# Nota sobre a curva epidêmica comparativa (Bloco 6):
#   - Ano único  (ex.: 2026) → curva inclui automaticamente 2024, 2025 e 2026
#   - Múltiplos anos         → curva usa os próprios anos selecionados
#   - NULL (todos)           → curva usa todos os anos disponíveis
ANO_ANALISE <- 2025

# --- 0.2 Seleção de Município ---
# Use o nome exatamente como aparece no arquivo IBGE (sem o código numérico).
# Para a regional inteira (todos os municípios): MUNICIPIO_ANALISE <- NULL
#
# Municípios da 15ª RS disponíveis no arquivo IBGE:
#   NULL                         → Toda a 15ª Regional
#   "MARINGA"                    → Apenas Maringá       (pop. 429.660)
#   "SARANDI"                    → Apenas Sarandi       (pop. 128.106)
#   "PAICANDU"                   → Apenas Paiçandu      (pop.  48.695)
#   "MARIALVA"                   → Apenas Marialva      (pop.  44.749)
#   "MANDAGUARI"                 → Apenas Mandaguari    (pop.  38.313)
#   "MANDAGUACU"                 → Apenas Mandaguaçu    (pop.  34.521)
#   "NOVA ESPERANCA"             → Nova Esperança       (pop.  27.142)
#   "ASTORGA"                    → Astorga              (pop.  26.203)
#   "COLORADO"                   → Colorado             (pop.  23.313)
#   "FLORAI"                     → Floraí               (pop.   4.805)
#   "FLORESTA"                   → Floresta             (pop.  11.522)
#   "FLORIDA"                    → Flórida              (pop.   2.711)
#   "IGUARACU"                   → Iguaraçu             (pop.   5.693)
#   "ITAGUAJE"                   → Itaguajé             (pop.   4.530)
#   "ITAMBE"                     → Itambé               (pop.   6.228)
#   "IVATUBA"                    → Ivatuba              (pop.   2.685)
#   "LOBATO"                     → Lobato               (pop.   4.707)
#   "MUNHOZ DE MELO"             → Munhoz de Melo       (pop.   4.057)
#   "NOSSA SENHORA DAS GRACAS"   → N. S. das Graças     (pop.   3.669)
#   "OURIZONA"                   → Ourizona             (pop.   3.193)
#   "PARANACITY"                 → Paranacity           (pop.   9.549)
#   "PRESIDENTE CASTELO BRANCO"  → Pres. Castelo Branco (pop.   4.304)
#   "SANTA FE"                   → Santa Fé             (pop.  11.730)
#   "SANTA INES"                 → Santa Inês           (pop.   1.745)
#   "SANTO INACIO"               → Santo Inácio         (pop.   6.463)
#   "SAO JORGE DO IVAI"          → São Jorge do Ivaí    (pop.   5.170)
#   "UNIFLOR"                    → Uniflor              (pop.   2.106)
#   "ANGULO"                     → Ângulo               (pop.   3.357)
#   "ATALAIA"                    → Atalaia              (pop.   4.046)
#   "DOUTOR CAMARGO"             → Doutor Camargo       (pop.   6.517)
MUNICIPIO_ANALISE <- NULL

# --- 0.3 Caminhos de arquivo ---
DIRETORIO_DBF <- "C:/SIVEPGRIPE/BaseDBF"

ARQUIVO_IBGE  <- "C:/Users/valentim.junior/OneDrive/Área de Trabalho/sivep_15rs/ibge_cnv_pop.csv"


# ==============================================================================
# BLOCO A — DIRETÓRIO DE SAÍDA DOS GRÁFICOS
# ==============================================================================

DIR_GRAFICOS <- "C:/SIVEPGRIPE/Graficos_boletim"

if (!dir.exists(DIR_GRAFICOS)) {
  dir.create(DIR_GRAFICOS, recursive = TRUE)
}

message("Diretório de gráficos: ", DIR_GRAFICOS)


# ==============================================================================
# BLOCO 1 — INSTALAÇÃO E CARREGAMENTO DE PACOTES
# ==============================================================================

pacotes_necessarios <- c("sf", "foreign", "dplyr", "ggplot2", "scales", "tidyr", "readr")

instalar_se_ausente <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(pacotes_necessarios, instalar_se_ausente))

library(sf)
library(foreign)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(readr)


# ==============================================================================
# BLOCO 2 — CARREGAMENTO DO ARQUIVO IBGE (municípios + população 2025)
# ==============================================================================
# Fonte: DATASUS/RIPSA — Estimativas Populacionais por Município 2000-2025
# Formato: CSV com separador ";", encoding Latin1, cabeçalho na linha 6.

ibge_raw <- readr::read_delim(
  ARQUIVO_IBGE,
  delim      = ";",
  skip       = 6,
  locale     = readr::locale(encoding = "latin1"),
  col_names  = c("municipio_raw", "populacao"),
  col_types  = readr::cols(municipio_raw = "c", populacao = "d"),
  show_col_types = FALSE
)

ibge_15rs <- ibge_raw %>%
  filter(
    !is.na(populacao),
    !grepl("^Total", municipio_raw, ignore.case = TRUE),
    !grepl("^\\s*$", municipio_raw)
  ) %>%
  mutate(
    codigo_ibge    = as.integer(trimws(substr(municipio_raw, 1, 6))),
    nome_municipio = trimws(substr(municipio_raw, 8, nchar(municipio_raw))),
    populacao      = as.integer(populacao)
  ) %>%
  select(codigo_ibge, nome_municipio, populacao) %>%
  filter(!is.na(codigo_ibge))

message("Municípios carregados do IBGE: ", nrow(ibge_15rs))
message("População total da 15ª RS: ", format(sum(ibge_15rs$populacao), big.mark = "."))
print(ibge_15rs)

MUNICIPIOS_15RS      <- ibge_15rs$nome_municipio
CODIGOS_IBGE_15RS    <- ibge_15rs$codigo_ibge
POPULACAO_15RS_TOTAL <- sum(ibge_15rs$populacao)


# ==============================================================================
# BLOCO 3 — FUNÇÕES AUXILIARES
# ==============================================================================

diagnosticar_diretorio <- function(diretorio) {
  if (!dir.exists(diretorio)) {
    stop(
      "\n[ERRO] Diretório não encontrado: ", diretorio,
      "\nVerifique o valor de DIRETORIO_DBF no Bloco 0.",
      "\nDica: no Windows use barras duplas ou barra normal:\n",
      '  DIRETORIO_DBF <- "C:/SIVEPGRIPE/BaseDBF"\n',
      '  DIRETORIO_DBF <- "C:\\\\SIVEPGRIPE\\\\BaseDBF"'
    )
  }
  dbfs <- list.files(diretorio, pattern = "(?i)\\.dbf$", full.names = FALSE)
  message("\nDiretório DBF encontrado: ", diretorio)
  if (length(dbfs) == 0) {
    warning("Nenhum arquivo .dbf encontrado em: ", diretorio)
  } else {
    message("Arquivos .dbf disponíveis (", length(dbfs), "):")
    message(paste0("  ", dbfs, collapse = "\n"))
  }
  invisible(dbfs)
}

carregar_base <- function(ano, diretorio) {
  nomes_possiveis <- c(
    paste0("SRAGHOSP", ano, ".dbf"),
    paste0("sraghosp", ano, ".dbf"),
    paste0("SRAGHOSP", ano, ".DBF")
  )
  caminho <- NULL
  for (nome in nomes_possiveis) {
    candidato <- file.path(diretorio, nome)
    if (file.exists(candidato)) { caminho <- candidato; break }
  }
  if (is.null(caminho)) {
    message("  [aviso] Arquivo não encontrado para o ano ", ano, " em: ", diretorio)
    return(NULL)
  }
  base <- tryCatch({
    df <- foreign::read.dbf(caminho, as.is = TRUE)
    message("  [OK] foreign::read.dbf | ", caminho, " | ", nrow(df), " registros")
    df
  }, error = function(e) {
    message("  [aviso] foreign::read.dbf falhou: ", conditionMessage(e))
    NULL
  })
  if (is.null(base)) {
    base <- tryCatch({
      df <- sf::st_read(caminho, stringsAsFactors = FALSE, quiet = TRUE)
      if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      message("  [OK] sf::st_read | ", caminho, " | ", nrow(df), " registros")
      df
    }, error = function(e) {
      message("  [ERRO] sf::st_read também falhou: ", conditionMessage(e))
      NULL
    })
  }
  if (!is.null(base)) base$ANO_BASE <- ano
  return(base)
}

gerar_rodape <- function() {
  paste0("Fonte: SIVEP-Gripe | Dados extraídos em: ", format(Sys.Date(), "%d/%m/%Y"))
}

aplicar_filtros <- function(base, municipio = NULL) {
  resultado <- base %>%
    filter(
      grepl("15RS", ID_REGIONA, ignore.case = TRUE) &
        grepl("MARINGA", ID_REGIONA, ignore.case = TRUE)
    )
  message("Registros da 15ª RS: ", nrow(resultado))
  if (nrow(resultado) == 0) {
    warning("Nenhum registro encontrado para a 15ª RS. Verifique o campo ID_REGIONA.")
    return(resultado)
  }
  if (!is.null(municipio) && nzchar(trimws(municipio))) {
    mun_upper  <- toupper(trimws(municipio))
    codigo_mun <- ibge_15rs %>%
      filter(toupper(nome_municipio) == mun_upper) %>%
      pull(codigo_ibge)
    if (length(codigo_mun) > 0 && "CO_MUN_RES" %in% names(resultado)) {
      resultado <- resultado %>%
        filter(as.integer(CO_MUN_RES) == codigo_mun[1])
      message("Filtro por código IBGE (CO_MUN_RES == ", codigo_mun[1],
              "): ", nrow(resultado), " registros")
    } else {
      resultado <- resultado %>%
        filter(grepl(mun_upper, toupper(ID_MUNICIP), fixed = TRUE))
      message("Filtro por nome (ID_MUNICIP ~ '", municipio,
              "'): ", nrow(resultado), " registros")
    }
  }
  return(resultado)
}

populacao_escopo <- function(municipio = NULL) {
  if (is.null(municipio) || !nzchar(trimws(municipio))) return(POPULACAO_15RS_TOTAL)
  mun_upper <- toupper(trimws(municipio))
  pop <- ibge_15rs %>% filter(toupper(nome_municipio) == mun_upper) %>% pull(populacao)
  if (length(pop) == 0) {
    warning("Município '", municipio, "' não encontrado no IBGE. Usando população total da RS.")
    return(POPULACAO_15RS_TOTAL)
  }
  return(pop[1])
}

criar_faixa_etaria <- function(df) {
  df %>%
    mutate(
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
  df %>%
    mutate(
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

texto_rodape <- gerar_rodape()

salvar_grafico <- function(grafico, nome_arquivo) {
  caminho <- file.path(DIR_GRAFICOS, paste0(nome_arquivo, ".png"))
  ggsave(
    filename = caminho,
    plot     = grafico,
    width    = 12,
    height   = 7,
    units    = "in",
    dpi      = 300,
    bg       = "white"
  )
  message("Gráfico salvo: ", caminho)
}


# ==============================================================================
# BLOCO 4 — DEFINIÇÃO DOS INTERVALOS DE ANOS E CARREGAMENTO DAS BASES
# ==============================================================================

anos_disponiveis <- 2019:2026

# --- Anos para a análise principal (gráficos 7–21) ---
anos_carregar <- if (!is.null(ANO_ANALISE)) {
  intersect(ANO_ANALISE, anos_disponiveis)
} else {
  anos_disponiveis
}

# --- Anos para a curva epidêmica comparativa (Bloco 6) ---
#   Ano único  → ano selecionado + os 2 anos anteriores
#   Múltiplos  → os próprios anos selecionados
#   NULL       → todos os anos disponíveis
anos_curva <- if (is.null(ANO_ANALISE)) {
  anos_disponiveis
} else if (length(ANO_ANALISE) == 1) {
  intersect(seq(ANO_ANALISE - 2, ANO_ANALISE), anos_disponiveis)
} else {
  intersect(ANO_ANALISE, anos_disponiveis)
}

message("\nAnos da análise principal : ", paste(anos_carregar, collapse = ", "))
message("Anos da curva comparativa : ", paste(anos_curva,    collapse = ", "))

# --- Carrega a união dos dois conjuntos (sem duplicatas) ---
anos_a_carregar <- sort(union(anos_carregar, anos_curva))

diagnosticar_diretorio(DIRETORIO_DBF)

lista_bases <- Filter(
  Negate(is.null),
  lapply(anos_a_carregar, carregar_base, diretorio = DIRETORIO_DBF)
)

if (length(lista_bases) == 0) {
  stop(
    "\n[ERRO] Nenhuma base carregada.",
    "\nVerifique:",
    "\n  1. DIRETORIO_DBF = '", DIRETORIO_DBF, "'",
    "\n  2. Os arquivos devem se chamar SRAGHOSP2025.dbf, SRAGHOSP2024.dbf etc.",
    "\n  3. ANO_ANALISE = ", paste(ANO_ANALISE, collapse = ", "),
    "\n\nArquivos encontrados no diretório:",
    "\n  ", paste(list.files(DIRETORIO_DBF), collapse = "\n  ")
  )
}

base_completa <- dplyr::bind_rows(lista_bases)
message("Total de registros combinados: ", format(nrow(base_completa), big.mark = "."))

message("\nValores de ID_REGIONA (top 10):")
print(sort(table(base_completa$ID_REGIONA, useNA = "ifany"), decreasing = TRUE)[1:10])


# ==============================================================================
# BLOCO 5 — FILTRO PRINCIPAL (apenas anos_carregar — usada nos gráficos 7–21)
# ==============================================================================

base_ano_principal <- base_completa %>% filter(ANO_BASE %in% anos_carregar)
base_filtrada      <- aplicar_filtros(base_ano_principal, municipio = MUNICIPIO_ANALISE)
POPULACAO_ESCOPO   <- populacao_escopo(MUNICIPIO_ANALISE)

if (nrow(base_filtrada) == 0) {
  stop("Nenhum registro após filtros. Revise MUNICIPIO_ANALISE ou ANO_ANALISE.")
}

escopo_titulo <- if (!is.null(MUNICIPIO_ANALISE) && nzchar(MUNICIPIO_ANALISE)) {
  paste0(tools::toTitleCase(tolower(MUNICIPIO_ANALISE)), " — 15ª RS Maringá")
} else {
  "15ª Regional de Saúde de Maringá"
}

message("\nEscopo   : ", escopo_titulo)
message("Registros: ", format(nrow(base_filtrada), big.mark = "."))
message("Pop. IBGE: ", format(POPULACAO_ESCOPO,   big.mark = "."))


# ==============================================================================
# BLOCO 6 — CURVA EPIDÊMICA COMPARATIVA
# Sempre gerada. Quando ANO_ANALISE é um único ano, exibe esse ano
# e os 2 anteriores. O ano de análise é destacado em negrito na legenda.
# ==============================================================================

# Base filtrada pelo escopo geográfico, usando os anos da curva
base_curva_rs <- base_completa %>%
  filter(ANO_BASE %in% anos_curva) %>%
  aplicar_filtros(municipio = MUNICIPIO_ANALISE)

casos_semana_ano <- base_curva_rs %>%
  mutate(
    DT_NOTIFIC_DATE = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    Ano             = format(DT_NOTIFIC_DATE, "%Y"),
    Semana_Epi      = as.numeric(format(DT_NOTIFIC_DATE, "%V"))
  ) %>%
  filter(!is.na(DT_NOTIFIC_DATE)) %>%
  group_by(Ano, Semana_Epi) %>%
  summarise(Total_Casos = n(), .groups = "drop")

if (nrow(casos_semana_ano) > 0) {
  
  # Cada ano recebe uma cor distinta de uma paleta qualitativa.
  # O(s) ano(s) em análise ficam com linha mais grossa para se destacar.
  anos_destaque <- as.character(anos_carregar)
  anos_contexto <- setdiff(unique(casos_semana_ano$Ano), anos_destaque)
  todos_anos    <- sort(unique(casos_semana_ano$Ano))
  
  # Paleta qualitativa com cores bem separadas (suporta até 8 anos)
  paleta_base <- c("#E63946", "#F4A261", "#2A9D8F", "#457B9D",
                   "#6A0572", "#E9C46A", "#264653", "#A8DADC")
  cores_curva    <- setNames(paleta_base[seq_along(todos_anos)], todos_anos)
  
  # Ano(s) em análise: linha grossa (2.2); anos de contexto: linha fina (0.9)
  espessuras_curva <- setNames(
    ifelse(todos_anos %in% anos_destaque, 2.2, 0.9),
    todos_anos
  )
  
  # N por ano — usado nos rótulos da legenda
  n_por_ano <- casos_semana_ano %>%
    group_by(Ano) %>%
    summarise(n_ano = sum(Total_Casos), .groups = "drop")
  
  rotulos_legenda <- setNames(
    paste0(n_por_ano$Ano, "  (N = ", format(n_por_ano$n_ano, big.mark = "."), ")"),
    n_por_ano$Ano
  )
  
  g06 <- ggplot(casos_semana_ano,
                aes(x = Semana_Epi, y = Total_Casos,
                    group = Ano, color = Ano, linewidth = Ano)) +
    geom_line(alpha = 0.9) +
    geom_point(size = 1.2, alpha = 0.85) +
    scale_color_manual(values = cores_curva, labels = rotulos_legenda) +
    scale_linewidth_manual(values = espessuras_curva, guide = "none") +
    scale_x_continuous(breaks = seq(1, 52, by = 4), limits = c(1, 53)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste0("Curva Epidêmica Comparativa — ", escopo_titulo),
      subtitle = paste0(
        "Ano(s) em análise: ", paste(anos_carregar, collapse = ", "),
        " | Contexto histórico: ", paste(anos_contexto, collapse = ", ")
      ),
      x       = "Semana Epidemiológica",
      y       = "Casos Notificados",
      color   = "Ano",
      caption = texto_rodape
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
  
  print(g06)
  salvar_grafico(g06, "06_curva_epidemica_comparativa")
  
} else {
  message("[aviso] Bloco 6: nenhum dado disponível para os anos da curva comparativa.")
}


# ==============================================================================
# BLOCO 7 — NOTIFICAÇÕES POR SEMANA EPIDEMIOLÓGICA
# ==============================================================================

casos_semana <- base_filtrada %>%
  group_by(SEM_NOT) %>%
  summarise(total_notificacoes = n(), .groups = "drop")

n_semana   <- sum(casos_semana$total_notificacoes)
incid_100k <- round(n_semana / POPULACAO_ESCOPO * 100000, 1)

g07 <- ggplot(casos_semana, aes(x = factor(SEM_NOT), y = total_notificacoes)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total_notificacoes), vjust = -0.5, size = 3.2) +
  labs(
    title    = paste0("SRAG por Semana Epidemiológica — ", escopo_titulo,
                      " (N = ", format(n_semana, big.mark = "."), ")"),
    subtitle = paste0(
      "N = ", format(n_semana, big.mark = "."),
      " | Taxa: ", incid_100k, " por 100.000 hab.",
      " | Pop. IBGE 2025: ", format(POPULACAO_ESCOPO, big.mark = ".")
    ),
    x       = "Semana Epidemiológica",
    y       = "Notificações",
    caption = texto_rodape
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g07)
salvar_grafico(g07, "07_notificacoes_semana_epi")


# ==============================================================================
# BLOCO 8 — CASOS CONFIRMADOS POR SEMANA (PCR_RESUL == 1)
# ==============================================================================

casos_confirmados <- base_filtrada %>%
  filter(PCR_RESUL == 1) %>%
  group_by(SEM_NOT) %>%
  summarise(total_notificacoes = n(), .groups = "drop")

n_conf <- sum(casos_confirmados$total_notificacoes)

g08 <- ggplot(casos_confirmados, aes(x = factor(SEM_NOT), y = total_notificacoes)) +
  geom_col(fill = "#C0392B") +
  geom_text(aes(label = total_notificacoes), vjust = -0.5, size = 3.2) +
  labs(
    title    = paste0("SRAG Confirmado por Semana Epidemiológica — ", escopo_titulo,
                      " (N = ", format(n_conf, big.mark = "."), ")"),
    subtitle = paste0("N = ", format(n_conf, big.mark = ".")),
    x        = "Semana Epidemiológica",
    y        = "Casos Confirmados",
    caption  = texto_rodape
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g08)
salvar_grafico(g08, "08_confirmados_semana_epi")


# ==============================================================================
# BLOCO 9 — INCIDÊNCIA POR MUNICÍPIO (por 100.000 hab. — IBGE 2025)
# Gerado apenas quando MUNICIPIO_ANALISE == NULL (análise da regional inteira)
# ==============================================================================

if (is.null(MUNICIPIO_ANALISE) || !nzchar(trimws(MUNICIPIO_ANALISE))) {
  
  notif_por_mun <- base_filtrada %>%
    group_by(CO_MUN_RES) %>%
    summarise(total_casos = n(), .groups = "drop") %>%
    mutate(codigo_ibge = as.integer(as.character(CO_MUN_RES)))
  
  incidencia_mun <- ibge_15rs %>%
    left_join(notif_por_mun, by = "codigo_ibge") %>%
    mutate(
      total_casos = replace_na(total_casos, 0),
      incidencia  = round(total_casos / populacao * 100000, 1)
    ) %>%
    arrange(desc(incidencia))
  
  n_mun <- sum(incidencia_mun$total_casos)
  
  g09 <- ggplot(incidencia_mun,
                aes(x = reorder(nome_municipio, incidencia), y = incidencia)) +
    geom_col(fill = "#1A6B3C") +
    geom_text(
      aes(label = paste0(incidencia, " /100k  (n=", total_casos, ")")),
      hjust = -0.05, size = 3
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, max(incidencia_mun$incidencia, na.rm = TRUE) * 1.6)
    ) +
    labs(
      title    = paste0("Taxa de Incidência de SRAG por Município — 15ª RS Maringá",
                        " (N = ", format(n_mun, big.mark = "."), ")"),
      subtitle = paste0(
        "Por 100.000 habitantes | Pop. IBGE 2025",
        " | Pop. total: ", format(POPULACAO_15RS_TOTAL, big.mark = "."),
        " | Ano(s): ", paste(anos_carregar, collapse = "/")
      ),
      x       = "Município",
      y       = "Incidência por 100.000 hab.",
      caption = texto_rodape
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(g09)
  salvar_grafico(g09, "09_incidencia_por_municipio")
  
  message("\n--- TABELA: Incidência por Município ---")
  print(
    incidencia_mun %>%
      select(nome_municipio, populacao, total_casos, incidencia) %>%
      rename(
        Município       = nome_municipio,
        `Pop. 2025`     = populacao,
        `Casos SRAG`    = total_casos,
        `Inc./100k hab` = incidencia
      )
  )
}


# ==============================================================================
# BLOCO 10 — NOTIFICAÇÕES POR REGIONAIS DE SAÚDE DO PARANÁ (comparativo)
# ==============================================================================

regionais_pr <- base_completa %>%
  filter(ANO_BASE %in% anos_carregar) %>%
  mutate(regional_pad = toupper(trimws(ID_REGIONA))) %>%
  filter(grepl("RS ", regional_pad)) %>%
  group_by(regional_pad) %>%
  summarise(total_notificacoes = n(), .groups = "drop")

n_reg <- sum(regionais_pr$total_notificacoes)

g10 <- ggplot(regionais_pr,
              aes(x = reorder(regional_pad, total_notificacoes),
                  y = total_notificacoes)) +
  geom_col(fill = "#0057A3") +
  geom_text(
    aes(label = format(total_notificacoes, big.mark = ".")),
    hjust = -0.1, size = 3, color = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(regionais_pr$total_notificacoes) * 1.2),
    labels = scales::comma
  ) +
  labs(
    title    = paste0("Notificações de SRAG por Regional de Saúde — Paraná",
                      " (N = ", format(n_reg, big.mark = "."), ")"),
    subtitle = paste(anos_carregar, collapse = "/"),
    x        = "Regional de Saúde",
    y        = "Total de Notificações",
    caption  = texto_rodape
  ) +
  theme_minimal()

print(g10)
salvar_grafico(g10, "10_notificacoes_regionais_pr")


# ==============================================================================
# BLOCO 11 — DISTRIBUIÇÃO POR SEXO
# ==============================================================================

casos_sexo <- base_filtrada %>%
  filter(CS_SEXO %in% c("F", "M")) %>%
  group_by(CS_SEXO) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(
    CS_SEXO = factor(CS_SEXO, levels = c("F", "M")),
    pct     = round(total / sum(total) * 100, 1),
    rotulo  = paste0(format(total, big.mark = "."), " (", pct, "%)")
  )

n_sexo <- sum(casos_sexo$total)

g11 <- ggplot(casos_sexo, aes(x = CS_SEXO, y = total, fill = CS_SEXO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = rotulo), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(
    values = c("F" = "#E91E8C", "M" = "#0057A3"),
    labels = c("F" = "Feminino", "M" = "Masculino"),
    name   = "Sexo"
  ) +
  scale_y_continuous(limits = c(0, max(casos_sexo$total) * 1.25)) +
  labs(
    title   = paste0("Distribuição por Sexo — ", escopo_titulo,
                     " (N = ", format(n_sexo, big.mark = "."), ")"),
    x = NULL, y = "Notificações", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g11)
salvar_grafico(g11, "11_distribuicao_sexo")


# ==============================================================================
# BLOCO 12 — CLASSIFICAÇÃO FINAL
# ==============================================================================

classificacao <- base_filtrada %>%
  mutate(
    CLASS_FIN = case_when(
      CLASSI_FIN == 1   ~ "SRAG por Influenza",
      CLASSI_FIN == 2   ~ "SRAG por Outro Vírus Respiratório",
      CLASSI_FIN == 3   ~ "SRAG por Outro Agente Etiológico",
      CLASSI_FIN == 4   ~ "SRAG Não Especificado",
      CLASSI_FIN == 5   ~ "SRAG por Covid-19",
      is.na(CLASSI_FIN) ~ "Em Investigação"
    )
  ) %>%
  group_by(CLASS_FIN) %>%
  summarise(total = n(), .groups = "drop")

n_class <- sum(classificacao$total)

g12 <- ggplot(classificacao,
              aes(x = reorder(CLASS_FIN, total), y = total)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), hjust = -0.1, size = 4, color = "black") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(classificacao$total) * 1.2)) +
  labs(
    title   = paste0("Classificação Final — ", escopo_titulo,
                     " (N = ", format(n_class, big.mark = "."), ")"),
    x = "Classificação", y = "Total de Notificações", caption = texto_rodape
  ) +
  theme_minimal()

print(g12)
salvar_grafico(g12, "12_classificacao_final")


# ==============================================================================
# BLOCO 13 — CIRCULAÇÃO VIRAL (TOTAL)
# ==============================================================================

virus_total <- base_filtrada %>%
  select(SEM_NOT, POS_PCRFLU, PCR_SARS2, PCR_VSR,
         PCR_PARA1, PCR_PARA2, PCR_PARA3, PCR_PARA4,
         PCR_ADENO, PCR_METAP, PCR_BOCA, PCR_RINO) %>%
  pivot_longer(cols = -SEM_NOT, names_to = "virus", values_to = "resultado") %>%
  filter(resultado == 1) %>%
  mutate(
    virus = dplyr::recode(
      virus,
      POS_PCRFLU = "Influenza",       PCR_SARS2  = "Covid-19",
      PCR_VSR    = "VSR",             PCR_PARA1  = "Parainfluenza 1",
      PCR_PARA2  = "Parainfluenza 2", PCR_PARA3  = "Parainfluenza 3",
      PCR_PARA4  = "Parainfluenza 4", PCR_ADENO  = "Adenovírus",
      PCR_METAP  = "Metapneumovírus", PCR_BOCA   = "Bocavírus",
      PCR_RINO   = "Rinovírus"
    )
  ) %>%
  group_by(virus) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))

n_virus <- sum(virus_total$total)

g13 <- ggplot(virus_total, aes(x = reorder(virus, total), y = total)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total), hjust = -0.1, color = "black", size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(virus_total$total) * 1.2)) +
  labs(
    title   = paste0("Vírus Identificados por RT-PCR — ", escopo_titulo,
                     " (N = ", format(n_virus, big.mark = "."), ")"),
    x = "Vírus", y = "Casos Positivos", caption = texto_rodape
  ) +
  theme_minimal()

print(g13)
salvar_grafico(g13, "13_circulacao_viral_total")


# ==============================================================================
# BLOCO 14 — TENDÊNCIA VIRAL SEMANAL
# ==============================================================================

virus_semana <- base_filtrada %>%
  select(SEM_NOT, POS_PCRFLU, PCR_SARS2, PCR_VSR, PCR_RINO, PCR_ADENO, PCR_METAP) %>%
  pivot_longer(cols = -SEM_NOT, names_to = "virus", values_to = "resultado") %>%
  filter(resultado == 1) %>%
  mutate(
    virus = dplyr::recode(
      virus,
      POS_PCRFLU = "Influenza",   PCR_SARS2 = "Covid-19",
      PCR_VSR    = "VSR",         PCR_RINO  = "Rinovírus",
      PCR_ADENO  = "Adenovírus",  PCR_METAP = "Metapneumovírus"
    )
  ) %>%
  group_by(SEM_NOT, virus) %>%
  summarise(total = n(), .groups = "drop")

n_virus_sem <- sum(virus_semana$total)

g14 <- ggplot(virus_semana, aes(x = SEM_NOT, y = total, color = virus, group = virus)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  labs(
    title    = paste0("Tendência Semanal de Vírus Respiratórios — ", escopo_titulo,
                      " (N = ", format(n_virus_sem, big.mark = "."), ")"),
    subtitle = "Influenza, Covid-19, VSR, Rinovírus, Adenovírus, Metapneumovírus",
    x        = "Semana Epidemiológica",
    y        = "Casos Positivos",
    color    = "Vírus",
    caption  = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g14)
salvar_grafico(g14, "14_tendencia_viral_semanal")


# ==============================================================================
# BLOCO 15 — TIPOS E LINHAGENS DE INFLUENZA
# ==============================================================================

influenza_graf <- base_filtrada %>%
  filter(POS_PCRFLU == 1) %>%
  mutate(
    classificacao = case_when(
      TP_FLU_PCR == 1 & PCR_FLUASU == 1 ~ "Influenza A(H1N1)pdm09",
      TP_FLU_PCR == 1 & PCR_FLUASU == 2 ~ "Influenza A(H3N2)",
      TP_FLU_PCR == 1 & PCR_FLUASU == 3 ~ "Influenza A não subtipado",
      TP_FLU_PCR == 1 & PCR_FLUASU == 4 ~ "Influenza A não subtipável",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 1 ~ "Influenza B – Victoria",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 2 ~ "Influenza B – Yamagata",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 3 ~ "Influenza B – Não realizado",
      TP_FLU_PCR == 2 & PCR_FLUBLI == 4 ~ "Influenza B – Inconclusivo",
      TRUE ~ "Ignorado / Não classificado"
    )
  ) %>%
  group_by(classificacao) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))

if (nrow(influenza_graf) > 0) {
  n_flu <- sum(influenza_graf$total)
  
  g15 <- ggplot(influenza_graf, aes(x = reorder(classificacao, total), y = total)) +
    geom_col(fill = "#0057A3") +
    geom_text(aes(label = total), hjust = -0.1, color = "black", size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(influenza_graf$total) * 1.2)) +
    labs(
      title   = paste0("Tipos e Linhagens de Influenza (RT-PCR) — ", escopo_titulo,
                       " (N = ", format(n_flu, big.mark = "."), ")"),
      x = "Classificação", y = "Total de Casos", caption = texto_rodape
    ) +
    theme_minimal()
  
  print(g15)
  salvar_grafico(g15, "15_tipos_linhagens_influenza")
}


# ==============================================================================
# BLOCO 16 — FAIXA ETÁRIA (NOTIFICADOS vs CONFIRMADOS)
# ==============================================================================

base_faixa <- base_filtrada %>% criar_faixa_etaria()

tabela_notif <- base_faixa %>%
  count(faixa_etaria, name = "total") %>%
  mutate(tipo = "Notificados")

tabela_conf <- base_faixa %>%
  filter(CLASSI_FIN %in% c(1, 2, 3, 5)) %>%
  count(faixa_etaria, name = "total") %>%
  mutate(tipo = "Confirmados")

faixa_long <- bind_rows(tabela_notif, tabela_conf) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS))

n_notif16 <- sum(tabela_notif$total)
n_conf16  <- sum(tabela_conf$total)

g16 <- ggplot(faixa_long, aes(x = faixa_etaria, y = total, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = total),
    position = position_dodge(width = 0.8),
    hjust = 1.1, colour = "white", size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values = c("Notificados" = "#0057A3", "Confirmados" = "#27AE60")) +
  labs(
    title   = paste0("Faixa Etária: Notificados vs Confirmados — ", escopo_titulo,
                     " (Notif.: ", format(n_notif16, big.mark = "."),
                     " | Conf.: ", format(n_conf16, big.mark = "."), ")"),
    x = "Faixa Etária", y = "Quantidade", fill = "Status", caption = texto_rodape
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(g16)
salvar_grafico(g16, "16_faixa_etaria_notif_confirmados")


# ==============================================================================
# BLOCO 17 — PIRÂMIDE ETÁRIA (NOTIFICADOS)
# ==============================================================================

piramide_notif <- base_filtrada %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado") %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS),
    value        = ifelse(sexo == "Masculino", -n, n)
  )

n_piramide_notif <- sum(piramide_notif$n)

g17 <- ggplot(piramide_notif, aes(x = faixa_etaria, y = value, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(
    aes(label = n, hjust = ifelse(sexo == "Masculino", 1.15, -0.15)),
    size = 3.5
  ) +
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

print(g17)
salvar_grafico(g17, "17_piramide_etaria_notificados")


# ==============================================================================
# BLOCO 18 — PIRÂMIDE ETÁRIA (ÓBITOS — EVOLUCAO == 2)
# ==============================================================================

piramide_obitos <- base_filtrada %>%
  filter(EVOLUCAO == 2) %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado") %>%
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
    geom_text(
      aes(label = n, hjust = ifelse(sexo == "Masculino", 1.15, -0.15)),
      size = 3.5
    ) +
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
  
  print(g18)
  salvar_grafico(g18, "18_piramide_etaria_obitos")
} else {
  message("Sem óbitos registrados no escopo selecionado.")
}


# ==============================================================================
# BLOCO 19 — EVOLUÇÃO DOS CASOS (DESFECHO)
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
  summarise(total_casos = n(), .groups = "drop")

n_evol <- sum(casos_evolucao$total_casos)

g19 <- ggplot(casos_evolucao,
              aes(x = reorder(evolucao_label, total_casos), y = total_casos)) +
  geom_col(fill = "#A30000") +
  geom_text(aes(label = total_casos), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(casos_evolucao$total_casos) * 1.2)) +
  labs(
    title   = paste0("Casos por Evolução (Desfecho) — ", escopo_titulo,
                     " (N = ", format(n_evol, big.mark = "."), ")"),
    x = "Evolução", y = "Número de Casos", caption = texto_rodape
  ) +
  theme_minimal()

print(g19)
salvar_grafico(g19, "19_evolucao_desfecho")


# ==============================================================================
# BLOCO 20 — RAÇA / COR
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
  summarise(total_casos = n(), .groups = "drop")

n_raca <- sum(casos_raca$total_casos)

g20 <- ggplot(casos_raca,
              aes(x = reorder(raca_label, total_casos), y = total_casos)) +
  geom_col(fill = "#0057A3") +
  geom_text(aes(label = total_casos), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(casos_raca$total_casos) * 1.2)) +
  labs(
    title   = paste0("Casos por Raça/Cor — ", escopo_titulo,
                     " (N = ", format(n_raca, big.mark = "."), ")"),
    x = "Raça/Cor", y = "Número de Casos", caption = texto_rodape
  ) +
  theme_minimal()

print(g20)
salvar_grafico(g20, "20_raca_cor")


# ==============================================================================
# BLOCO 21 — ESCOLARIDADE (> 18 ANOS)
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
  summarise(total_casos = n(), .groups = "drop")

n_escol <- sum(casos_escol$total_casos)

g21 <- ggplot(casos_escol,
              aes(x = reorder(escolaridade_label, total_casos), y = total_casos)) +
  geom_col(fill = "#FF8C00") +
  geom_text(aes(label = total_casos), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(casos_escol$total_casos) * 1.2)) +
  labs(
    title   = paste0("Casos por Escolaridade (> 18 anos) — ", escopo_titulo,
                     " (N = ", format(n_escol, big.mark = "."), ")"),
    x = "Escolaridade", y = "Número de Casos", caption = texto_rodape
  ) +
  theme_minimal()

print(g21)
salvar_grafico(g21, "21_escolaridade")


# ==============================================================================
# BLOCO 22 — RESUMO FINAL (CONSOLE)
# ==============================================================================

n_obitos   <- sum(base_filtrada$EVOLUCAO == 2, na.rm = TRUE)
n_pcr_pos  <- sum(base_filtrada$PCR_RESUL == 1, na.rm = TRUE)
letalidade <- round(n_obitos / nrow(base_filtrada) * 100, 2)

message("\n", strrep("=", 60))
message("RESUMO DA ANÁLISE")
message(strrep("=", 60))
message("Escopo              : ", escopo_titulo)
message("Ano(s) analisados   : ", paste(anos_carregar, collapse = ", "))
message("Curva comparativa   : ", paste(anos_curva,    collapse = ", "))
message("Pop. IBGE 2025      : ", format(POPULACAO_ESCOPO, big.mark = "."))
message("Total notificações  : ", format(nrow(base_filtrada), big.mark = "."))
message("Tx notif. /100k hab.: ", round(nrow(base_filtrada) / POPULACAO_ESCOPO * 100000, 1))
message("Confirmados PCR     : ", format(n_pcr_pos, big.mark = "."))
message("Óbitos (EVOLUCAO=2) : ", format(n_obitos, big.mark = "."))
message("Letalidade          : ", letalidade, "%")
message(strrep("=", 60))
message("Análise concluída.")