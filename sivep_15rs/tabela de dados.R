# ==============================================================================
# EXPORTAÇÃO DE TABELAS — SIVEP-GRIPE — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# ==============================================================================
# Autor       : Valentim Sala Junior
# Descrição   : Gera um arquivo Excel com uma aba por tabela analítica.
#               Execute APÓS o script principal de gráficos, ou de forma
#               independente (os blocos 0–5 são replicados aqui).
#               O arquivo gerado pode ser carregado no Claude para análise
#               e redação do boletim epidemiológico.
# ==============================================================================


# ==============================================================================
# BLOCO 0 — CONFIGURAÇÃO (deve ser idêntica ao script de gráficos)
# ==============================================================================

ANO_ANALISE       <- 2026
MUNICIPIO_ANALISE <- NULL

DIRETORIO_DBF <- "C:/SIVEPGRIPE/BaseDBF"
ARQUIVO_IBGE  <- "C:/Users/valentim.junior/OneDrive/Área de Trabalho/sivep_15rs/ibge_cnv_pop.csv"

# Saída: mesmo diretório dos gráficos
DIR_GRAFICOS  <- "C:/SIVEPGRIPE/Graficos_boletim"
ARQUIVO_EXCEL <- file.path(
  DIR_GRAFICOS,
  paste0("tabelas_boletim_", paste(ANO_ANALISE, collapse = "_"), ".xlsx")
)


# ==============================================================================
# BLOCO 1 — PACOTES
# ==============================================================================

pkgs <- c("sf", "foreign", "dplyr", "tidyr", "readr", "openxlsx")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(sf); library(foreign); library(dplyr)
library(tidyr); library(readr); library(openxlsx)


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
         !grepl("^Total",  municipio_raw, ignore.case = TRUE),
         !grepl("^\\s*$",  municipio_raw)) %>%
  mutate(
    codigo_ibge    = as.integer(trimws(substr(municipio_raw, 1, 6))),
    nome_municipio = trimws(substr(municipio_raw, 8, nchar(municipio_raw))),
    populacao      = as.integer(populacao)
  ) %>%
  select(codigo_ibge, nome_municipio, populacao) %>%
  filter(!is.na(codigo_ibge))

POPULACAO_15RS_TOTAL <- sum(ibge_15rs$populacao)


# ==============================================================================
# BLOCO 3 — FUNÇÕES AUXILIARES
# ==============================================================================

carregar_base <- function(ano, diretorio) {
  nomes <- c(paste0("SRAGHOSP", ano, ".dbf"),
             paste0("sraghosp", ano, ".dbf"),
             paste0("SRAGHOSP", ano, ".DBF"))
  caminho <- NULL
  for (n in nomes) { c_ <- file.path(diretorio, n); if (file.exists(c_)) { caminho <- c_; break } }
  if (is.null(caminho)) { message("  [aviso] Não encontrado: ano ", ano); return(NULL) }
  base <- tryCatch({
    df <- foreign::read.dbf(caminho, as.is = TRUE)
    message("  [OK] ", caminho, " | ", nrow(df), " registros"); df
  }, error = function(e) NULL)
  if (is.null(base)) {
    base <- tryCatch({
      df <- sf::st_read(caminho, stringsAsFactors = FALSE, quiet = TRUE)
      if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      message("  [OK-sf] ", caminho, " | ", nrow(df), " registros"); df
    }, error = function(e) { message("  [ERRO] ", e$message); NULL })
  }
  if (!is.null(base)) base$ANO_BASE <- ano
  base
}

aplicar_filtros <- function(base, municipio = NULL) {
  r <- base %>%
    filter(grepl("15RS",    ID_REGIONA, ignore.case = TRUE),
           grepl("MARINGA", ID_REGIONA, ignore.case = TRUE))
  if (!is.null(municipio) && nzchar(trimws(municipio))) {
    mu  <- toupper(trimws(municipio))
    cod <- ibge_15rs %>% filter(toupper(nome_municipio) == mu) %>% pull(codigo_ibge)
    if (length(cod) > 0 && "CO_MUN_RES" %in% names(r))
      r <- r %>% filter(as.integer(CO_MUN_RES) == cod[1])
    else
      r <- r %>% filter(grepl(mu, toupper(ID_MUNICIP), fixed = TRUE))
  }
  r
}

populacao_escopo <- function(municipio = NULL) {
  if (is.null(municipio) || !nzchar(trimws(municipio))) return(POPULACAO_15RS_TOTAL)
  mu  <- toupper(trimws(municipio))
  pop <- ibge_15rs %>% filter(toupper(nome_municipio) == mu) %>% pull(populacao)
  if (length(pop) == 0) return(POPULACAO_15RS_TOTAL)
  pop[1]
}

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

padronizar_sexo <- function(df) {
  df %>% mutate(sexo = case_when(
    CS_SEXO %in% c("M","m","1",1) ~ "Masculino",
    CS_SEXO %in% c("F","f","2",2) ~ "Feminino",
    TRUE                          ~ "Ignorado"
  ))
}

ORDEM_FAIXAS <- c(
  "0-6 meses","6-11 meses","1-4 anos","5-9 anos","10-14 anos",
  "15-19 anos","20-29 anos","30-39 anos","40-49 anos","50-59 anos",
  "60 anos e mais","Em branco/Ignorado","Erro/Outro"
)


# ==============================================================================
# BLOCO 4 — CARREGAMENTO
# ==============================================================================

anos_disponiveis <- 2019:2026

anos_carregar <- if (!is.null(ANO_ANALISE)) intersect(ANO_ANALISE, anos_disponiveis) else anos_disponiveis

anos_curva <- if (is.null(ANO_ANALISE)) anos_disponiveis else
  if (length(ANO_ANALISE) == 1) intersect(seq(ANO_ANALISE - 2, ANO_ANALISE), anos_disponiveis) else
    intersect(ANO_ANALISE, anos_disponiveis)

anos_a_carregar <- sort(union(anos_carregar, anos_curva))

lista_bases <- Filter(Negate(is.null),
                      lapply(anos_a_carregar, carregar_base, diretorio = DIRETORIO_DBF))

if (length(lista_bases) == 0) stop("[ERRO] Nenhuma base carregada.")

base_completa <- dplyr::bind_rows(lista_bases)
message("Registros carregados: ", format(nrow(base_completa), big.mark = "."))


# ==============================================================================
# BLOCO 5 — FILTROS
# ==============================================================================

base_ano_principal <- base_completa %>% filter(ANO_BASE %in% anos_carregar)
base_filtrada      <- aplicar_filtros(base_ano_principal, municipio = MUNICIPIO_ANALISE)
POPULACAO_ESCOPO   <- populacao_escopo(MUNICIPIO_ANALISE)

if (nrow(base_filtrada) == 0) stop("Nenhum registro após filtros.")

escopo_label <- if (!is.null(MUNICIPIO_ANALISE) && nzchar(MUNICIPIO_ANALISE))
  paste0(tools::toTitleCase(tolower(MUNICIPIO_ANALISE)), " — 15ª RS Maringá") else
    "15ª Regional de Saúde de Maringá"

message("Escopo    : ", escopo_label)
message("Registros : ", format(nrow(base_filtrada), big.mark = "."))

# Base para curva (inclui anos de contexto)
base_curva_rs <- base_completa %>%
  filter(ANO_BASE %in% anos_curva) %>%
  aplicar_filtros(municipio = MUNICIPIO_ANALISE)


# ==============================================================================
# BLOCO 6 — CONSTRUÇÃO DAS TABELAS
# ==============================================================================

# --- T01: Resumo geral ---
n_total    <- nrow(base_filtrada)
n_obitos   <- sum(base_filtrada$EVOLUCAO == 2, na.rm = TRUE)
n_pcr_pos  <- sum(base_filtrada$PCR_RESUL == 1, na.rm = TRUE)
letalidade <- round(n_obitos / n_total * 100, 2)
tx_notif   <- round(n_total / POPULACAO_ESCOPO * 100000, 1)

t01_resumo <- tibble::tibble(
  Indicador = c(
    "Escopo",
    "Ano(s) analisados",
    "Anos da curva comparativa",
    "População IBGE 2025",
    "Total de notificações",
    "Taxa de notificação (por 100.000 hab.)",
    "Confirmados por PCR",
    "Óbitos (EVOLUCAO = 2)",
    "Letalidade (%)"
  ),
  Valor = c(
    escopo_label,
    paste(anos_carregar, collapse = ", "),
    paste(anos_curva, collapse = ", "),
    format(POPULACAO_ESCOPO, big.mark = "."),
    format(n_total, big.mark = "."),
    as.character(tx_notif),
    format(n_pcr_pos, big.mark = "."),
    format(n_obitos, big.mark = "."),
    as.character(letalidade)
  )
)


# --- T02: Curva epidêmica comparativa (casos por semana × ano) ---
t02_curva_base <- base_curva_rs %>%
  mutate(
    DT_NOTIFIC_DATE = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    Ano             = format(DT_NOTIFIC_DATE, "%Y"),
    Semana_Epi      = as.integer(format(DT_NOTIFIC_DATE, "%V"))
  ) %>%
  filter(!is.na(DT_NOTIFIC_DATE)) %>%
  group_by(Ano, Semana_Epi) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  arrange(Ano, Semana_Epi)

t02_total_ano <- t02_curva_base %>%
  group_by(Ano) %>%
  summarise(`Total no Ano` = sum(Casos), .groups = "drop")

t02_curva <- t02_curva_base %>%
  left_join(t02_total_ano, by = "Ano") %>%
  rename(`Semana Epidemiológica` = Semana_Epi)


# --- T03: Notificações por semana (ano principal) ---
t03_semana <- base_filtrada %>%
  group_by(`Semana Epi` = SEM_NOT) %>%
  summarise(Notificações = n(), .groups = "drop") %>%
  mutate(
    `% do Total` = round(Notificações / sum(Notificações) * 100, 1)
  ) %>%
  arrange(`Semana Epi`)


# --- T04: Casos confirmados por semana (PCR_RESUL == 1) ---
t04_confirmados_semana <- base_filtrada %>%
  filter(PCR_RESUL == 1) %>%
  group_by(`Semana Epi` = SEM_NOT) %>%
  summarise(`Confirmados PCR` = n(), .groups = "drop") %>%
  arrange(`Semana Epi`)


# --- T05: Incidência por município ---
notif_mun <- base_filtrada %>%
  group_by(CO_MUN_RES) %>%
  summarise(total_casos = n(), .groups = "drop") %>%
  mutate(codigo_ibge = as.integer(as.character(CO_MUN_RES)))

t05_incidencia_mun <- ibge_15rs %>%
  left_join(notif_mun, by = "codigo_ibge") %>%
  mutate(
    total_casos = replace_na(total_casos, 0),
    `Inc. /100k hab.` = round(total_casos / populacao * 100000, 1)
  ) %>%
  arrange(desc(`Inc. /100k hab.`)) %>%
  select(
    Município       = nome_municipio,
    `Cód. IBGE`     = codigo_ibge,
    `Pop. 2025`     = populacao,
    `Casos SRAG`    = total_casos,
    `Inc. /100k hab.`
  )


# --- T06: Notificações por regional de saúde do Paraná ---
t06_regionais <- base_completa %>%
  filter(ANO_BASE %in% anos_carregar) %>%
  mutate(Regional = toupper(trimws(ID_REGIONA))) %>%
  filter(grepl("RS ", Regional)) %>%
  group_by(Regional) %>%
  summarise(Notificações = n(), .groups = "drop") %>%
  arrange(desc(Notificações)) %>%
  mutate(`% do Total` = round(Notificações / sum(Notificações) * 100, 1))


# --- T07: Distribuição por sexo ---
t07_sexo <- base_filtrada %>%
  filter(CS_SEXO %in% c("F","M")) %>%
  mutate(Sexo = ifelse(CS_SEXO == "F", "Feminino", "Masculino")) %>%
  group_by(Sexo) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1))


# --- T08: Classificação final ---
t08_classificacao <- base_filtrada %>%
  mutate(Classificação = case_when(
    CLASSI_FIN == 1   ~ "SRAG por Influenza",
    CLASSI_FIN == 2   ~ "SRAG por Outro Vírus Respiratório",
    CLASSI_FIN == 3   ~ "SRAG por Outro Agente Etiológico",
    CLASSI_FIN == 4   ~ "SRAG Não Especificado",
    CLASSI_FIN == 5   ~ "SRAG por Covid-19",
    is.na(CLASSI_FIN) ~ "Em Investigação"
  )) %>%
  group_by(Classificação) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1)) %>%
  arrange(desc(Casos))


# --- T09: Circulação viral total (RT-PCR) ---
t09_viral <- base_filtrada %>%
  select(POS_PCRFLU, PCR_SARS2, PCR_VSR,
         PCR_PARA1, PCR_PARA2, PCR_PARA3, PCR_PARA4,
         PCR_ADENO, PCR_METAP, PCR_BOCA, PCR_RINO) %>%
  pivot_longer(everything(), names_to = "campo", values_to = "resultado") %>%
  filter(resultado == 1) %>%
  mutate(Vírus = dplyr::recode(campo,
                               POS_PCRFLU = "Influenza",       PCR_SARS2  = "Covid-19",
                               PCR_VSR    = "VSR",             PCR_PARA1  = "Parainfluenza 1",
                               PCR_PARA2  = "Parainfluenza 2", PCR_PARA3  = "Parainfluenza 3",
                               PCR_PARA4  = "Parainfluenza 4", PCR_ADENO  = "Adenovírus",
                               PCR_METAP  = "Metapneumovírus", PCR_BOCA   = "Bocavírus",
                               PCR_RINO   = "Rinovírus"
  )) %>%
  group_by(Vírus) %>%
  summarise(`Positivos` = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Positivos / sum(Positivos) * 100, 1)) %>%
  arrange(desc(Positivos))


# --- T10: Tendência semanal dos principais vírus ---
t10_viral_semana <- base_filtrada %>%
  select(SEM_NOT, POS_PCRFLU, PCR_SARS2, PCR_VSR, PCR_RINO, PCR_ADENO, PCR_METAP) %>%
  pivot_longer(cols = -SEM_NOT, names_to = "campo", values_to = "resultado") %>%
  filter(resultado == 1) %>%
  mutate(Vírus = dplyr::recode(campo,
                               POS_PCRFLU = "Influenza",   PCR_SARS2 = "Covid-19",
                               PCR_VSR    = "VSR",         PCR_RINO  = "Rinovírus",
                               PCR_ADENO  = "Adenovírus",  PCR_METAP = "Metapneumovírus"
  )) %>%
  group_by(`Semana Epi` = SEM_NOT, Vírus) %>%
  summarise(Positivos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Vírus, values_from = Positivos, values_fill = 0) %>%
  arrange(`Semana Epi`)


# --- T11: Tipos e linhagens de Influenza ---
t11_influenza <- base_filtrada %>%
  filter(POS_PCRFLU == 1) %>%
  mutate(Classificação = case_when(
    TP_FLU_PCR == 1 & PCR_FLUASU == 1 ~ "Influenza A(H1N1)pdm09",
    TP_FLU_PCR == 1 & PCR_FLUASU == 2 ~ "Influenza A(H3N2)",
    TP_FLU_PCR == 1 & PCR_FLUASU == 3 ~ "Influenza A não subtipado",
    TP_FLU_PCR == 1 & PCR_FLUASU == 4 ~ "Influenza A não subtipável",
    TP_FLU_PCR == 2 & PCR_FLUBLI == 1 ~ "Influenza B – Victoria",
    TP_FLU_PCR == 2 & PCR_FLUBLI == 2 ~ "Influenza B – Yamagata",
    TP_FLU_PCR == 2 & PCR_FLUBLI == 3 ~ "Influenza B – Não realizado",
    TP_FLU_PCR == 2 & PCR_FLUBLI == 4 ~ "Influenza B – Inconclusivo",
    TRUE ~ "Ignorado / Não classificado"
  )) %>%
  group_by(Classificação) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1)) %>%
  arrange(desc(Casos))


# --- T12: Faixa etária — notificados e confirmados ---
base_faixa <- base_filtrada %>% criar_faixa_etaria()

t12_faixa <- base_faixa %>%
  count(faixa_etaria, name = "Notificados") %>%
  left_join(
    base_faixa %>%
      filter(CLASSI_FIN %in% c(1,2,3,5)) %>%
      count(faixa_etaria, name = "Confirmados"),
    by = "faixa_etaria"
  ) %>%
  mutate(
    Confirmados     = replace_na(Confirmados, 0),
    `% Notificados` = round(Notificados  / sum(Notificados)  * 100, 1),
    `% Confirmados` = round(Confirmados  / sum(Confirmados)  * 100, 1),
    faixa_etaria    = factor(faixa_etaria, levels = ORDEM_FAIXAS)
  ) %>%
  arrange(faixa_etaria) %>%
  rename(`Faixa Etária` = faixa_etaria)


# --- T13: Pirâmide etária — notificados por sexo e faixa ---
t13_piramide_notif <- base_filtrada %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado") %>%
  group_by(`Faixa Etária` = faixa_etaria, Sexo = sexo) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`Faixa Etária` = factor(`Faixa Etária`, levels = ORDEM_FAIXAS)) %>%
  arrange(`Faixa Etária`, Sexo)


# --- T14: Pirâmide etária — óbitos por sexo e faixa ---
t14_piramide_obitos <- base_filtrada %>%
  filter(EVOLUCAO == 2) %>%
  criar_faixa_etaria() %>%
  padronizar_sexo() %>%
  filter(sexo != "Ignorado") %>%
  group_by(`Faixa Etária` = faixa_etaria, Sexo = sexo) %>%
  summarise(Óbitos = n(), .groups = "drop") %>%
  mutate(`Faixa Etária` = factor(`Faixa Etária`, levels = ORDEM_FAIXAS)) %>%
  arrange(`Faixa Etária`, Sexo)


# --- T15: Desfecho / evolução dos casos ---
t15_evolucao <- base_filtrada %>%
  mutate(Evolução = case_when(
    EVOLUCAO == 1 ~ "Cura",
    EVOLUCAO == 2 ~ "Óbito por SRAG",
    EVOLUCAO == 3 ~ "Óbito por Outras Causas",
    EVOLUCAO == 9 ~ "Ignorado",
    TRUE          ~ "Em investigação"
  )) %>%
  group_by(Evolução) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1)) %>%
  arrange(desc(Casos))


# --- T16: Raça / cor ---
t16_raca <- base_filtrada %>%
  mutate(`Raça/Cor` = case_when(
    CS_RACA == 1 ~ "Branca",
    CS_RACA == 2 ~ "Preta",
    CS_RACA == 3 ~ "Amarela",
    CS_RACA == 4 ~ "Parda",
    CS_RACA == 5 ~ "Indígena",
    TRUE         ~ "Ignorado/Outros"
  )) %>%
  group_by(`Raça/Cor`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1)) %>%
  arrange(desc(Casos))


# --- T17: Escolaridade (> 18 anos) ---
t17_escolaridade <- base_filtrada %>%
  filter(NU_IDADE_N > 18) %>%
  mutate(Escolaridade = case_when(
    CS_ESCOL_N == "0" ~ "Analfabeto",
    CS_ESCOL_N == "1" ~ "Fund. 1º Ciclo (1ª–5ª série)",
    CS_ESCOL_N == "2" ~ "Fund. 2º Ciclo (6ª–9ª série)",
    CS_ESCOL_N == "3" ~ "Ensino Médio",
    CS_ESCOL_N == "4" ~ "Ensino Superior",
    CS_ESCOL_N == "5" ~ "Não se aplica",
    CS_ESCOL_N == "9" ~ "Ignorado",
    TRUE              ~ "Não Registrado"
  )) %>%
  group_by(Escolaridade) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(`% do Total` = round(Casos / sum(Casos) * 100, 1)) %>%
  arrange(desc(Casos))


# --- T18: Comparativo anual — totais e indicadores por ano ---
t18_comparativo_anual <- base_completa %>%
  filter(ANO_BASE %in% anos_curva) %>%
  aplicar_filtros(municipio = MUNICIPIO_ANALISE) %>%
  group_by(Ano = ANO_BASE) %>%
  summarise(
    Notificações       = n(),
    `Confirmados PCR`  = sum(PCR_RESUL  == 1, na.rm = TRUE),
    Óbitos             = sum(EVOLUCAO   == 2, na.rm = TRUE),
    `Influenza+`       = sum(POS_PCRFLU == 1, na.rm = TRUE),
    `Covid-19+`        = sum(PCR_SARS2  == 1, na.rm = TRUE),
    `VSR+`             = sum(PCR_VSR    == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Letalidade (%)`        = round(Óbitos       / Notificações * 100, 2),
    `Taxa Notif./100k`      = round(Notificações / POPULACAO_ESCOPO * 100000, 1),
    `% Confirmados`         = round(`Confirmados PCR` / Notificações * 100, 1)
  ) %>%
  arrange(Ano)


# ==============================================================================
# BLOCO 7 — EXPORTAÇÃO PARA EXCEL (uma aba por tabela)
# ==============================================================================

if (!dir.exists(DIR_GRAFICOS)) dir.create(DIR_GRAFICOS, recursive = TRUE)

wb <- createWorkbook()

# Estilo de cabeçalho
estilo_cabecalho <- createStyle(
  fontName    = "Calibri",
  fontSize    = 11,
  fontColour  = "#FFFFFF",
  fgFill      = "#0057A3",
  halign      = "CENTER",
  textDecoration = "bold",
  border      = "TopBottomLeftRight",
  borderColour = "#FFFFFF"
)
estilo_celula <- createStyle(
  fontName = "Calibri",
  fontSize = 10,
  border   = "TopBottomLeftRight",
  borderColour = "#DDDDDD"
)
estilo_destaque <- createStyle(
  fontName = "Calibri",
  fontSize = 10,
  fgFill   = "#EAF3FB",
  border   = "TopBottomLeftRight",
  borderColour = "#DDDDDD"
)

adicionar_aba <- function(wb, nome_aba, df, destacar_linhas_pares = TRUE) {
  addWorksheet(wb, nome_aba)
  writeDataTable(wb, nome_aba, df, startRow = 1, startCol = 1,
                 tableStyle = "TableStyleMedium2", withFilter = FALSE)
  # Largura automática nas colunas
  setColWidths(wb, nome_aba, cols = seq_len(ncol(df)), widths = "auto")
  invisible(wb)
}

adicionar_aba(wb, "T01_Resumo_Geral",         t01_resumo)
adicionar_aba(wb, "T02_Curva_Comparativa",     t02_curva)
adicionar_aba(wb, "T03_Notif_Semana",          t03_semana)
adicionar_aba(wb, "T04_Confirmados_Semana",    t04_confirmados_semana)
adicionar_aba(wb, "T05_Incidencia_Municipio",  t05_incidencia_mun)
adicionar_aba(wb, "T06_Regionais_PR",          t06_regionais)
adicionar_aba(wb, "T07_Sexo",                  t07_sexo)
adicionar_aba(wb, "T08_Classificacao_Final",   t08_classificacao)
adicionar_aba(wb, "T09_Circulacao_Viral",      t09_viral)
adicionar_aba(wb, "T10_Tendencia_Viral_Sem",   t10_viral_semana)
adicionar_aba(wb, "T11_Influenza_Linhagens",   t11_influenza)
adicionar_aba(wb, "T12_Faixa_Etaria",          t12_faixa)
adicionar_aba(wb, "T13_Piramide_Notificados",  t13_piramide_notif)
adicionar_aba(wb, "T14_Piramide_Obitos",       t14_piramide_obitos)
adicionar_aba(wb, "T15_Evolucao_Desfecho",     t15_evolucao)
adicionar_aba(wb, "T16_Raca_Cor",              t16_raca)
adicionar_aba(wb, "T17_Escolaridade",          t17_escolaridade)
adicionar_aba(wb, "T18_Comparativo_Anual",     t18_comparativo_anual)

saveWorkbook(wb, ARQUIVO_EXCEL, overwrite = TRUE)

message("\n", strrep("=", 60))
message("Arquivo gerado com sucesso:")
message("  ", ARQUIVO_EXCEL)
message(strrep("=", 60))
message("Abas exportadas (", 18, "):")
message("  T01  Resumo geral — indicadores consolidados")
message("  T02  Curva epidêmica comparativa (por semana × ano)")
message("  T03  Notificações por semana epidemiológica")
message("  T04  Confirmados por PCR por semana")
message("  T05  Incidência por município (/100k hab.)")
message("  T06  Notificações por regional de saúde — Paraná")
message("  T07  Distribuição por sexo")
message("  T08  Classificação final")
message("  T09  Circulação viral total (RT-PCR)")
message("  T10  Tendência semanal dos principais vírus")
message("  T11  Tipos e linhagens de Influenza")
message("  T12  Faixa etária — notificados vs confirmados")
message("  T13  Pirâmide etária — notificados (por sexo)")
message("  T14  Pirâmide etária — óbitos (por sexo)")
message("  T15  Evolução / desfecho dos casos")
message("  T16  Raça / cor")
message("  T17  Escolaridade (> 18 anos)")
message("  T18  Comparativo anual — totais e indicadores")
message(strrep("=", 60))
message("Carregue o arquivo aqui para análise e redação do boletim.")