# =============================================================================
# INDICADORES SIM — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# Sistema de Informações sobre Mortalidade (SIM) / DATASUS
# Seção de Vigilância Epidemiológica — SCVGE | 15ª RS — Maringá/PR
# =============================================================================
# Eixos:
#   1. Perfil demográfico (sexo, faixa etária, raça, escolaridade)
#   2. Causas de morte (capítulos CID, top 20, externas, evitáveis)
#   3. Assistência e local de ocorrência
#   4. Mortalidade materna e fetal
#   5. Geográfico e temporal (municípios, série mensal)
#   6. Qualidade da informação (completude, oportunidade)
# Saída: Excel consolidado (17 abas) + mapa PNG
# =============================================================================

library(read.dbc)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(scales)
library(patchwork)
library(openxlsx)

# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÕES
# -----------------------------------------------------------------------------

ANO  <- 2025   # ano principal de análise
base <- "/Users/valentimsalajunior/Desktop/R_SIM"

caminho_dbc <- file.path(base, "SIM", "DBF", paste0("DOPR", ANO, ".dbc"))
caminho_shp <- file.path(base, "GIS", "Pr_Municipios_2024", "PR_Municipios_2024.shp")
dir_saida   <- file.path(base, "SIM", "outputs")
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Lookup CID-10 (gera objeto `cid10`)
source(file.path(base, "SIM", "DBF", "cid10_lookup.R"))

# -----------------------------------------------------------------------------
# 1. MUNICÍPIOS DA 15ª RS — lidos diretamente do CSV IBGE/DATASUS
# -----------------------------------------------------------------------------

caminho_pop <- file.path(base, "SIM", "ibge_cnv_pop.csv")
# Coloque o arquivo ibge_cnv_pop.csv em: R_SIM/SIM/

# Leitura do CSV (encoding DATASUS = latin1, separador ;, cabeçalho em linha 6)
pop_raw <- readLines(caminho_pop, encoding = "latin1", warn = FALSE)
pop_dados <- pop_raw[grepl("^\"[0-9]", pop_raw)]
pop_dados <- pop_dados[!grepl("Total", pop_dados)]

tab_pop <- do.call(rbind, lapply(pop_dados, function(linha) {
  linha  <- gsub("\r|\"", "", linha)
  partes <- strsplit(linha, ";")[[1]]
  cod_nome <- trimws(partes[1])
  codigo   <- trimws(substr(cod_nome, 1, 6))        # 6 dígitos DATASUS
  nome     <- trimws(substr(cod_nome, 8, nchar(cod_nome)))
  pop      <- as.numeric(gsub("[^0-9]", "", partes[2]))
  data.frame(codigo_datasus = codigo, nome_datasus = nome,
             populacao_2025 = pop, stringsAsFactors = FALSE)
}))

# Vetor de códigos DATASUS (6 dígitos) — usado para filtrar CODMUNRES no DBC
municipios_15rs_cod6 <- tab_pop$codigo_datasus

message("Municípios da 15ª RS carregados: ", nrow(tab_pop))
message("População total estimada 2025: ",
        format(sum(tab_pop$populacao_2025), big.mark = "."))

# -----------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES
# -----------------------------------------------------------------------------

# Decodifica campo IDADE (formato DATASUS: 1º dígito = unidade, 2 últimos = qty)
calcular_idade_anos <- function(idade_raw) {
  # IDADE é armazenado como factor no DBC — converter via as.character, não as.integer
  idade_str <- str_pad(as.character(idade_raw), 3, "left", "0")
  unid <- as.integer(substr(idade_str, 1, 1))
  qtd  <- as.integer(substr(idade_str, 2, 3))
  dplyr::case_when(
    unid == 4 ~ as.numeric(qtd),
    unid == 5 ~ as.numeric(100 + qtd),
    unid == 3 ~ 0,          # meses — menos de 1 ano
    unid %in% c(1, 2) ~ 0,  # horas/minutos — neonato
    TRUE ~ NA_real_
  )
}

faixa_etaria <- function(idade_anos) {
  dplyr::case_when(
    is.na(idade_anos)      ~ "Ignorada",
    idade_anos < 1         ~ "< 1 ano",
    idade_anos <= 4        ~ "1–4 anos",
    idade_anos <= 14       ~ "5–14 anos",
    idade_anos <= 29       ~ "15–29 anos",
    idade_anos <= 49       ~ "30–49 anos",
    idade_anos <= 59       ~ "50–59 anos",
    idade_anos <= 69       ~ "60–69 anos",
    idade_anos <= 79       ~ "70–79 anos",
    TRUE                   ~ "80 anos e mais"
  )
}

# Labels decodificados
label_sexo <- function(x) dplyr::case_when(
  x %in% c("M","1") ~ "Masculino",
  x %in% c("F","2") ~ "Feminino",
  TRUE ~ "Ignorado"
)

label_raca <- function(x) dplyr::case_when(
  x == "1" ~ "Branca",
  x == "2" ~ "Preta",
  x == "3" ~ "Amarela",
  x == "4" ~ "Parda",
  x == "5" ~ "Indígena",
  TRUE ~ "Ignorada"
)

label_esc <- function(x) dplyr::case_when(
  x == "00" ~ "Sem escolaridade",
  x == "01" ~ "Fund I incompleto",
  x == "02" ~ "Fund I completo",
  x == "03" ~ "Fund II incompleto",
  x == "04" ~ "Fund II completo",
  x == "05" ~ "Médio incompleto",
  x == "06" ~ "Médio completo",
  x == "07" ~ "Superior incompleto",
  x == "08" ~ "Superior completo",
  x == "09" ~ "Ignorado",
  TRUE ~ "Não informado"
)

label_lococor <- function(x) dplyr::case_when(
  x == "1" ~ "Hospital",
  x == "2" ~ "Outro estab. de saúde",
  x == "3" ~ "Domicílio",
  x == "4" ~ "Via pública",
  x == "5" ~ "Outros",
  x == "6" ~ "Aldeia indígena",
  x == "9" ~ "Ignorado",
  TRUE ~ "Não informado"
)

label_atestante <- function(x) dplyr::case_when(
  x == "1" ~ "Médico assistente",
  x == "2" ~ "Médico substituto",
  x == "3" ~ "IML",
  x == "4" ~ "SVO",
  x == "5" ~ "Outro",
  TRUE ~ "Não informado"
)

label_circobito <- function(x) dplyr::case_when(
  x == "1" ~ "Acidente",
  x == "2" ~ "Suicídio",
  x == "3" ~ "Homicídio",
  x == "4" ~ "Outros",
  x == "9" ~ "Ignorado",
  TRUE ~ "Não informado"
)

# Causas evitáveis — lista baseada em SVS/MS 2011/2021
# Prefixos de 3 dígitos considerados reduzíveis por ações de saúde
cid_evitaveis <- c(
  # Imunopreveníveis
  "A33","A35","A36","A37","A80","B05","B06","B16","B26","B91",
  # Infecções tratáveis
  "A00","A01","A02","A03","A04","A05","A06","A07","A08","A09",
  "A15","A16","A17","A18","A19","A27","A39","A40","A41",
  "B50","B51","B52","B53","B54","B57","B76",
  # Doenças nutricionais
  "E40","E41","E42","E43","E44","E45","E46","E50","D50","D51","D52","D53",
  # Doenças crônicas com manejo adequado
  "I10","I11","I12","I13","I20","I21","I22","I23","I24","I25",
  "I60","I61","I62","I63","I64","I65","I66","I67","I69",
  "J40","J41","J42","J43","J44","J45","J46",
  "E10","E11","E12","E13","E14","N18",
  # Causas maternas e perinatais
  "O00","O01","O02","O03","O04","O05","O06","O07","O08",
  "O10","O11","O12","O13","O14","O15","O16",
  "O20","O21","O22","O23","O24","O25","O26","O27","O28","O29",
  "P00","P01","P02","P03","P04","P05","P06","P07","P08",
  "P10","P11","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29"
)

# -----------------------------------------------------------------------------
# 3. LEITURA E PREPARAÇÃO DOS DADOS
# -----------------------------------------------------------------------------

message("\nLendo: ", caminho_dbc)
sim_raw <- read.dbc(caminho_dbc)
names(sim_raw) <- toupper(names(sim_raw))
message("Total de registros no arquivo: ", nrow(sim_raw))

# Campos necessários
campos_ok <- c("CAUSABAS","CODMUNRES","DTOBITO","TIPOBITO","SEXO",
               "RACACOR","ESCFALAGR1","LOCOCOR","ATESTANTE","NECROPSIA",
               "ASSISTMED","CIRCOBITO","ACIDTRAB","IDADE",
               "OBITOGRAV","OBITOPUERP","TIPOBITO","PESO","PARTO",
               "GRAVIDEZ","IDADEMAE","SEMAGESTAC","QTDFILVIVO","QTDFILMORT",
               "DIFDATA","TPPOS","ALTCAUSA","DTRECEBIM","EXAME","CIRURGIA")

ausentes <- setdiff(campos_ok, names(sim_raw))
if (length(ausentes) > 0)
  message("Campos ausentes (serão ignorados): ", paste(ausentes, collapse = ", "))

# Preparação base — todos os registros da 15ª RS
sim_base <- sim_raw %>%
  # CODMUNRES é factor com valores de 6 dígitos — converter direto para character
  mutate(CODMUNRES = as.character(CODMUNRES)) %>%
  filter(CODMUNRES %in% municipios_15rs_cod6) %>%
  mutate(
    CAUSABAS  = toupper(trimws(as.character(CAUSABAS))),
    codigo3   = substr(CAUSABAS, 1, 3),
    valido    = !is.na(CAUSABAS) & CAUSABAS != "" & CAUSABAS != "*",
    SEXO      = as.character(SEXO),
    RACACOR   = if ("RACACOR"    %in% names(.)) as.character(RACACOR)    else NA_character_,
    ESCFALAGR1= if ("ESCFALAGR1" %in% names(.)) as.character(ESCFALAGR1) else NA_character_,
    LOCOCOR   = if ("LOCOCOR"    %in% names(.)) as.character(LOCOCOR)    else NA_character_,
    ATESTANTE = if ("ATESTANTE"  %in% names(.)) as.character(ATESTANTE)  else NA_character_,
    NECROPSIA = if ("NECROPSIA"  %in% names(.)) as.character(NECROPSIA)  else NA_character_,
    ASSISTMED = if ("ASSISTMED"  %in% names(.)) as.character(ASSISTMED)  else NA_character_,
    CIRCOBITO = if ("CIRCOBITO"  %in% names(.)) as.character(CIRCOBITO)  else NA_character_,
    TIPOBITO  = as.character(TIPOBITO),
    CAUSABAS  = toupper(trimws(as.character(CAUSABAS))),
    codigo3   = substr(CAUSABAS, 1, 3),
    valido    = !is.na(CAUSABAS) & CAUSABAS != "" & CAUSABAS != "*",
    # IDADE: factor no DBC — usar as.character diretamente
    IDADE_str = str_pad(as.character(IDADE), 3, "left", "0"),
    # Datas
    mes_obito = as.integer(substr(as.character(DTOBITO), 3, 4)),
    ano_obito = as.integer(substr(as.character(DTOBITO), 5, 8)),
    mes_label = month.abb[mes_obito],
    # Idade em anos — IDADE_str já corrigido acima
    idade_anos = calcular_idade_anos(IDADE),
    faixa_et   = factor(faixa_etaria(idade_anos),
                        levels = c("< 1 ano","1–4 anos","5–14 anos",
                                   "15–29 anos","30–49 anos","50–59 anos",
                                   "60–69 anos","70–79 anos","80 anos e mais","Ignorada")),
    # Labels
    sexo_label     = label_sexo(SEXO),
    raca_label     = label_raca(RACACOR),
    esc_label      = label_esc(ESCFALAGR1),
    lococor_label  = label_lococor(LOCOCOR),
    atestante_label= label_atestante(ATESTANTE),
    circobito_label= label_circobito(CIRCOBITO)
  ) %>%
  left_join(
    cid10 %>% select(codigo, descricao, capitulo_num, capitulo_nome, grupo_mapa),
    by = c("codigo3" = "codigo")
  ) %>%
  mutate(
    capitulo_nome = if_else(is.na(capitulo_nome), "Não classificado", capitulo_nome),
    grupo_mapa    = if_else(is.na(grupo_mapa),    "Outras",           grupo_mapa),
    descricao     = if_else(is.na(descricao),     CAUSABAS,           descricao),
    evitavel      = codigo3 %in% cid_evitaveis
  )

message("Registros 15ª RS: ", nrow(sim_base))

# Diagnóstico — mostra quantos registros por município para confirmar o join
if (nrow(sim_base) == 0) {
  stop(
    "Nenhum registro encontrado para os municípios da 15ª RS.\n",
    "Verifique os códigos CODMUNRES no arquivo DBC versus o CSV de população.\n",
    "Exemplos no DBC: ", paste(head(unique(as.character(sim_raw$CODMUNRES)), 5), collapse = ", "), "\n",
    "Exemplos no CSV: ", paste(head(municipios_15rs_cod6, 5), collapse = ", ")
  )
}

cat("\nRegistros por município (top 10):\n")
print(sim_base %>% count(CODMUNRES, sort = TRUE) %>% head(10))

# Separar fetais dos não fetais
sim_nf <- sim_base %>% filter(TIPOBITO == "2", valido)   # não fetais
sim_ft <- sim_base %>% filter(TIPOBITO == "1")            # fetais

n_total <- nrow(sim_nf)
message("Óbitos não fetais com causa válida: ", n_total)

# Diagnóstico — distribuição de faixas etárias (confirma que IDADE foi lido corretamente)
cat("\nDistribuição por faixa etária:\n")
print(sim_nf %>% count(faixa_et, sort = FALSE))

# -----------------------------------------------------------------------------
# 4. CÁLCULO DOS INDICADORES POR EIXO
# -----------------------------------------------------------------------------

# ── EIXO 1: PERFIL DEMOGRÁFICO ────────────────────────────────────────────────

ind_sexo <- sim_nf %>%
  count(sexo_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_faixa <- sim_nf %>%
  count(faixa_et, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1))

ind_faixa_sexo <- sim_nf %>%
  filter(sexo_label != "Ignorado") %>%
  count(faixa_et, sexo_label, name = "n") %>%
  group_by(sexo_label) %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  ungroup()

ind_raca <- sim_nf %>%
  count(raca_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_esc <- sim_nf %>%
  filter(!ESCFALAGR1 %in% c("09","NA")) %>%
  count(esc_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

# ── EIXO 2: CAUSAS DE MORTE ──────────────────────────────────────────────────

ind_capitulos <- sim_nf %>%
  count(capitulo_num, capitulo_nome, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_top20 <- sim_nf %>%
  count(codigo3, descricao, grupo_mapa, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20)

ind_externas <- sim_nf %>%
  filter(grupo_mapa == "Causas externas") %>%
  count(circobito_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_externas_sexo <- sim_nf %>%
  filter(grupo_mapa == "Causas externas", sexo_label != "Ignorado") %>%
  count(circobito_label, sexo_label, name = "n") %>%
  arrange(desc(n))

ind_evitaveis <- sim_nf %>%
  summarise(
    total         = n(),
    n_evitavel    = sum(evitavel, na.rm = TRUE),
    prop_evitavel = round(n_evitavel / total * 100, 1)
  )

ind_evitaveis_cid <- sim_nf %>%
  filter(evitavel) %>%
  count(codigo3, descricao, grupo_mapa, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20)

ind_acidtrab <- if ("ACIDTRAB" %in% names(sim_nf)) {
  sim_nf %>%
    count(ACIDTRAB, name = "n") %>%
    mutate(
      categoria = dplyr::case_when(
        ACIDTRAB == "1" ~ "Sim — acidente de trabalho",
        ACIDTRAB == "2" ~ "Não",
        TRUE ~ "Ignorado"
      ),
      proporcao = round(n / sum(n) * 100, 1)
    )
} else {
  tibble(nota = "Campo ACIDTRAB ausente no arquivo")
}

# ── EIXO 3: ASSISTÊNCIA E LOCAL ───────────────────────────────────────────────

ind_lococor <- sim_nf %>%
  count(lococor_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_lococor_causa <- sim_nf %>%
  count(lococor_label, grupo_mapa, name = "n") %>%
  group_by(grupo_mapa) %>%
  mutate(prop_grupo = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(desc(n))

ind_atestante <- sim_nf %>%
  count(atestante_label, name = "n") %>%
  mutate(proporcao = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

ind_necropsia <- if ("NECROPSIA" %in% names(sim_nf)) {
  sim_nf %>%
    mutate(nec = dplyr::case_when(
      NECROPSIA == "1" ~ "Sim",
      NECROPSIA == "2" ~ "Não",
      TRUE ~ "Ignorado"
    )) %>%
    count(nec, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Campo NECROPSIA ausente")
}

ind_assistmed <- if ("ASSISTMED" %in% names(sim_nf)) {
  sim_nf %>%
    mutate(assist = dplyr::case_when(
      ASSISTMED == "1" ~ "Sim — recebeu assistência",
      ASSISTMED == "2" ~ "Não recebeu assistência",
      TRUE ~ "Ignorado"
    )) %>%
    count(assist, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Campo ASSISTMED ausente")
}

# ── EIXO 4: MATERNA E FETAL ───────────────────────────────────────────────────

# Óbitos fetais por peso ao nascer
ind_peso <- if ("PESO" %in% names(sim_ft) && nrow(sim_ft) > 0) {
  sim_ft %>%
    mutate(
      peso_num  = as.numeric(as.character(PESO)),
      faixa_peso = dplyr::case_when(
        peso_num < 500             ~ "< 500g",
        peso_num < 1000            ~ "500–999g",
        peso_num < 1500            ~ "1000–1499g",
        peso_num < 2500            ~ "1500–2499g",
        peso_num >= 2500           ~ "≥ 2500g",
        TRUE                       ~ "Não informado"
      )
    ) %>%
    count(faixa_peso, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Sem óbitos fetais ou campo PESO ausente")
}

# Fetais por semanas de gestação
ind_gestacao <- if ("SEMAGESTAC" %in% names(sim_ft) && nrow(sim_ft) > 0) {
  sim_ft %>%
    mutate(
      sem = as.numeric(as.character(SEMAGESTAC)),
      faixa_sem = dplyr::case_when(
        sem < 22              ~ "< 22 semanas",
        sem <= 27             ~ "22–27 semanas",
        sem <= 31             ~ "28–31 semanas",
        sem <= 36             ~ "32–36 semanas",
        sem >= 37             ~ "≥ 37 semanas (a termo)",
        TRUE                  ~ "Não informado"
      )
    ) %>%
    count(faixa_sem, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Sem óbitos fetais ou campo SEMAGESTAC ausente")
}

# Fetais por tipo de parto
ind_parto <- if ("PARTO" %in% names(sim_ft) && nrow(sim_ft) > 0) {
  sim_ft %>%
    mutate(tipo_parto = dplyr::case_when(
      as.character(PARTO) == "1" ~ "Vaginal",
      as.character(PARTO) == "2" ~ "Cesáreo",
      TRUE ~ "Ignorado"
    )) %>%
    count(tipo_parto, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Sem óbitos fetais ou campo PARTO ausente")
}

# Óbitos maternos (gravidez/puerpério)
ind_materno <- if (all(c("OBITOGRAV","OBITOPUERP") %in% names(sim_nf))) {
  sim_nf %>%
    mutate(
      situacao = dplyr::case_when(
        as.character(OBITOGRAV) == "1"   ~ "Durante a gravidez",
        as.character(OBITOPUERP) == "1"  ~ "Puerpério (até 42 dias)",
        as.character(OBITOPUERP) == "2"  ~ "Puerpério (43 dias a 1 ano)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(situacao)) %>%
    count(situacao, name = "n")
} else {
  tibble(nota = "Campos OBITOGRAV/OBITOPUERP ausentes")
}

# Perfil da mãe nos óbitos fetais
ind_mae <- if (all(c("IDADEMAE","ESCMAEAGR1") %in% names(sim_ft)) && nrow(sim_ft) > 0) {
  sim_ft %>%
    mutate(
      idade_mae = as.numeric(as.character(IDADEMAE)),
      faixa_mae = dplyr::case_when(
        idade_mae < 15  ~ "< 15 anos",
        idade_mae <= 19 ~ "15–19 anos",
        idade_mae <= 34 ~ "20–34 anos",
        idade_mae <= 39 ~ "35–39 anos",
        idade_mae >= 40 ~ "≥ 40 anos",
        TRUE ~ "Não informado"
      )
    ) %>%
    count(faixa_mae, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(
    n_fetais = nrow(sim_ft),
    nota = "Campos IDADEMAE/ESCMAEAGR1 ausentes"
  )
}

# ── EIXO 5: GEOGRÁFICO E TEMPORAL ─────────────────────────────────────────────

# Por município de residência
ind_municipios <- sim_nf %>%
  count(CODMUNRES, name = "n_obitos") %>%
  arrange(desc(n_obitos))

# Causa dominante por município (para o mapa)
ind_causa_mun <- sim_nf %>%
  count(CODMUNRES, grupo_mapa, name = "n_obitos") %>%
  group_by(CODMUNRES) %>%
  mutate(
    total_mun      = sum(n_obitos),
    proporcao      = round(n_obitos / total_mun * 100, 1)
  ) %>%
  arrange(CODMUNRES, desc(n_obitos)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

ind_causa_dominante <- ind_causa_mun %>%
  filter(rank == 1) %>%
  select(CODMUNRES, causa_dominante = grupo_mapa,
         n_dominante = n_obitos, total_mun, prop_dominante = proporcao)

# Série mensal
ind_mensal <- sim_nf %>%
  filter(!is.na(mes_obito), mes_obito >= 1, mes_obito <= 12) %>%
  count(mes_obito, name = "n") %>%
  mutate(
    mes_label = factor(month.abb[mes_obito], levels = month.abb),
    media     = round(mean(n), 1),
    is_acima  = n > media
  ) %>%
  arrange(mes_obito)

# ── EIXO 6: QUALIDADE DA INFORMAÇÃO ──────────────────────────────────────────

# Completude dos campos obrigatórios e relevantes
campos_completude <- c(
  "TIPOBITO","DTOBITO","SEXO","CAUSABAS","LOCOCOR",
  "CODMUNRES","RACACOR","ESCFALAGR1","ATESTANTE","NECROPSIA"
)
campos_completude <- intersect(campos_completude, names(sim_base))

ind_completude <- do.call(rbind, lapply(campos_completude, function(campo) {
  vals <- as.character(sim_base[[campo]])
  n_total_campo <- nrow(sim_base)
  n_preenchido  <- sum(!is.na(vals) & vals != "" & vals != "9" &
                         vals != "99" & vals != "*", na.rm = TRUE)
  n_ignorado    <- sum(vals %in% c("9","99"), na.rm = TRUE)
  data.frame(
    Campo         = campo,
    Total         = n_total_campo,
    Preenchido    = n_preenchido,
    Ignorado      = n_ignorado,
    Completude_pct = round(n_preenchido / n_total_campo * 100, 1),
    stringsAsFactors = FALSE
  )
})) %>%
  arrange(Completude_pct)

# Oportunidade de envio da DO
ind_oportunidade <- if ("DIFDATA" %in% names(sim_base)) {
  sim_base %>%
    mutate(dias = suppressWarnings(as.numeric(as.character(DIFDATA)))) %>%
    filter(!is.na(dias), dias >= 0, dias <= 365) %>%
    summarise(
      n              = n(),
      mediana_dias   = median(dias),
      p75_dias       = quantile(dias, 0.75),
      p90_dias       = quantile(dias, 0.90),
      prop_7dias     = round(mean(dias <= 7) * 100, 1),
      prop_30dias    = round(mean(dias <= 30) * 100, 1),
      prop_mais60    = round(mean(dias > 60) * 100, 1)
    )
} else {
  tibble(nota = "Campo DIFDATA ausente no arquivo")
}

# Óbitos investigados
ind_investigados <- if ("TPPOS" %in% names(sim_base)) {
  sim_base %>%
    mutate(inv = dplyr::case_when(
      as.character(TPPOS) == "1" ~ "Investigado",
      as.character(TPPOS) == "2" ~ "Não investigado",
      TRUE ~ "Não informado"
    )) %>%
    count(inv, name = "n") %>%
    mutate(proporcao = round(n / sum(n) * 100, 1))
} else {
  tibble(nota = "Campo TPPOS ausente no arquivo")
}

# Causa mal definida (R00-R99)
n_mal_def <- sum(substr(sim_nf$CAUSABAS, 1, 1) == "R", na.rm = TRUE)
ind_mal_def <- tibble(
  Total_nao_fetais     = n_total,
  N_causa_mal_definida = n_mal_def,
  Prop_mal_definida    = round(n_mal_def / n_total * 100, 1),
  Meta_referencia      = "< 10% (RIPSA)"
)

# -----------------------------------------------------------------------------
# 5. MAPA: CAUSA BÁSICA DOMINANTE POR MUNICÍPIO
# -----------------------------------------------------------------------------

shp <- st_read(caminho_shp, quiet = TRUE)

# Normalizar nomes para join: remover acentos e converter para maiúsculo
normalizar <- function(x) {
  # chartr ANTES do toupper — remove acentos mantendo case original
  # depois toupper garante tudo maiúsculo de forma consistente
  x <- chartr(
    "ÀÁÂÃÄÅàáâãäåÈÉÊËèéêëÌÍÎÏìíîïÒÓÔÕÖòóôõöÙÚÛÜùúûüÇçÑñ",
    "AAAAAAaaaaaaeeeeeeeeiiiiiiiioooooooooouuuuuuuuccnn",
    x
  )
  x <- toupper(x)          # toupper depois — tudo vira maiúsculo sem problema
  x <- gsub("'|-", " ", x)
  trimws(gsub("\\s+", " ", x))
}

shp <- shp %>%
  mutate(nome_norm = normalizar(NM_MUN))

tab_pop <- tab_pop %>%
  mutate(nome_norm = normalizar(nome_datasus))

# Tabela de correspondência: código DATASUS 6 dígitos ↔ CD_MUN IBGE 7 dígitos
correspondencia <- shp %>%
  st_drop_geometry() %>%
  select(CD_MUN, NM_MUN, nome_norm) %>%
  inner_join(tab_pop %>% select(codigo_datasus, nome_datasus,
                                populacao_2025, nome_norm),
             by = "nome_norm")

nao_encontrados <- tab_pop %>%
  filter(!nome_norm %in% correspondencia$nome_norm)

if (nrow(nao_encontrados) > 0) {
  message("Municípios sem correspondência no shapefile (verificar nome):")
  print(nao_encontrados %>% select(codigo_datasus, nome_datasus))
}

message("Municípios com correspondência shapefile: ",
        nrow(correspondencia), " de ", nrow(tab_pop))

# Filtrar shapefile para os 30 municípios da 15ª RS
shp_15rs <- shp %>%
  filter(CD_MUN %in% correspondencia$CD_MUN)

shp_dados <- shp_15rs %>%
  left_join(correspondencia %>% select(CD_MUN, codigo_datasus, populacao_2025),
            by = "CD_MUN") %>%
  left_join(ind_causa_dominante, by = c("codigo_datasus" = "CODMUNRES")) %>%
  left_join(ind_municipios,      by = c("codigo_datasus" = "CODMUNRES")) %>%
  mutate(taxa_bruta = round(n_obitos / populacao_2025 * 100000, 1))

centroides <- shp_15rs %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(CD_MUN, NM_MUN, lon, lat)

shp_dados <- shp_dados %>%
  left_join(centroides %>% select(CD_MUN, lon, lat), by = "CD_MUN")

paleta_mapa <- c(
  "Circulatório"               = "#C0392B",
  "Neoplasias"                 = "#8E44AD",
  "Causas externas"            = "#E67E22",
  "Respiratório"               = "#2980B9",
  "Infecciosas e parasitárias" = "#27AE60",
  "Endócrinas / Diabetes"      = "#F39C12",
  "Digestivo"                  = "#16A085",
  "Neurológico / Mental"       = "#7F8C8D",
  "Mal definidas"              = "#BDC3C7",
  "Outras"                     = "#D5D8DC"
)

  # Níveis em ordem crescente — ggplot renderiza do 1º (baixo) ao último (cima)
  # rev() coloca o maior por último → aparece no topo da legenda
  ordem_causas <- sim_nf %>%
    count(grupo_mapa, sort = TRUE) %>%
    pull(grupo_mapa)

shp_dados <- shp_dados %>%
  mutate(causa_dominante = factor(causa_dominante, levels = rev(ordem_causas)))

mapa <- ggplot(shp_dados) +
  geom_sf(aes(fill = causa_dominante), color = "white", linewidth = 0.35) +
  geom_text(
    data = shp_dados %>% st_drop_geometry() %>% filter(!is.na(causa_dominante)),
    aes(x = lon, y = lat, label = NM_MUN),
    size = 2, color = "grey15", lineheight = 0.8,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values   = paleta_mapa,
    na.value = "#F0F0F0",
    name     = "Causa dominante"
  ) +
  labs(
    title    = paste0("Causa básica dominante por município  |  N = ", format(n_total, big.mark = ".")),
    subtitle = paste0("15ª Regional de Saúde — Maringá/PR | SIM/DATASUS, ", ANO),
    caption  = paste0(
      "Causa dominante = grupo CID-10 com maior proporção de óbitos.\n",
      "Excluídos: óbitos fetais e registros sem causa básica definida.\n",
      "Fonte: SIM/DATASUS | Elaboração: SCVGE — 15ª RS"
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(color = "grey40", size = 10, hjust = 0,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(color = "grey50", size = 7.5, hjust = 0,
                                 lineheight = 1.3, margin = margin(t = 10)),
    legend.position = "right",
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8),
    plot.margin     = margin(12, 12, 12, 12)
  )

grafico_barras <- ggplot(
  sim_nf %>% count(grupo_mapa, name = "n") %>%
    mutate(prop = round(n / sum(n) * 100, 1),
           grupo_mapa = factor(grupo_mapa, levels = rev(unique(grupo_mapa[order(n)])))),
  aes(x = prop, y = grupo_mapa, fill = grupo_mapa)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1, size = 2.8, color = "grey30") +
  scale_fill_manual(values = paleta_mapa) +
  scale_x_continuous(limits = c(0, 45), labels = label_number(suffix = "%")) +
  labs(title = "Distribuição regional", subtitle = paste0("N = ", format(n_total, big.mark = ".")),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 7.5))

layout_final <- mapa + grafico_barras + plot_layout(widths = c(2, 1))

arquivo_png <- file.path(dir_saida,
  paste0("mapa_causabas_15rs_", ANO, ".png"))
ggsave(arquivo_png, layout_final, width = 16, height = 10, dpi = 200, bg = "white")
message("Mapa salvo: ", arquivo_png)

# -----------------------------------------------------------------------------
# 5b. PIRÂMIDE ETÁRIA DOS ÓBITOS POR SEXO
# -----------------------------------------------------------------------------

# Ordenação das faixas (base = mais jovem, topo = mais velho)
niveis_faixa <- c(
  "< 1 ano", "1–4 anos", "5–14 anos", "15–29 anos",
  "30–49 anos", "50–59 anos", "60–69 anos", "70–79 anos",
  "80 anos e mais"
)

piramide_dados <- sim_nf %>%
  filter(sexo_label %in% c("Masculino", "Feminino"),
         faixa_et %in% niveis_faixa) %>%
  count(faixa_et, sexo_label, name = "n") %>%
  group_by(sexo_label) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    faixa_et = factor(faixa_et, levels = niveis_faixa),
    # Masculino fica no lado esquerdo (negativo)
    n_plot    = if_else(sexo_label == "Masculino", -n, n),
    prop_plot = if_else(sexo_label == "Masculino", -prop, prop)
  )

# Totais por sexo para o subtítulo
n_masc <- piramide_dados %>% filter(sexo_label == "Masculino") %>% pull(n) %>% sum()
n_fem  <- piramide_dados %>% filter(sexo_label == "Feminino")  %>% pull(n) %>% sum()

# Limite simétrico do eixo x
lim_x <- max(abs(piramide_dados$n_plot)) * 1.18

# Labels posicionados fora de cada barra
piramide_dados <- piramide_dados %>%
  mutate(
    label_pos = if_else(sexo_label == "Masculino",
                        n_plot - max(abs(n_plot)) * 0.02,
                        n_plot + max(abs(n_plot)) * 0.02),
    hjust_lbl = if_else(sexo_label == "Masculino", 1, 0),
    label_txt = paste0(n, " (", prop, "%)")
  )

piramide <- ggplot(piramide_dados,
                   aes(x = n_plot, y = faixa_et, fill = sexo_label)) +
  # Barras
  geom_col(width = 0.72, alpha = 0.92) +
  # Labels fora das barras
  geom_text(
    aes(x = label_pos, label = label_txt, hjust = hjust_lbl),
    size = 3, color = "grey25"
  ) +
  # Linha central
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  # Escala x: mostrar valores absolutos em ambos os lados
  scale_x_continuous(
    limits = c(-lim_x, lim_x),
    labels = function(x) format(abs(x), big.mark = ".")
  ) +
  scale_fill_manual(
    values = c("Masculino" = "#2980B9", "Feminino" = "#C0392B"),
    name = NULL
  ) +
  # Títulos centralizados acima de cada coluna (1/4 da largura do eixo de cada lado)
  annotate("text", x = -lim_x * 0.55, y = length(niveis_faixa) + 0.75,
           label = paste0("Masculino\nN = ", format(n_masc, big.mark = ".")),
           hjust = 0.5, vjust = 0, size = 3.8,
           color = "#2980B9", fontface = "bold", lineheight = 1.1) +
  annotate("text", x =  lim_x * 0.55, y = length(niveis_faixa) + 0.75,
           label = paste0("Feminino\nN = ", format(n_fem, big.mark = ".")),
           hjust = 0.5, vjust = 0, size = 3.8,
           color = "#C0392B", fontface = "bold", lineheight = 1.1) +
  coord_cartesian(clip = "off",
                  ylim = c(0.5, length(niveis_faixa) + 1.6)) +
  labs(
    title    = paste0("Pirâmide etária dos óbitos  |  N = ", format(n_total, big.mark = ".")),
    subtitle = paste0("15ª Regional de Saúde — Maringá/PR | SIM/DATASUS, ", ANO),
    x = "Número de óbitos",
    y = NULL,
    caption = paste0(
      "Excluídos: óbitos fetais, sexo ignorado e faixa etária ignorada.\n",
      "Fonte: SIM/DATASUS | Elaboração: SCVGE — 15ª RS"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle    = element_text(color = "grey40", size = 10, hjust = 0,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(color = "grey50", size = 7.5, hjust = 0,
                                    lineheight = 1.3, margin = margin(t = 10)),
    legend.position  = "none",
    axis.text.y      = element_text(size = 10, face = "bold"),
    axis.text.x      = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    plot.margin      = margin(14, 80, 14, 14)   # margem direita maior para labels
  )

arquivo_piramide <- file.path(dir_saida,
  paste0("piramide_etaria_obitos_15rs_", ANO, ".png"))
ggsave(arquivo_piramide, piramide, width = 12, height = 8, dpi = 200, bg = "white")
message("Pirâmide salva: ", arquivo_piramide)

# -----------------------------------------------------------------------------
# 6. EXPORTAR EXCEL CONSOLIDADO
# -----------------------------------------------------------------------------

wb <- createWorkbook()

# Estilo de cabeçalho
estilo_cab <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#004561",
  halign = "LEFT", textDecoration = "bold", fontSize = 10
)
estilo_num <- createStyle(numFmt = "0.0", halign = "RIGHT")

adicionar_aba <- function(wb, nome, dados, subtitulo = NULL) {
  addWorksheet(wb, nome)
  linha_ini <- 1
  if (!is.null(subtitulo)) {
    writeData(wb, nome, subtitulo, startRow = 1, startCol = 1)
    addStyle(wb, nome, createStyle(textDecoration = "italic", fontColour = "#555555"),
             rows = 1, cols = 1)
    linha_ini <- 3
  }
  writeDataTable(wb, nome, dados, startRow = linha_ini,
                 tableStyle = "TableStyleLight2", headerStyle = estilo_cab)
  setColWidths(wb, nome, cols = 1:ncol(dados), widths = "auto")
}

sub <- paste0("15ª RS | SIM/DATASUS | Ano: ", ANO,
              " | Elaboração: SCVGE | N não fetais = ", format(n_total, big.mark = "."))

adicionar_aba(wb, "Resumo — totais",
  data.frame(
    Indicador = c("Total de registros no arquivo","Registros da 15ª RS (todos)",
                  "Óbitos não fetais c/ causa válida","Óbitos fetais",
                  "Proporção causas mal definidas (%)","Proporção causas evitáveis (%)"),
    Valor = c(nrow(sim_raw), nrow(sim_base), n_total, nrow(sim_ft),
              ind_mal_def$Prop_mal_definida, ind_evitaveis$prop_evitavel)
  ), subtitulo = sub)

adicionar_aba(wb, "Perfil — Sexo",           ind_sexo,           sub)
adicionar_aba(wb, "Perfil — Faixa etária",   ind_faixa,          sub)
adicionar_aba(wb, "Perfil — Faixa etária x sexo", ind_faixa_sexo, sub)
adicionar_aba(wb, "Perfil — Pirâmide (dados)",
  piramide_dados %>%
    select(Faixa_etaria = faixa_et, Sexo = sexo_label,
           N_obitos = n, Proporcao_no_sexo = prop) %>%
    arrange(Faixa_etaria, Sexo),
  sub)
adicionar_aba(wb, "Perfil — Raça e cor",     ind_raca,           sub)
adicionar_aba(wb, "Perfil — Escolaridade",   ind_esc,            sub)
adicionar_aba(wb, "Causas — Capítulos CID",  ind_capitulos,      sub)
adicionar_aba(wb, "Causas — Top 20 CIDs",    ind_top20,          sub)
adicionar_aba(wb, "Causas — Externas",       ind_externas,       sub)
adicionar_aba(wb, "Causas — Evitáveis top20",ind_evitaveis_cid,  sub)
adicionar_aba(wb, "Assistência — Local",     ind_lococor,        sub)
adicionar_aba(wb, "Assistência — Atestante", ind_atestante,      sub)
adicionar_aba(wb, "Materna — Fetal (peso)",  ind_peso,           sub)
adicionar_aba(wb, "Materna — Gestação",      ind_gestacao,       sub)
adicionar_aba(wb, "Geo — Por município",
  ind_municipios %>%
    left_join(correspondencia %>% select(codigo_datasus, NM_MUN, populacao_2025),
              by = c("CODMUNRES" = "codigo_datasus")) %>%
    left_join(ind_causa_dominante %>% select(CODMUNRES, causa_dominante, prop_dominante),
              by = "CODMUNRES") %>%
    mutate(taxa_bruta_100mil = round(n_obitos / populacao_2025 * 100000, 1)) %>%
    select(Município = NM_MUN, Código_DATASUS = CODMUNRES,
           N_obitos = n_obitos, Populacao_2025 = populacao_2025,
           Taxa_bruta_100mil = taxa_bruta_100mil,
           Causa_dominante = causa_dominante,
           Prop_dominante = prop_dominante) %>%
    arrange(desc(N_obitos)),
  sub)
adicionar_aba(wb, "Temporal — Série mensal", ind_mensal,         sub)
adicionar_aba(wb, "Qualidade — Completude",  ind_completude,     sub)
adicionar_aba(wb, "Qualidade — Oportunidade",
  if (is.data.frame(ind_oportunidade)) ind_oportunidade
  else data.frame(Nota = "Campo DIFDATA ausente"), sub)

arquivo_xlsx <- file.path(dir_saida,
  paste0("indicadores_sim_15rs_", ANO, ".xlsx"))
saveWorkbook(wb, arquivo_xlsx, overwrite = TRUE)
message("Excel salvo: ", arquivo_xlsx)

# -----------------------------------------------------------------------------
# 7. GRÁFICOS POR EIXO TEMÁTICO
# Paleta institucional 15ª RS
# -----------------------------------------------------------------------------

cor_masc    <- "#2980B9"
cor_fem     <- "#C0392B"
cor_primaria <- "#004561"
cor_sec      <- "#1C7685"
cor_destaque <- "#FF6F31"

tema_base <- theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0,
                                   color = cor_primaria),
    plot.subtitle   = element_text(size = 9, color = "grey45", hjust = 0,
                                   margin = margin(b = 8)),
    plot.caption    = element_text(size = 7.5, color = "grey55", hjust = 0,
                                   lineheight = 1.3, margin = margin(t = 10)),
    panel.grid.minor  = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text         = element_text(size = 9),
    plot.margin       = margin(14, 14, 14, 14)
  )

rodape <- paste0("Fonte: SIM/DATASUS, ", ANO, " | Elaboração: SCVGE — 15ª RS")

salvar <- function(g, nome, w = 12, h = 7) {
  arq <- file.path(dir_saida, paste0(nome, "_", ANO, ".png"))
  ggsave(arq, g, width = w, height = h, dpi = 200, bg = "white")
  message("Salvo: ", arq)
}

# ── G1: SEXO ─────────────────────────────────────────────────────────────────

g_sexo <- ind_sexo %>%
  filter(sexo_label != "Ignorado") %>%
  mutate(sexo_label = factor(sexo_label, levels = c("Feminino","Masculino"))) %>%
  ggplot(aes(x = sexo_label, y = n, fill = sexo_label)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(n, "\n(", proporcao, "%)")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Masculino" = cor_masc, "Feminino" = cor_fem),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = paste0("Óbitos por sexo  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
       x = NULL, y = "N de óbitos", caption = rodape) +
  tema_base +
  theme(panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_blank())

salvar(g_sexo, "g1_sexo", w = 7, h = 6)

# ── G2: FAIXA ETÁRIA ─────────────────────────────────────────────────────────

g_faixa <- ind_faixa %>%
  filter(faixa_et != "Ignorada") %>%
  ggplot(aes(x = n, y = faixa_et)) +
  geom_col(fill = cor_primaria, width = 0.7, alpha = 0.88) +
  geom_text(aes(label = paste0(n, "  (", proporcao, "%)")),
            hjust = -0.05, size = 3.2, color = "grey25") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = paste0("Óbitos por faixa etária  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
       x = "N de óbitos", y = NULL, caption = rodape) +
  tema_base

salvar(g_faixa, "g2_faixa_etaria", w = 10, h = 7)

# ── G3: RAÇA / COR ───────────────────────────────────────────────────────────

g_raca <- ind_raca %>%
  filter(raca_label != "Ignorada") %>%
  mutate(raca_label = reorder(raca_label, n)) %>%
  ggplot(aes(x = n, y = raca_label, fill = raca_label)) +
  geom_col(width = 0.65, show.legend = FALSE, alpha = 0.9) +
  geom_text(aes(label = paste0(n, "  (", proporcao, "%)")),
            hjust = -0.05, size = 3.2, color = "grey25") +
  scale_fill_manual(values = colorRampPalette(c("#B0C4DE", cor_primaria))(
    nrow(ind_raca %>% filter(raca_label != "Ignorada")))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = paste0("Óbitos por raça e cor  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
       x = "N de óbitos", y = NULL, caption = rodape) +
  tema_base

salvar(g_raca, "g3_raca_cor", w = 10, h = 6)

# ── G4: CAPÍTULOS CID-10 ─────────────────────────────────────────────────────

g_cap <- ind_capitulos %>%
  filter(capitulo_nome != "Não classificado") %>%
  mutate(label_cap = paste0(capitulo_num, " — ", capitulo_nome),
         label_cap = reorder(label_cap, n)) %>%
  ggplot(aes(x = proporcao, y = label_cap)) +
  geom_col(fill = cor_sec, width = 0.72, alpha = 0.9) +
  geom_text(aes(label = paste0(proporcao, "%  (n=", n, ")")),
            hjust = -0.05, size = 2.9, color = "grey25") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28)),
                     labels = label_number(suffix = "%")) +
  labs(title = paste0("Mortalidade proporcional por capítulo CID-10  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
       x = "Proporção (%)", y = NULL, caption = rodape) +
  tema_base

salvar(g_cap, "g4_capitulos_cid", w = 13, h = 9)

# ── G5: TOP 20 CIDs ESPECÍFICOS ──────────────────────────────────────────────

g_top20 <- ind_top20 %>%
  mutate(
    desc_label = str_wrap(paste0(codigo3, " — ", descricao), 35),
    desc_label = reorder(desc_label, n)
  ) %>%
  ggplot(aes(x = n, y = desc_label)) +
  geom_segment(aes(x = 0, xend = n, yend = desc_label),
               color = "grey80", linewidth = 0.7) +
  geom_point(aes(color = grupo_mapa), size = 4) +
  geom_text(aes(label = paste0(n, " (", proporcao, "%)")),
            hjust = -0.2, size = 2.8, color = "grey25") +
  scale_color_manual(values = paleta_mapa, name = "Grupo") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(title = paste0("Top 20 causas básicas de morte  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO, " | CID-10 (3 dígitos)"),
       x = "N de óbitos", y = NULL, caption = rodape) +
  tema_base +
  theme(legend.position = "right",
        legend.title    = element_text(size = 8),
        legend.text     = element_text(size = 7.5))

salvar(g_top20, "g5_top20_cids", w = 14, h = 10)

# ── G6: CAUSAS EXTERNAS POR TIPO ─────────────────────────────────────────────

if (is.data.frame(ind_externas) && nrow(ind_externas) > 0 &&
    !"nota" %in% names(ind_externas)) {
  g_ext <- ind_externas %>%
    filter(circobito_label != "Não informado") %>%
    mutate(circobito_label = reorder(circobito_label, n)) %>%
    ggplot(aes(x = n, y = circobito_label, fill = circobito_label)) +
    geom_col(width = 0.6, show.legend = FALSE, alpha = 0.9) +
    geom_text(aes(label = paste0(n, "  (", proporcao, "%)")),
              hjust = -0.05, size = 3.2, color = "grey25") +
    scale_fill_manual(values = c(
      "Acidente"  = "#E67E22",
      "Homicídio" = "#C0392B",
      "Suicídio"  = "#8E44AD",
      "Outros"    = "#7F8C8D",
      "Ignorado"  = "#BDC3C7"
    )) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = paste0("Causas externas por tipo de circunstância  |  N = ",
                        format(sum(ind_externas$n), big.mark = ".")),
         subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO, " | Cap. XX CID-10"),
         x = "N de óbitos", y = NULL, caption = rodape) +
    tema_base

  salvar(g_ext, "g6_causas_externas", w = 10, h = 6)
}

# ── G7: LOCAL DE OCORRÊNCIA ──────────────────────────────────────────────────

if (is.data.frame(ind_lococor) && !"nota" %in% names(ind_lococor)) {
  g_local <- ind_lococor %>%
    filter(lococor_label != "Não informado", lococor_label != "Ignorado") %>%
    mutate(lococor_label = reorder(lococor_label, n)) %>%
    ggplot(aes(x = n, y = lococor_label)) +
    geom_col(fill = cor_sec, width = 0.65, alpha = 0.9) +
    geom_text(aes(label = paste0(n, "  (", proporcao, "%)")),
              hjust = -0.05, size = 3.2, color = "grey25") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = paste0("Local de ocorrência do óbito  |  N = ", format(n_total, big.mark = ".")),
         subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
         x = "N de óbitos", y = NULL, caption = rodape) +
    tema_base

  salvar(g_local, "g7_local_ocorrencia", w = 10, h = 6)
}

# ── G8: ATESTANTE ────────────────────────────────────────────────────────────

if (is.data.frame(ind_atestante) && !"nota" %in% names(ind_atestante)) {
  g_atestante <- ind_atestante %>%
    filter(atestante_label != "Não informado") %>%
    mutate(atestante_label = reorder(atestante_label, n)) %>%
    ggplot(aes(x = n, y = atestante_label)) +
    geom_col(fill = cor_primaria, width = 0.6, alpha = 0.85) +
    geom_text(aes(label = paste0(n, "  (", proporcao, "%)")),
              hjust = -0.05, size = 3.2, color = "grey25") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(title = paste0("Tipo de atestante do óbito  |  N = ", format(n_total, big.mark = ".")),
         subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
         x = "N de óbitos", y = NULL, caption = rodape) +
    tema_base

  salvar(g_atestante, "g8_atestante", w = 10, h = 6)
}

# ── G9: SÉRIE MENSAL ─────────────────────────────────────────────────────────

g_mensal <- ind_mensal %>%
  ggplot(aes(x = mes_label, y = n, group = 1)) +
  geom_line(color = cor_sec, linewidth = 1) +
  geom_point(aes(fill = is_acima), shape = 21, size = 3.5,
             color = "white", stroke = 1.2) +
  geom_hline(aes(yintercept = media), linetype = "dashed",
             color = cor_destaque, linewidth = 0.6) +
  geom_text(aes(label = n), vjust = -1.1, size = 3, color = "grey30") +
  annotate("text", x = 0.6, y = unique(ind_mensal$media) * 1.02,
           label = paste0("Média: ", unique(ind_mensal$media)),
           hjust = 0, size = 3, color = cor_destaque, fontface = "italic") +
  scale_fill_manual(values = c("TRUE" = cor_destaque, "FALSE" = cor_sec),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = paste0("Série de óbitos por mês  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO,
                         " | Pontos laranjas = acima da média mensal"),
       x = NULL, y = "N de óbitos", caption = rodape) +
  tema_base +
  theme(panel.grid.major.x = element_line(color = "grey92"),
        panel.grid.major.y = element_line(color = "grey92"))

salvar(g_mensal, "g9_serie_mensal", w = 12, h = 6)

# ── G10: MUNICÍPIOS — TAXA BRUTA ─────────────────────────────────────────────

g_mun <- ind_municipios %>%
  left_join(correspondencia %>% select(codigo_datasus, NM_MUN, populacao_2025),
            by = c("CODMUNRES" = "codigo_datasus")) %>%
  mutate(
    taxa = round(n_obitos / populacao_2025 * 100000, 1),
    NM_MUN = reorder(NM_MUN, taxa)
  ) %>%
  ggplot(aes(x = taxa, y = NM_MUN)) +
  geom_col(fill = cor_primaria, width = 0.72, alpha = 0.88) +
  geom_text(aes(label = taxa), hjust = -0.15, size = 2.8, color = "grey25") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = paste0("Taxa bruta de mortalidade por município  |  N = ", format(n_total, big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO,
                         " | Óbitos não fetais por 100.000 hab."),
       x = "Taxa por 100.000 hab.", y = NULL, caption = rodape) +
  tema_base

salvar(g_mun, "g10_taxa_municipios", w = 12, h = 10)

# ── G11: COMPLETUDE DOS CAMPOS ───────────────────────────────────────────────

g_completude <- ind_completude %>%
  mutate(
    Campo = reorder(Campo, Completude_pct),
    cor   = case_when(
      Completude_pct >= 95 ~ "#27AE60",
      Completude_pct >= 80 ~ "#F39C12",
      TRUE                 ~ "#C0392B"
    )
  ) %>%
  ggplot(aes(x = Completude_pct, y = Campo, fill = cor)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(Completude_pct, "%")),
            hjust = -0.1, size = 3.2, color = "grey25") +
  geom_vline(xintercept = 95, linetype = "dashed",
             color = "#27AE60", linewidth = 0.5) +
  geom_vline(xintercept = 80, linetype = "dashed",
             color = "#F39C12", linewidth = 0.5) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 115),
                     labels = label_number(suffix = "%")) +
  annotate("text", x = 95.5, y = 0.6, label = "95%", size = 2.8,
           color = "#27AE60", hjust = 0) +
  annotate("text", x = 80.5, y = 0.6, label = "80%", size = 2.8,
           color = "#F39C12", hjust = 0) +
  labs(title = paste0("Completude dos campos  |  N = ", format(nrow(sim_base), big.mark = ".")),
       subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO,
                         " | Verde ≥ 95%  Amarelo 80–94%  Vermelho < 80%"),
       x = "Completude (%)", y = NULL, caption = rodape) +
  tema_base

salvar(g_completude, "g11_completude", w = 11, h = 7)

# ── PAINEL RESUMO — 4 gráficos em grade ──────────────────────────────────────

painel <- (g_sexo | g_faixa) / (g_cap | g_mensal) +
  plot_annotation(
    title    = paste0("Painel de indicadores — SIM | 15ª RS | ", ANO),
    subtitle = paste0("N total óbitos não fetais = ",
                      format(n_total, big.mark = ".")),
    caption  = rodape,
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14, color = cor_primaria),
      plot.subtitle = element_text(size = 10, color = "grey45"),
      plot.caption  = element_text(size = 8, color = "grey55")
    )
  )

salvar(painel, "g00_painel_resumo", w = 18, h = 14)

message("\nTodos os gráficos salvos em: ", dir_saida)

cat("\n", rep("=", 60), "\n", sep = "")
cat("INDICADORES SIM — 15ª RS | ANO:", ANO, "\n")
cat(rep("=", 60), "\n", sep = "")
cat("Total de óbitos não fetais:    ", n_total, "\n")
cat("Causa mal definida (R%):       ", ind_mal_def$Prop_mal_definida, "%\n")
cat("Causas evitáveis (%):          ", ind_evitaveis$prop_evitavel, "%\n")
cat("\nTop 5 causas:\n")
print(ind_top20 %>% select(codigo3, descricao, n, proporcao) %>% head(5))
cat(rep("=", 60), "\n\n", sep = "")
