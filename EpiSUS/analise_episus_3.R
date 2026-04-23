## ============================================================================
## ANALISE E BOLETIM - Avaliacao EpiSUS-Fundamental 15a RS
## ============================================================================
## Le a planilha de respostas direto do Google Sheets via API,
## trata os dados, calcula indicadores e gera um boletim em HTML.
## ============================================================================

# --- Pacotes ----------------------------------------------------------------
pacotes <- c("googlesheets4", "dplyr", "tidyr", "stringr", "ggplot2",
             "forcats", "scales", "patchwork", "knitr", "rmarkdown", "glue",
             "writexl")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

setwd("/Users/valentimsalajunior/Documents/Documentos_2026/EpiSUS")

# --- Configuracoes ----------------------------------------------------------
#
# AUTENTICACAO - escolha UMA das opcoes abaixo:
#
# OPCAO 1 - Interativa (uso manual, abre navegador na 1a vez):
#   gs4_auth()
#
# OPCAO 2 - Conta de servico (automacao, sem navegador):
#   gs4_auth(path = "credencial_service_account.json")
#
# OPCAO 3 - Planilha publica (somente leitura, sem autenticacao):
#   gs4_deauth()

gs4_auth(email = TRUE)

# URL ou ID da planilha de respostas do Google Forms
PLANILHA_URL <- "https://docs.google.com/spreadsheets/d/1eZyZVPUjyN_vat3wMmRh7WFcHDQl4adlYUztAlj9mLY/edit?resourcekey=&gid=1178221128#gid=1178221128"

# Paleta institucional 15a RS
COR_AZUL    <- "#004561"
COR_VERDE   <- "#1C7685"
COR_LARANJA <- "#FF6F31"
COR_CINZA   <- "#6B7280"

TEMA_EPISUS <- theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", color = COR_AZUL, size = 13),
    plot.subtitle    = element_text(color = COR_CINZA, size = 10),
    plot.caption     = element_text(color = COR_CINZA, size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E7EB"),
    panel.grid.major.x = element_blank(),
    axis.title       = element_text(color = COR_CINZA),
    legend.position  = "bottom"
  )

theme_set(TEMA_EPISUS)

# --- Leitura via Google Sheets API ------------------------------------------
cat("Conectando ao Google Sheets...\n")
df_raw <- read_sheet(PLANILHA_URL)
cat("Registros lidos:", nrow(df_raw), "\n")
cat("Colunas:", ncol(df_raw), "\n\n")

writexl::write_xlsx(as.data.frame(df_raw), "Respostas_EpiSUS_backup.xlsx")
cat("Backup local salvo: Respostas_EpiSUS_backup.xlsx\n\n")

# --- Mapeamento de colunas --------------------------------------------------
renomear <- c(
  "timestamp"          = "Timestamp",
  "tcle"               = "Declaro que li e concordo com os termos acima",
  "codigo_anonimo"     = "C\u00f3digo an\u00f4nimo",
  "turma"              = "Turma",
  "idade"              = "Idade",
  "sexo"               = "Sexo",
  "profissao"          = "Profiss\u00e3o",
  "escolaridade"       = "Escolaridade",
  "municipio"          = "Munic\u00edpio de atua\u00e7\u00e3o",
  "tempo_atuacao"      = "Tempo de atua\u00e7\u00e3o em vigil\u00e2ncia epidemiol\u00f3gica",
  "pre_analise"        = "Avalie seu n\u00edvel ANTES do curso [An\u00e1lise de dados epidemiol\u00f3gicos]",
  "pre_investigacao"   = "Avalie seu n\u00edvel ANTES do curso [Investiga\u00e7\u00e3o de surtos e epidemias]",
  "pre_indicadores"    = "Avalie seu n\u00edvel ANTES do curso [Interpreta\u00e7\u00e3o de indicadores de sa\u00fade]",
  "pre_comunicacao"    = "Avalie seu n\u00edvel ANTES do curso [Comunica\u00e7\u00e3o t\u00e9cnica de resultados]",
  "pos_analise"        = "Avalie seu n\u00edvel AP\u00d3S o curso [An\u00e1lise de dados epidemiol\u00f3gicos]",
  "pos_investigacao"   = "Avalie seu n\u00edvel AP\u00d3S o curso [Investiga\u00e7\u00e3o de surtos e epidemias]",
  "pos_indicadores"    = "Avalie seu n\u00edvel AP\u00d3S o curso [Interpreta\u00e7\u00e3o de indicadores de sa\u00fade]",
  "pos_comunicacao"    = "Avalie seu n\u00edvel AP\u00d3S o curso [Comunica\u00e7\u00e3o t\u00e9cnica de resultados]",
  "sat_qualidade"      = "Qualidade geral do curso",
  "sat_organizacao"    = "Organiza\u00e7\u00e3o e log\u00edstica",
  "sat_didatica"       = "Did\u00e1tica dos facilitadores",
  "sat_suporte"        = "Suporte dos tutores",
  "sat_carga"          = "Adequa\u00e7\u00e3o da carga hor\u00e1ria",
  "sat_relevancia"     = "Relev\u00e2ncia do conte\u00fado para sua pr\u00e1tica",
  "sat_material"       = "Qualidade do material did\u00e1tico",
  "sat_objetivos"      = "Clareza dos objetivos de aprendizagem",
  "met_aprender"       = "Aprender fazendo foi uma estrat\u00e9gia eficaz",
  "met_praticas"       = "As atividades pr\u00e1ticas foram adequadas",
  "met_produtos"       = "Os produtos elaborados contribu\u00edram para a aprendizagem",
  "met_integracao"     = "Houve boa integra\u00e7\u00e3o entre teoria e pr\u00e1tica",
  "met_tempo"          = "O tempo para atividades pr\u00e1ticas foi suficiente",
  "met_simulacoes"     = "As simula\u00e7\u00f5es/exerc\u00edcios refletiram situa\u00e7\u00f5es reais",
  "aplicacao_rotina"   = "Com que frequ\u00eancia voc\u00ea aplica o que aprendeu no curso em sua rotina de trabalho?",
  "tipo_aplicacao"     = "Em quais atividades voc\u00ea aplicou o aprendizado? (marque todas que se aplicam)",
  "imp_resposta"       = "Melhora na resposta a eventos epidemiol\u00f3gicos",
  "imp_qualidade"      = "Melhora na qualidade do trabalho em vigil\u00e2ncia",
  "imp_vigilancia"     = "Fortalecimento da vigil\u00e2ncia epidemiol\u00f3gica local",
  "imp_decisao"        = "Melhora na tomada de decis\u00e3o baseada em dados",
  "nps_score"          = "De 0 a 10, qual a probabilidade de voc\u00ea recomendar o EpiSUS-Fundamental a um colega?",
  "dificuldades"       = "Quais dificuldades voc\u00ea enfrentou durante o curso? (marque todas que se aplicam)",
  "exemplo_aplicacao"  = "Descreva brevemente uma situa\u00e7\u00e3o em que voc\u00ea aplicou algo aprendido no curso",
  "sugestoes"          = "Sugest\u00f5es para melhoria do curso"
)

df <- df_raw
for (novo in names(renomear)) {
  original <- renomear[[novo]]
  if (original %in% names(df)) {
    names(df)[names(df) == original] <- novo
  }
}

# --- Limpeza ----------------------------------------------------------------
extrair_numero <- function(x) {
  as.numeric(str_extract(as.character(x), "^\\d+"))
}

colunas_pre  <- c("pre_analise", "pre_investigacao", "pre_indicadores", "pre_comunicacao")
colunas_pos  <- c("pos_analise", "pos_investigacao", "pos_indicadores", "pos_comunicacao")

for (col in c(colunas_pre, colunas_pos)) {
  if (col %in% names(df)) df[[col]] <- extrair_numero(df[[col]])
}

colunas_sat <- names(df)[str_starts(names(df), "sat_")]
colunas_met <- names(df)[str_starts(names(df), "met_")]
colunas_imp <- names(df)[str_starts(names(df), "imp_")]

for (col in c(colunas_sat, colunas_met, colunas_imp, "nps_score")) {
  if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
}

if ("idade" %in% names(df)) df$idade <- as.numeric(df$idade)

cat("Colunas renomeadas e limpas.\n\n")

# ============================================================================
# INDICADORES
# ============================================================================

competencias <- c("analise", "investigacao", "indicadores", "comunicacao")
competencias_labels <- c(
  "analise"       = "Analise de dados",
  "investigacao"  = "Investigacao de surtos",
  "indicadores"   = "Interpretacao de indicadores",
  "comunicacao"   = "Comunicacao tecnica"
)

for (comp in competencias) {
  col_pre   <- paste0("pre_", comp)
  col_pos   <- paste0("pos_", comp)
  col_delta <- paste0("delta_", comp)
  if (all(c(col_pre, col_pos) %in% names(df))) {
    df[[col_delta]] <- df[[col_pos]] - df[[col_pre]]
  }
}

colunas_delta <- paste0("delta_", competencias)

df$escore_satisfacao  <- rowMeans(df[colunas_sat], na.rm = TRUE)
df$escore_metodologia <- rowMeans(df[colunas_met], na.rm = TRUE)
df$escore_impacto     <- rowMeans(df[colunas_imp], na.rm = TRUE)
df$delta_medio        <- rowMeans(df[colunas_delta], na.rm = TRUE)

# --- NPS --------------------------------------------------------------------
calcular_nps <- function(scores) {
  scores <- scores[!is.na(scores)]
  if (length(scores) == 0) return(list(nps = NA, n = 0, prom = NA, neut = NA, detr = NA))
  promotores  <- sum(scores >= 9)
  neutros     <- sum(scores >= 7 & scores < 9)
  detratores  <- sum(scores < 7)
  n <- length(scores)
  list(
    nps  = round((promotores - detratores) / n * 100, 1),
    n    = n,
    prom = round(promotores / n * 100, 1),
    neut = round(neutros / n * 100, 1),
    detr = round(detratores / n * 100, 1)
  )
}

nps_resultado <- calcular_nps(df$nps_score)
cat("NPS:", nps_resultado$nps, "\n")
cat("  Promotores:", nps_resultado$prom, "%\n")
cat("  Neutros:", nps_resultado$neut, "%\n")
cat("  Detratores:", nps_resultado$detr, "%\n\n")

# --- Explodir checkbox ------------------------------------------------------
explodir_checkbox <- function(df, coluna) {
  df %>%
    filter(!is.na(.data[[coluna]])) %>%
    mutate(item = str_split(.data[[coluna]], ",\\s*")) %>%
    unnest(item) %>%
    mutate(item = str_trim(item)) %>%
    count(item, sort = TRUE)
}

if ("tipo_aplicacao" %in% names(df)) {
  tab_aplicacao <- explodir_checkbox(df, "tipo_aplicacao")
  cat("Tipos de aplicacao:\n"); print(tab_aplicacao); cat("\n")
}

if ("dificuldades" %in% names(df)) {
  tab_dificuldades <- explodir_checkbox(df, "dificuldades")
  cat("Dificuldades:\n"); print(tab_dificuldades); cat("\n")
}

# ============================================================================
# GRAFICOS
# ============================================================================

dir.create("output", showWarnings = FALSE)

# --- G1: Delta pre/pos (dumbbell) -------------------------------------------
df_prepos <- df %>%
  select(all_of(c(colunas_pre, colunas_pos))) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "media") %>%
  mutate(
    momento     = ifelse(str_starts(variavel, "pre_"), "Antes", "Depois"),
    competencia = str_remove(variavel, "^(pre_|pos_)"),
    competencia = competencias_labels[competencia]
  ) %>%
  pivot_wider(names_from = momento, values_from = media)

g1 <- ggplot(df_prepos, aes(y = fct_reorder(competencia, Depois))) +
  geom_segment(aes(x = Antes, xend = Depois, yend = competencia),
               color = COR_CINZA, linewidth = 1.2) +
  geom_point(aes(x = Antes), color = COR_LARANJA, size = 4) +
  geom_point(aes(x = Depois), color = COR_VERDE, size = 4) +
  geom_text(aes(x = Antes, label = round(Antes, 1)),
            nudge_y = 0.25, color = COR_LARANJA, size = 3.5, fontface = "bold") +
  geom_text(aes(x = Depois, label = round(Depois, 1)),
            nudge_y = 0.25, color = COR_VERDE, size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(title = "Autoavaliacao de competencias: antes vs. depois",
       subtitle = glue("n = {nrow(df)} respondentes"), x = "Nivel (1-5)", y = NULL,
       caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")

ggsave("output/g1_prepos.png", g1, width = 8, height = 4, dpi = 300, bg = "white")

# --- G2: Satisfacao ---------------------------------------------------------
df_sat <- df %>%
  select(all_of(colunas_sat)) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "media") %>%
  mutate(item = str_remove(item, "^sat_"),
         item = recode(item, "qualidade" = "Qualidade geral", "organizacao" = "Organizacao",
                       "didatica" = "Didatica", "suporte" = "Suporte tutores",
                       "carga" = "Carga horaria", "relevancia" = "Relevancia",
                       "material" = "Material didatico", "objetivos" = "Clareza dos objetivos"))

g2 <- ggplot(df_sat, aes(x = media, y = fct_reorder(item, media))) +
  geom_col(fill = COR_AZUL, width = 0.6) +
  geom_text(aes(label = round(media, 1)), hjust = -0.3, color = COR_AZUL, fontface = "bold", size = 3.5) +
  scale_x_continuous(limits = c(0, 5.5), breaks = 1:5) +
  labs(title = "Satisfacao com o curso", subtitle = glue("Media por item (1-5)  |  n = {nrow(df)}"),
       x = NULL, y = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")

ggsave("output/g2_satisfacao.png", g2, width = 8, height = 4.5, dpi = 300, bg = "white")

# --- G3: Metodologia --------------------------------------------------------
df_met <- df %>%
  select(all_of(colunas_met)) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "media") %>%
  mutate(item = str_remove(item, "^met_"),
         item = recode(item, "aprender" = "Aprender fazendo", "praticas" = "Atividades praticas",
                       "produtos" = "Produtos de aprendizagem", "integracao" = "Integracao teoria-pratica",
                       "tempo" = "Tempo para praticas", "simulacoes" = "Simulacoes realistas"))

g3 <- ggplot(df_met, aes(x = media, y = fct_reorder(item, media))) +
  geom_col(fill = COR_VERDE, width = 0.6) +
  geom_text(aes(label = round(media, 1)), hjust = -0.3, color = COR_VERDE, fontface = "bold", size = 3.5) +
  scale_x_continuous(limits = c(0, 5.5), breaks = 1:5) +
  labs(title = "Avaliacao da metodologia", subtitle = glue("Concordancia media (1-5)  |  n = {nrow(df)}"),
       x = NULL, y = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")

ggsave("output/g3_metodologia.png", g3, width = 8, height = 4, dpi = 300, bg = "white")

# --- G4: Impacto ------------------------------------------------------------
df_imp <- df %>%
  select(all_of(colunas_imp)) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "media") %>%
  mutate(item = str_remove(item, "^imp_"),
         item = recode(item, "resposta" = "Resposta a eventos", "qualidade" = "Qualidade do trabalho",
                       "vigilancia" = "Vigilancia local", "decisao" = "Tomada de decisao"))

g4 <- ggplot(df_imp, aes(x = media, y = fct_reorder(item, media))) +
  geom_col(fill = COR_LARANJA, width = 0.6) +
  geom_text(aes(label = round(media, 1)), hjust = -0.3, color = COR_LARANJA, fontface = "bold", size = 3.5) +
  scale_x_continuous(limits = c(0, 5.5), breaks = 1:5) +
  labs(title = "Percepcao de impacto", subtitle = glue("Contribuicao media (1-5)  |  n = {nrow(df)}"),
       x = NULL, y = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")

ggsave("output/g4_impacto.png", g4, width = 8, height = 3.5, dpi = 300, bg = "white")

# --- G5: NPS ----------------------------------------------------------------
df_nps_bar <- data.frame(
  categoria = factor(c("Detratores", "Neutros", "Promotores"),
                     levels = c("Detratores", "Neutros", "Promotores")),
  pct = c(nps_resultado$detr, nps_resultado$neut, nps_resultado$prom)
)

g5 <- ggplot(df_nps_bar, aes(x = pct, y = "", fill = categoria)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Detratores" = "#EF4444", "Neutros" = COR_CINZA, "Promotores" = COR_VERDE)) +
  labs(title = glue("Net Promoter Score: {nps_resultado$nps}"),
       subtitle = glue("n = {nps_resultado$n} respondentes"),
       x = "%", y = NULL, fill = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS") +
  theme(panel.grid.major.y = element_blank())

ggsave("output/g5_nps.png", g5, width = 8, height = 2.5, dpi = 300, bg = "white")

# --- G6: Aplicacao na rotina ------------------------------------------------
if ("aplicacao_rotina" %in% names(df)) {
  df_rotina <- df %>% count(aplicacao_rotina) %>% mutate(pct = round(n / sum(n) * 100, 1))
  g6 <- ggplot(df_rotina, aes(x = fct_reorder(aplicacao_rotina, -pct), y = pct)) +
    geom_col(fill = COR_AZUL, width = 0.6) +
    geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, color = COR_AZUL, fontface = "bold") +
    scale_y_continuous(limits = c(0, max(df_rotina$pct) * 1.2)) +
    labs(title = "Frequencia de aplicacao na rotina", subtitle = glue("n = {nrow(df)}"),
         x = NULL, y = "%", caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")
  ggsave("output/g6_aplicacao_rotina.png", g6, width = 7, height = 4, dpi = 300, bg = "white")
}

# --- G7: Tipo de aplicacao --------------------------------------------------
if (exists("tab_aplicacao") && nrow(tab_aplicacao) > 0) {
  g7 <- ggplot(tab_aplicacao, aes(x = n, y = fct_reorder(item, n))) +
    geom_col(fill = COR_VERDE, width = 0.6) +
    geom_text(aes(label = n), hjust = -0.3, color = COR_VERDE, fontface = "bold") +
    labs(title = "Tipos de aplicacao do aprendizado", subtitle = "Respostas multiplas",
         x = "Mencoes", y = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")
  ggsave("output/g7_tipo_aplicacao.png", g7, width = 8, height = 4, dpi = 300, bg = "white")
}

# --- G8: Dificuldades -------------------------------------------------------
if (exists("tab_dificuldades") && nrow(tab_dificuldades) > 0) {
  g8 <- ggplot(tab_dificuldades, aes(x = n, y = fct_reorder(item, n))) +
    geom_col(fill = "#EF4444", width = 0.6) +
    geom_text(aes(label = n), hjust = -0.3, color = "#EF4444", fontface = "bold") +
    labs(title = "Dificuldades enfrentadas", subtitle = "Respostas multiplas",
         x = "Mencoes", y = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")
  ggsave("output/g8_dificuldades.png", g8, width = 8, height = 4, dpi = 300, bg = "white")
}

# --- G9: Comparacao por turma -----------------------------------------------
if ("turma" %in% names(df) && n_distinct(df$turma, na.rm = TRUE) > 1) {
  df_turma <- df %>%
    group_by(turma) %>%
    summarise(n = n(), delta_medio = mean(delta_medio, na.rm = TRUE),
              satisfacao = mean(escore_satisfacao, na.rm = TRUE),
              metodologia = mean(escore_metodologia, na.rm = TRUE),
              impacto = mean(escore_impacto, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(-c(turma, n), names_to = "dimensao", values_to = "media") %>%
    mutate(dimensao = recode(dimensao, "delta_medio" = "Delta pre/pos",
                             "satisfacao" = "Satisfacao", "metodologia" = "Metodologia", "impacto" = "Impacto"))
  g9 <- ggplot(df_turma, aes(x = media, y = dimensao, fill = turma)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = round(media, 1)), position = position_dodge(width = 0.7),
              hjust = -0.2, size = 3, fontface = "bold") +
    scale_fill_manual(values = c(COR_AZUL, COR_VERDE, COR_LARANJA)) +
    scale_x_continuous(limits = c(0, 5.5)) +
    labs(title = "Comparacao entre turmas", subtitle = "Escores medios por dimensao",
         x = NULL, y = NULL, fill = NULL, caption = "Fonte: Avaliacao EpiSUS-Fundamental - 15a RS")
  ggsave("output/g9_turmas.png", g9, width = 9, height = 5, dpi = 300, bg = "white")
}

# ============================================================================
# TABELA RESUMO
# ============================================================================

resumo <- data.frame(
  Indicador = c("Respondentes (n)", "Idade media (DP)", "Delta medio pre/pos",
                "Escore satisfacao", "Escore metodologia", "Escore impacto", "NPS"),
  Valor = c(
    nrow(df),
    paste0(round(mean(df$idade, na.rm = TRUE), 1), " (", round(sd(df$idade, na.rm = TRUE), 1), ")"),
    round(mean(df$delta_medio, na.rm = TRUE), 2),
    round(mean(df$escore_satisfacao, na.rm = TRUE), 2),
    round(mean(df$escore_metodologia, na.rm = TRUE), 2),
    round(mean(df$escore_impacto, na.rm = TRUE), 2),
    nps_resultado$nps
  )
)

cat("\n===== RESUMO =====\n")
print(resumo, row.names = FALSE)

# ============================================================================
# GERAR BOLETIM (Quarto .qmd)
# ============================================================================

bt <- c(
  "---",
  "title: \"Boletim de Avaliacao - EpiSUS-Fundamental 15a RS\"",
  "format:",
  "  html:",
  "    embed-resources: true",
  "    theme: cosmo",
  "    toc: true",
  "    toc-title: \"Sumario\"",
  "---",
  "",
  "## Autoavaliacao pre vs. pos",
  "",
  "![](output/g1_prepos.png){width=100%}",
  "",
  "## Satisfacao com o curso",
  "",
  "![](output/g2_satisfacao.png){width=100%}",
  "",
  "## Avaliacao da metodologia",
  "",
  "![](output/g3_metodologia.png){width=100%}",
  "",
  "## Percepcao de impacto",
  "",
  "![](output/g4_impacto.png){width=100%}",
  "",
  "## Net Promoter Score",
  "",
  "![](output/g5_nps.png){width=100%}",
  "",
  "## Aplicacao na rotina",
  "",
  "![](output/g6_aplicacao_rotina.png){width=100%}",
  "",
  "## Tipos de aplicacao",
  "",
  "![](output/g7_tipo_aplicacao.png){width=100%}",
  "",
  "## Dificuldades enfrentadas",
  "",
  "![](output/g8_dificuldades.png){width=100%}",
  "",
  "---",
  "",
  paste0("*Gerado automaticamente em ", Sys.Date(), " - Secao de Vigilancia Epidemiologica, 15a RS de Maringa.*")
)

writeLines(bt, "boletim_episus.qmd")
cat("\nArquivo boletim_episus.qmd criado.\n")
cat("  Para renderizar: quarto render boletim_episus.qmd\n")

# ============================================================================
# EXPORTAR DADOS TRATADOS
# ============================================================================

writexl::write_xlsx(
  list(
    "Dados tratados"  = as.data.frame(df),
    "Resumo"          = resumo,
    "Tipos aplicacao"  = if (exists("tab_aplicacao")) tab_aplicacao else data.frame(),
    "Dificuldades"    = if (exists("tab_dificuldades")) tab_dificuldades else data.frame()
  ),
  "output/dados_tratados_episus.xlsx"
)

cat("Planilha tratada: output/dados_tratados_episus.xlsx\n")
cat("\n===== CONCLUIDO =====\n")
