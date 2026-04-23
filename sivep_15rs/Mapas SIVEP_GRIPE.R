# ============================================================
# SRAG por Bairro - 15ª Regional de Saúde (Maringá/PR)
# Fonte SRAG: SIVEP-GRIPE (.dbf) — arquivos SRAGHOSP
# Fonte população: IBGE/RIPSA - Estimativas 2025
# Shapefile municípios: C:/GIS/PR_Municipios_2024/PR_Municipios_2024.shp
# Shapefile bairros Maringá: C:/GIS/bairros/Bairros.shp
# Shapefile bairros Sarandi: C:/GIS/bairros_sarandi/Bairros_loteamentos.shp
# Municípios verificados em ibge_cnv_pop.csv
# Nomenclatura: Dicionário SIVEP-GRIPE 25/05/2023
# ============================================================


# 1. PACOTES -----------------------------------------------

pacotes_cran <- c(
  "dplyr", "tidyr", "stringr", "lubridate",
  "ggplot2", "scales", "forcats",
  "sf", "tmap",
  "writexl", "foreign"
)

for (pkg in pacotes_cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# 2. PARÂMETROS -------------------------------------------
# Altere aqui sem precisar mexer no resto do script

ANO_ANALISE            <- 2026   # ano de análise: 2019 a 2026
CORTE_NOME_BAIRRO      <- 5     # corte de casos para exibir nome no mapa — Maringá
CORTE_NOME_BAIRRO_SAR  <- 5      # corte de casos para exibir nome no mapa — Sarandi


# 3. MUNICÍPIOS E POPULAÇÃO DA 15ª RS ----------------------

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

cat("Municípios carregados:", nrow(municipios_15rs), "\n")
cat("População total 15ª RS (2025):",
    format(sum(municipios_15rs$populacao_2025), big.mark = ".", decimal.mark = ","), "\n")


# 4. LEITURA DO ARQUIVO SIVEP -----------------------------

pasta           <- "C:/SIVEPGRIPE/BaseDBF/"
caminho_arquivo <- paste0(pasta, "SRAGHOSP", ANO_ANALISE, ".dbf")

if (!file.exists(caminho_arquivo)) {
  stop("Arquivo não encontrado: ", caminho_arquivo)
}

cat("\nLendo:", basename(caminho_arquivo), "\n")
dados_brutos <- foreign::read.dbf(caminho_arquivo, as.is = TRUE)
cat("Registros lidos:", nrow(dados_brutos), "\n")

names(dados_brutos) <- toupper(names(dados_brutos))

campos_esperados <- c(
  "CO_MUN_RES", "NM_BAIRRO", "DT_NOTIFIC",
  "SEM_NOT", "CLASSI_FIN", "EVOLUCAO", "CS_ZONA", "UTI"
)
campos_ausentes <- setdiff(campos_esperados, names(dados_brutos))
if (length(campos_ausentes) > 0) {
  warning("Campos não encontrados: ", paste(campos_ausentes, collapse = ", "))
}


# 5. PARSE DE DATA ----------------------------------------

parseia_data <- function(x) {
  if (inherits(x, "Date")) return(x)
  d <- as.Date(as.character(x), format = "%d/%m/%Y")
  if (sum(!is.na(d)) > 0) return(d)
  as.Date(as.character(x), format = "%Y-%m-%d")
}

cat("\nClasse original de DT_NOTIFIC:", class(dados_brutos$DT_NOTIFIC), "\n")
print(head(dados_brutos$DT_NOTIFIC, 5))

dados_brutos <- dados_brutos %>%
  mutate(DT_NOTIFIC_PARSED = parseia_data(DT_NOTIFIC))

na_datas <- sum(is.na(dados_brutos$DT_NOTIFIC_PARSED))
cat("Datas com NA após parse:", na_datas,
    "(", round(na_datas / nrow(dados_brutos) * 100, 1), "%)\n")

if (na_datas == nrow(dados_brutos)) {
  stop("Parse de data falhou. Cole: head(dados_brutos$DT_NOTIFIC, 5)")
}


# 6. FILTROS E LIMPEZA ------------------------------------

dados <- dados_brutos %>%
  mutate(CO_MUN_RES = as.integer(CO_MUN_RES)) %>%
  filter(CO_MUN_RES %in% municipios_15rs$codigo_ibge_6) %>%
  mutate(
    DT_NOTIFIC = DT_NOTIFIC_PARSED,
    ANO        = lubridate::year(DT_NOTIFIC),
    SEM_EPI    = as.integer(SEM_NOT),
    BAIRRO = NM_BAIRRO %>%
      str_to_upper() %>%
      str_trim() %>%
      str_squish() %>%
      str_replace_all("[^A-ZÁÉÍÓÚÂÊÎÔÛÃÕÀÈÌÒÙÇ0-9 ]", "") %>%
      na_if("") %>%
      na_if("NAO INFORMADO") %>%
      na_if("IGNORADO") %>%
      na_if("SEM INFORMACAO") %>%
      na_if("SEM BAIRRO"),
    CLASSIFICACAO = case_when(
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

cat("\nRegistros na 15ª RS:", nrow(dados), "\n")
cat("ANO preenchido:", sum(!is.na(dados$ANO)), "\n")
cat("SEM_EPI preenchido:", sum(!is.na(dados$SEM_EPI)), "\n")

sem_bairro <- sum(is.na(dados$BAIRRO))
cat("Sem bairro:", sem_bairro,
    "(", round(sem_bairro / nrow(dados) * 100, 1), "%)\n")


# 7. ANÁLISE POR MUNICÍPIO --------------------------------

casos_municipio <- dados %>%
  group_by(CO_MUN_RES) %>%
  summarise(
    casos         = n(),
    obitos_srag   = sum(OBITO_SRAG,   na.rm = TRUE),
    obitos_outras = sum(OBITO_OUTRAS, na.rm = TRUE),
    uti           = sum(UTI_SIM,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    municipios_15rs %>% select(codigo_ibge_6, municipio, populacao_2025),
    by = c("CO_MUN_RES" = "codigo_ibge_6")
  ) %>%
  mutate(
    incidencia_100k  = round(casos       / populacao_2025 * 100000, 1),
    mortalidade_100k = round(obitos_srag / populacao_2025 * 100000, 1),
    letalidade_pct   = round(obitos_srag / casos          * 100,    1)
  ) %>%
  arrange(desc(incidencia_100k))


# 8. ANÁLISE POR BAIRRO — Maringá -------------------------

cod_maringa <- 411520

casos_bairro <- dados %>%
  filter(CO_MUN_RES == cod_maringa, !is.na(BAIRRO)) %>%
  group_by(BAIRRO) %>%
  summarise(
    casos       = n(),
    obitos_srag = sum(OBITO_SRAG, na.rm = TRUE),
    uti         = sum(UTI_SIM,    na.rm = TRUE),
    letalidade  = round(obitos_srag / casos * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(casos))

casos_bairro_sem <- dados %>%
  filter(CO_MUN_RES == cod_maringa, !is.na(BAIRRO), !is.na(SEM_EPI)) %>%
  group_by(BAIRRO, SEM_EPI) %>%
  summarise(casos = n(), .groups = "drop")


# 8b. ANÁLISE POR BAIRRO — Sarandi ------------------------

cod_sarandi <- 412625

casos_bairro_sar <- dados %>%
  filter(CO_MUN_RES == cod_sarandi, !is.na(BAIRRO)) %>%
  group_by(BAIRRO) %>%
  summarise(
    casos       = n(),
    obitos_srag = sum(OBITO_SRAG, na.rm = TRUE),
    uti         = sum(UTI_SIM,    na.rm = TRUE),
    letalidade  = round(obitos_srag / casos * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(casos))

casos_bairro_sem_sar <- dados %>%
  filter(CO_MUN_RES == cod_sarandi, !is.na(BAIRRO), !is.na(SEM_EPI)) %>%
  group_by(BAIRRO, SEM_EPI) %>%
  summarise(casos = n(), .groups = "drop")

cat("\nTop 10 bairros de Sarandi:\n")
print(head(casos_bairro_sar, 10))


# 9. SÉRIE TEMPORAL (15ª RS) ------------------------------

casos_semana <- dados %>%
  filter(!is.na(SEM_EPI)) %>%
  group_by(SEM_EPI) %>%
  summarise(casos = n(), .groups = "drop") %>%
  arrange(SEM_EPI)

casos_semana_class <- dados %>%
  filter(!is.na(SEM_EPI)) %>%
  group_by(SEM_EPI, CLASSIFICACAO) %>%
  summarise(casos = n(), .groups = "drop") %>%
  arrange(SEM_EPI)


# 10. GRÁFICOS --------------------------------------------

dir.create("graficos", showWarnings = FALSE)
dir.create("tabelas",  showWarnings = FALSE)

titulo_ano <- paste0("| ", ANO_ANALISE)

# 10.1 Top 20 bairros — Maringá
g1 <- casos_bairro %>%
  head(20) %>%
  mutate(BAIRRO = fct_reorder(BAIRRO, casos)) %>%
  ggplot(aes(x = casos, y = BAIRRO)) +
  geom_col(fill = "#2166ac") +
  geom_text(aes(label = casos), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste("Top 20 Bairros — Casos de SRAG (Maringá/PR)", titulo_ano),
    subtitle = "Fonte: SIVEP-GRIPE | Residência do paciente",
    x = "Casos notificados", y = NULL
  ) +
  theme_minimal(base_size = 11)

print(g1)
ggsave(paste0("graficos/srag_top20_bairros_maringa_", ANO_ANALISE, ".png"),
       g1, width = 10, height = 7, dpi = 150)


# 10.1b Top 20 bairros — Sarandi
g1b <- casos_bairro_sar %>%
  head(20) %>%
  mutate(BAIRRO = fct_reorder(BAIRRO, casos)) %>%
  ggplot(aes(x = casos, y = BAIRRO)) +
  geom_col(fill = "#1b7837") +
  geom_text(aes(label = casos), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste("Top 20 Bairros — Casos de SRAG (Sarandi/PR)", titulo_ano),
    subtitle = "Fonte: SIVEP-GRIPE | Residência do paciente",
    x = "Casos notificados", y = NULL
  ) +
  theme_minimal(base_size = 11)

print(g1b)
ggsave(paste0("graficos/srag_top20_bairros_sarandi_", ANO_ANALISE, ".png"),
       g1b, width = 10, height = 7, dpi = 150)


# 10.2 Incidência por município
g2 <- casos_municipio %>%
  mutate(municipio = fct_reorder(str_to_title(municipio), incidencia_100k)) %>%
  ggplot(aes(x = incidencia_100k, y = municipio)) +
  geom_col(fill = "#d6604d") +
  geom_text(aes(label = incidencia_100k), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste("Incidência de SRAG por Município", titulo_ano),
    subtitle = "Taxa por 100 mil hab. | 15ª RS Maringá | Pop. IBGE 2025",
    x = "Casos por 100.000 habitantes", y = NULL
  ) +
  theme_minimal(base_size = 11)

print(g2)
ggsave(paste0("graficos/srag_incidencia_municipio_", ANO_ANALISE, ".png"),
       g2, width = 10, height = 8, dpi = 150)


# 10.3 Série temporal por semana epidemiológica
g3 <- casos_semana %>%
  ggplot(aes(x = SEM_EPI, y = casos)) +
  geom_line(color = "#2166ac", linewidth = 0.8) +
  geom_point(color = "#2166ac", size = 1.5) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    title    = paste("Casos de SRAG por Semana Epidemiológica", titulo_ano),
    subtitle = "15ª RS Maringá | Fonte: SIVEP-GRIPE",
    x = "Semana epidemiológica", y = "Casos notificados"
  ) +
  theme_minimal(base_size = 11)

print(g3)
ggsave(paste0("graficos/srag_serie_temporal_", ANO_ANALISE, ".png"),
       g3, width = 12, height = 5, dpi = 150)


# 10.4 Série por classificação etiológica
g4 <- casos_semana_class %>%
  ggplot(aes(x = SEM_EPI, y = casos, color = CLASSIFICACAO, group = CLASSIFICACAO)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = paste("SRAG por Classificação Etiológica e Semana", titulo_ano),
    subtitle = "15ª RS Maringá | Fonte: SIVEP-GRIPE",
    x = "Semana epidemiológica", y = "Casos notificados", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g4)
ggsave(paste0("graficos/srag_serie_classificacao_", ANO_ANALISE, ".png"),
       g4, width = 12, height = 5, dpi = 150)


# 10.5 Heatmap bairro x semana — Maringá (top 15)
top15_bairros <- head(casos_bairro$BAIRRO, 15)

totais_bairro <- casos_bairro %>%
  filter(BAIRRO %in% top15_bairros) %>%
  mutate(LABEL = paste0(BAIRRO, " (", casos, ")")) %>%
  select(BAIRRO, LABEL, casos)

g5 <- casos_bairro_sem %>%
  filter(BAIRRO %in% top15_bairros) %>%
  left_join(totais_bairro %>% select(BAIRRO, LABEL), by = "BAIRRO") %>%
  mutate(LABEL = fct_reorder(LABEL, casos, sum)) %>%
  ggplot(aes(x = SEM_EPI, y = LABEL, fill = casos)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    title    = paste("SRAG por Bairro e Semana Epidemiológica — Maringá", titulo_ano),
    subtitle = "Top 15 bairros | Total acumulado entre parênteses | Fonte: SIVEP-GRIPE",
    x = "Semana epidemiológica", y = NULL, fill = "Casos\nna semana"
  ) +
  theme_minimal(base_size = 11)

print(g5)
ggsave(paste0("graficos/srag_heatmap_bairro_semana_maringa_", ANO_ANALISE, ".png"),
       g5, width = 14, height = 6, dpi = 150)


# 10.5b Heatmap bairro x semana — Sarandi (top 15)
top15_bairros_sar <- head(casos_bairro_sar$BAIRRO, 15)

totais_bairro_sar <- casos_bairro_sar %>%
  filter(BAIRRO %in% top15_bairros_sar) %>%
  mutate(LABEL = paste0(BAIRRO, " (", casos, ")")) %>%
  select(BAIRRO, LABEL, casos)

g5b <- casos_bairro_sem_sar %>%
  filter(BAIRRO %in% top15_bairros_sar) %>%
  left_join(totais_bairro_sar %>% select(BAIRRO, LABEL), by = "BAIRRO") %>%
  mutate(LABEL = fct_reorder(LABEL, casos, sum)) %>%
  ggplot(aes(x = SEM_EPI, y = LABEL, fill = casos)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    title    = paste("SRAG por Bairro e Semana Epidemiológica — Sarandi", titulo_ano),
    subtitle = "Top 15 bairros | Total acumulado entre parênteses | Fonte: SIVEP-GRIPE",
    x = "Semana epidemiológica", y = NULL, fill = "Casos\nna semana"
  ) +
  theme_minimal(base_size = 11)

print(g5b)
ggsave(paste0("graficos/srag_heatmap_bairro_semana_sarandi_", ANO_ANALISE, ".png"),
       g5b, width = 14, height = 6, dpi = 150)


# 11. MAPA POR MUNICÍPIO — shapefile IBGE 2024 ------------

caminho_shp_municipios <- "C:/GIS/PR_Municipios_2024/PR_Municipios_2024.shp"

if (!file.exists(caminho_shp_municipios)) {
  warning("Shapefile de municípios não encontrado. Ajuste o caminho acima.")
} else {
  
  cat("\nLendo shapefile de municípios...\n")
  malha_pr <- sf::st_read(caminho_shp_municipios, quiet = TRUE)
  
  malha_15rs <- malha_pr %>%
    mutate(CO_MUN_6 = floor(as.integer(CD_MUN) / 10)) %>%
    filter(CO_MUN_6 %in% municipios_15rs$codigo_ibge_6) %>%
    left_join(casos_municipio, by = c("CO_MUN_6" = "CO_MUN_RES"))
  
  cat("Municípios carregados (15ª RS):", nrow(malha_15rs), "\n")
  
  tmap_mode("plot")
  
  mapa_casos <- tm_shape(malha_15rs) +
    tm_polygons(
      fill        = "casos",
      fill.scale  = tm_scale_continuous(values = "brewer.blues"),
      fill.legend = tm_legend(title = "Casos de SRAG"),
      col         = "white",
      lwd         = 0.5
    ) +
    tm_text("NM_MUN", size = 0.45, col = "grey20") +
    tm_title(paste("SRAG — Casos por Município\n15ª RS Maringá/PR |", ANO_ANALISE)) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  tmap_save(mapa_casos,
            paste0("graficos/mapa_srag_casos_", ANO_ANALISE, ".png"),
            width = 2400, height = 2000)
  
  mapa_incid <- tm_shape(malha_15rs) +
    tm_polygons(
      fill        = "incidencia_100k",
      fill.scale  = tm_scale_continuous(values = "brewer.yl_or_rd"),
      fill.legend = tm_legend(title = "Casos/100 mil hab."),
      col         = "white",
      lwd         = 0.5
    ) +
    tm_text("NM_MUN", size = 0.45, col = "grey20") +
    tm_title(paste("SRAG — Incidência\n15ª RS Maringá/PR |",
                   ANO_ANALISE, "| Pop. IBGE 2025")) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  tmap_save(mapa_incid,
            paste0("graficos/mapa_srag_incidencia_", ANO_ANALISE, ".png"),
            width = 2400, height = 2000)
  
  print(mapa_casos)
  print(mapa_incid)
  
  cat("Mapas por município salvos.\n")
}


# 12 / 12b. MAPA POR BAIRRO — função reutilizável --------

gera_mapa_bairro <- function(bairros_geo,
                             casos_bairro_mun,
                             col_nome,
                             de_para,
                             corte,
                             nome_municipio,
                             ano) {
  
  normaliza <- function(x) {
    x %>%
      str_to_upper() %>% str_trim() %>% str_squish() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_replace_all("[^A-Z0-9 ]", "") %>%
      str_squish()
  }
  
  bairros_geo <- bairros_geo %>% mutate(JOIN_KEY = normaliza(.data[[col_nome]]))
  casos_join  <- casos_bairro_mun %>% mutate(JOIN_KEY = normaliza(BAIRRO))
  
  lookup <- bind_rows(
    casos_join %>%
      rename(SHP_KEY = JOIN_KEY) %>%
      select(SHP_KEY, casos, obitos_srag, uti, letalidade),
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
  
  mapa_dados <- bairros_geo %>%
    left_join(lookup, by = c("JOIN_KEY" = "SHP_KEY"))
  
  com_dados <- sum(!is.na(mapa_dados$casos))
  cat("Bairros com casos (", nome_municipio, "):", com_dados, "\n")
  
  sem_match <- casos_join %>%
    filter(!JOIN_KEY %in% lookup$SHP_KEY,
           !JOIN_KEY %in% de_para$SIVEP_KEY) %>%
    arrange(desc(casos)) %>% head(10)
  
  if (nrow(sem_match) > 0) {
    cat("Bairros do SIVEP sem correspondência no shapefile:\n")
    print(sem_match %>% select(BAIRRO, JOIN_KEY, casos))
  }
  
  bairros_nome <- mapa_dados %>% filter(!is.na(casos) & casos >= corte)
  cat("Bairros com nome exibido (>=", corte, "casos):", nrow(bairros_nome), "\n")
  
  mapa <- tm_shape(mapa_dados) +
    tm_polygons(
      fill        = "casos",
      fill.scale  = tm_scale_continuous(
        values   = "brewer.yl_or_rd",
        value.na = "grey90"
      ),
      fill.legend = tm_legend(title = "Casos de SRAG"),
      col         = "grey40",
      lwd         = 0.5
    ) +
    tm_title(paste0("SRAG por Bairro — ", nome_municipio, "/PR | ", ano,
                    "\nNomes exibidos: >= ", corte, " casos")) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scalebar(position = c("left", "bottom"))
  
  if (nrow(bairros_nome) > 0) {
    mapa <- mapa +
      tm_shape(bairros_nome) +
      tm_text(
        text           = col_nome,
        size           = 0.35,
        col            = "grey10",
        fontface       = "bold",
        remove.overlap = TRUE
      )
  } else {
    cat("Nenhum bairro atingiu o corte — nomes não exibidos.\n")
    cat("Dica: reduza CORTE_NOME_BAIRRO no bloco 2.\n")
  }
  
  mapa
}


# 12. MAPA — Maringá --------------------------------------

de_para_maringa <- tibble::tribble(
  ~SHP_KEY,                                                ~SIVEP_KEY,
  "JARDIM ALVORADA I PARTE",                               "JARDIM ALVORADA",
  "JARDIM ALVORADA II PARTE",                              "JARDIM ALVORADA",
  "SUB LT 77A71 JARDIM ALVORADA III",                      "JARDIM ALVORADA",
  "JARDIM ALVORADA I PARTE",                               "ALVORADA",
  "JARDIM ALVORADA II PARTE",                              "ALVORADA",
  "SUB LT 77A71 JARDIM ALVORADA III",                      "ALVORADA",
  "CONJUNTO HABITACIONAL REQUIAO I 1 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 2 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 3 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "CONJUNTO HABITACIONAL REQUIAO I 4 PARTE",               "CONJUNTO HABITACIONAL REQUIAO",
  "PARQUE ITAIPU I PARTE",                                 "JARDIM ITAIPU",
  "PARQUE ITAIPU II PARTE",                                "JARDIM ITAIPU",
  "CONJUNTO RESIDENCIAL INOCENTE VILA NOVA JR BORBA GATO", "CONJUNTO HABITACIONAL INOCENTE",
  "PARQUE HORTENCIA I PARTE",                              "PARQUE HORTENCIA",
  "PARQUE HORTENCIA II PARTE",                             "PARQUE HORTENCIA",
  "LOTEAMENTO LIBERDADE I PARTE",                          "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE II PARTE",                         "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE III PARTE",                        "JARDIM LIBERDADE",
  "LOTEAMENTO LIBERDADE IV PARTE",                         "JARDIM LIBERDADE",
  "CONJUNTO CIDADE ALTA",                                  "CONJUNTO RESIDENCIAL CIDADE AL"
)

caminho_shp_bairros <- "C:/GIS/bairros/Bairros.shp"

if (!file.exists(caminho_shp_bairros)) {
  warning("Shapefile de bairros de Maringá não encontrado.")
} else {
  cat("\nGerando mapa de bairros — Maringá...\n")
  bairros_mar <- sf::st_read(caminho_shp_bairros, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)
  
  tmap_mode("plot")
  
  mapa_mar <- gera_mapa_bairro(
    bairros_geo      = bairros_mar,
    casos_bairro_mun = casos_bairro,
    col_nome         = "NOME",
    de_para          = de_para_maringa,
    corte            = CORTE_NOME_BAIRRO,
    nome_municipio   = "Maringá",
    ano              = ANO_ANALISE
  )
  
  print(mapa_mar)
  tmap_save(mapa_mar,
            paste0("graficos/mapa_srag_bairro_maringa_", ANO_ANALISE, ".png"),
            width = 2400, height = 2400)
  cat("Mapa de Maringá salvo.\n")
}


# 12b. MAPA — Sarandi -------------------------------------
# de_para inicialmente vazia — expanda após verificar os bairros sem match

de_para_sarandi <- tibble::tribble(
  ~SHP_KEY,   ~SIVEP_KEY
  # exemplo:
  # "JARDIM EXEMPLO I PARTE",  "JARDIM EXEMPLO",
)

caminho_shp_sarandi <- "C:/GIS/bairros_sarandi/Bairros_loteamentos.shp"

if (!file.exists(caminho_shp_sarandi)) {
  warning("Shapefile de bairros de Sarandi não encontrado.")
} else {
  cat("\nGerando mapa de bairros — Sarandi...\n")
  bairros_sar <- sf::st_read(caminho_shp_sarandi, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)
  
  tmap_mode("plot")
  
  mapa_sar <- gera_mapa_bairro(
    bairros_geo      = bairros_sar,
    casos_bairro_mun = casos_bairro_sar,
    col_nome         = "Bairro",
    de_para          = de_para_sarandi,
    corte            = CORTE_NOME_BAIRRO_SAR,
    nome_municipio   = "Sarandi",
    ano              = ANO_ANALISE
  )
  
  print(mapa_sar)
  tmap_save(mapa_sar,
            paste0("graficos/mapa_srag_bairro_sarandi_", ANO_ANALISE, ".png"),
            width = 2400, height = 2400)
  cat("Mapa de Sarandi salvo.\n")
}


# 13. EXPORTAÇÃO EXCEL ------------------------------------

writexl::write_xlsx(
  list(
    "municipios_15rs"         = municipios_15rs,
    "casos_municipio"         = casos_municipio,
    "casos_bairro_maringa"    = casos_bairro,
    "casos_bairro_sarandi"    = casos_bairro_sar,
    "serie_temporal"          = casos_semana,
    "serie_por_classificacao" = casos_semana_class
  ),
  paste0("tabelas/srag_15rs_", ANO_ANALISE, ".xlsx")
)

cat("\nConcluído. Arquivos em ./tabelas/ e ./graficos/\n")