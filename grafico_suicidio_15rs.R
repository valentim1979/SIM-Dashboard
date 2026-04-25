# =============================================================================
# ANÁLISE DE SUICÍDIO — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# SIM/DATASUS | Seção de Vigilância Epidemiológica — SCVGE
# =============================================================================

library(read.dbc)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)

# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÕES
# -----------------------------------------------------------------------------

ANO  <- 2025
base <- "/Users/valentimsalajunior/Desktop/R_SIM"
dir_saida <- file.path(base, "SIM", "outputs")
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

cor_masc     <- "#2980B9"
cor_fem      <- "#C0392B"
cor_primaria <- "#004561"

rodape <- paste0(
  "Fonte: SIM/DATASUS, ", ANO,
  " | CID-10: X60–X84 (lesão autoprovocada intencional)\n",
  "Elaboração: SCVGE — 15ª RS"
)

# -----------------------------------------------------------------------------
# 1. FAIXAS ETÁRIAS PARA SUICÍDIO
# Função nomeada de forma única para não colidir com o script principal
# DEFINIDA SEMPRE — independente de sim_nf existir
# -----------------------------------------------------------------------------

niveis_faixa_sui <- c(
  "< 10 anos", "10–14 anos", "15–19 anos", "20–24 anos",
  "25–29 anos", "30–39 anos", "40–49 anos", "50–59 anos",
  "60–69 anos", "70 anos e mais"
)

faixa_sui_fn <- function(x) {
  dplyr::case_when(
    is.na(x)  ~ NA_character_,
    x < 10    ~ "< 10 anos",
    x <= 14   ~ "10–14 anos",
    x <= 19   ~ "15–19 anos",
    x <= 24   ~ "20–24 anos",
    x <= 29   ~ "25–29 anos",
    x <= 39   ~ "30–39 anos",
    x <= 49   ~ "40–49 anos",
    x <= 59   ~ "50–59 anos",
    x <= 69   ~ "60–69 anos",
    TRUE      ~ "70 anos e mais"
  )
}

# Decodificação de IDADE — as.character direto (campo é factor no DBC)
decodificar_idade <- function(idade_raw) {
  idade_str <- str_pad(as.character(idade_raw), 3, "left", "0")
  unid <- as.integer(substr(idade_str, 1, 1))
  qtd  <- as.integer(substr(idade_str, 2, 3))
  dplyr::case_when(
    unid == 4 ~ as.numeric(qtd),
    unid == 5 ~ as.numeric(100 + qtd),
    unid %in% c(1, 2, 3) ~ 0,
    TRUE ~ NA_real_
  )
}

# -----------------------------------------------------------------------------
# 2. LEITURA DO DBC
# Sempre relê o arquivo para garantir IDADE decodificado corretamente
# -----------------------------------------------------------------------------

municipios_15rs_cod6 <- c(
  "410115","410210","410220","410590","410730","410780","410790","410810",
  "411000","411090","411110","411160","411360","411410","411420","411480",
  "411520","411630","411640","411690","411740","411750","411810","412040",
  "412340","412360","412450","412530","412625","412830"
)

message("Lendo DBC: DOPR", ANO, ".dbc")
sim_raw <- read.dbc(
  file.path(base, "SIM", "DBF", paste0("DOPR", ANO, ".dbc"))
)
names(sim_raw) <- toupper(names(sim_raw))

sim_prep <- sim_raw %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>%
  filter(
    CODMUNRES %in% municipios_15rs_cod6,
    as.character(TIPOBITO) == "2"
  ) %>%
  mutate(
    CAUSABAS   = toupper(trimws(as.character(CAUSABAS))),
    codigo3    = substr(CAUSABAS, 1, 3),
    SEXO       = as.character(SEXO),
    sexo_label = dplyr::case_when(
      SEXO %in% c("M","1") ~ "Masculino",
      SEXO %in% c("F","2") ~ "Feminino",
      TRUE                 ~ "Ignorado"
    ),
    CIRCOBITO  = if ("CIRCOBITO" %in% names(.)) as.character(CIRCOBITO)
    else NA_character_,
    # IDADE decodificado via as.character — nunca as.integer
    idade_anos = decodificar_idade(IDADE),
    # Faixa etária com função local exclusiva deste script
    faixa_sui  = factor(faixa_sui_fn(idade_anos), levels = niveis_faixa_sui)
  ) %>%
  filter(!is.na(CAUSABAS), CAUSABAS != "", CAUSABAS != "*")

message("Registros 15ª RS: ", nrow(sim_prep))

# Diagnóstico
cat("\nDistribuição de idade_anos:\n")
print(summary(sim_prep$idade_anos))
cat("\nFaixas nos dados (todos):\n")
print(table(sim_prep$faixa_sui, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 3. FILTRAR E CLASSIFICAR SUICÍDIOS
# -----------------------------------------------------------------------------

classificar_meio <- function(cid3) {
  num <- suppressWarnings(as.integer(substr(cid3, 2, 3)))
  dplyr::case_when(
    substr(cid3,1,1) == "X" & num >= 60 & num <= 69 ~
      "Autointoxicação\n(substâncias/drogas)",
    cid3 == "X70" ~ "Enforcamento /\nestrangulamento",
    cid3 == "X71" ~ "Afogamento",
    cid3 %in% c("X72","X73","X74") ~ "Arma de fogo",
    cid3 == "X78" ~ "Objeto cortante /\npenetrante",
    cid3 == "X80" ~ "Precipitação de\nlugar elevado",
    cid3 %in% c("X75","X76","X77","X79","X81","X82","X83","X84") ~
      "Outros meios",
    TRUE ~ NA_character_
  )
}

sui <- sim_prep %>%
  filter(
    (substr(CAUSABAS,1,1) == "X" &
       suppressWarnings(as.integer(substr(CAUSABAS,2,3))) >= 60 &
       suppressWarnings(as.integer(substr(CAUSABAS,2,3))) <= 84) |
      (!is.na(CIRCOBITO) & CIRCOBITO == "2")
  ) %>%
  mutate(meio = classificar_meio(codigo3)) %>%
  filter(sexo_label != "Ignorado", !is.na(meio), !is.na(faixa_sui))

n_sui   <- nrow(sui)
n_sui_m <- sum(sui$sexo_label == "Masculino")
n_sui_f <- sum(sui$sexo_label == "Feminino")

message("\nTotal de suicídios: ", n_sui,
        " | M=", n_sui_m, " | F=", n_sui_f)

cat("\nFaixas etárias nos suicídios:\n")
print(table(sui$faixa_sui, useNA = "ifany"))

if (n_sui == 0) stop("Nenhum registro de suicídio encontrado.")

# Ordem dos meios por frequência total (mais frequente no topo = último nível)
ordem_meios <- sui %>% count(meio, sort = FALSE) %>%
  arrange(n) %>% pull(meio)

# -----------------------------------------------------------------------------
# 4. GRÁFICO 1 — PIRÂMIDE ETÁRIA
# -----------------------------------------------------------------------------

pir <- sui %>%
  count(faixa_sui, sexo_label, name = "n") %>%
  complete(faixa_sui, sexo_label, fill = list(n = 0)) %>%
  group_by(sexo_label) %>%
  mutate(prop = ifelse(sum(n) > 0, round(n / sum(n) * 100, 1), 0)) %>%
  ungroup() %>%
  mutate(
    n_plot    = if_else(sexo_label == "Masculino", -n, n),
    hjust_lbl = if_else(sexo_label == "Masculino", 1, 0),
    lbl_x     = if_else(sexo_label == "Masculino",
                        n_plot - 0.4, n_plot + 0.4),
    label     = if_else(n > 0, paste0(n, " (", prop, "%)"), "")
  )

lim_pir <- max(abs(pir$n_plot), na.rm = TRUE) * 1.45
if (lim_pir == 0) lim_pir <- 5

g1 <- ggplot(pir, aes(x = n_plot, y = faixa_sui, fill = sexo_label)) +
  geom_col(width = 0.72, alpha = 0.92) +
  geom_text(aes(x = lbl_x, label = label, hjust = hjust_lbl),
            size = 3, color = "grey20") +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  scale_x_continuous(
    limits = c(-lim_pir, lim_pir),
    labels = function(x) format(abs(x), big.mark = ".")
  ) +
  scale_fill_manual(
    values = c("Masculino" = cor_masc, "Feminino" = cor_fem),
    guide  = "none"
  ) +
  annotate("text",
           x = -lim_pir * 0.55, y = length(niveis_faixa_sui) + 0.9,
           label = paste0("Masculino\nN = ", n_sui_m),
           hjust = 0.5, size = 3.8, color = cor_masc,
           fontface = "bold", lineheight = 1.1) +
  annotate("text",
           x =  lim_pir * 0.55, y = length(niveis_faixa_sui) + 0.9,
           label = paste0("Feminino\nN = ", n_sui_f),
           hjust = 0.5, size = 3.8, color = cor_fem,
           fontface = "bold", lineheight = 1.1) +
  coord_cartesian(clip = "off",
                  ylim = c(0.5, length(niveis_faixa_sui) + 1.8)) +
  labs(
    title    = paste0("Suicídios por faixa etária e sexo  |  N = ", n_sui),
    subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
    x = "N de óbitos", y = NULL,
    caption = rodape
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = cor_primaria, hjust = 0),
    plot.subtitle      = element_text(size = 9.5, color = "grey40",
                                      margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, color = "grey55",
                                      hjust = 0, lineheight = 1.3,
                                      margin = margin(t = 10)),
    axis.text.y        = element_text(size = 10, face = "bold"),
    axis.text.x        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    plot.margin        = margin(14, 60, 14, 14)
  )

ggsave(
  file.path(dir_saida, paste0("suicidio_g1_piramide_", ANO, ".png")),
  g1, width = 12, height = 8, dpi = 200, bg = "white"
)
message("G1 salvo.")

# -----------------------------------------------------------------------------
# 5. GRÁFICO 2 — MÉTODO POR SEXO
# -----------------------------------------------------------------------------

meio_sexo <- sui %>%
  count(meio, sexo_label, name = "n") %>%
  complete(meio, sexo_label, fill = list(n = 0)) %>%
  group_by(sexo_label) %>%
  mutate(prop = ifelse(sum(n) > 0, round(n / sum(n) * 100, 1), 0)) %>%
  ungroup() %>%
  mutate(meio = factor(meio, levels = ordem_meios))

g2 <- ggplot(meio_sexo, aes(x = n, y = meio, fill = sexo_label)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.68, alpha = 0.92) +
  geom_text(
    aes(label = if_else(n > 0, paste0(n, "\n(", prop, "%)"), ""),
        group = sexo_label),
    position = position_dodge(width = 0.75),
    hjust = -0.08, size = 2.8, color = "grey25", lineheight = 0.9
  ) +
  scale_fill_manual(
    values = c("Masculino" = cor_masc, "Feminino" = cor_fem),
    name   = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.40))) +
  labs(
    title    = paste0("Método de suicídio por sexo  |  N = ", n_sui),
    subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO),
    x = "N de óbitos", y = NULL,
    caption = rodape
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = cor_primaria, hjust = 0),
    plot.subtitle      = element_text(size = 9.5, color = "grey40",
                                      margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, color = "grey55",
                                      hjust = 0, lineheight = 1.3,
                                      margin = margin(t = 10)),
    axis.text.y        = element_text(size = 10),
    axis.text.x        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    plot.margin        = margin(14, 14, 14, 14)
  )

ggsave(
  file.path(dir_saida, paste0("suicidio_g2_metodo_sexo_", ANO, ".png")),
  g2, width = 12, height = 8, dpi = 200, bg = "white"
)
message("G2 salvo.")

# -----------------------------------------------------------------------------
# 6. GRÁFICO 3 — MAPA DE CALOR FAIXA ETÁRIA × MÉTODO
# -----------------------------------------------------------------------------

heat <- sui %>%
  count(faixa_sui, meio, name = "n") %>%
  complete(faixa_sui, meio, fill = list(n = 0)) %>%
  mutate(
    meio      = factor(meio, levels = ordem_meios),
    label_txt = if_else(n > 0, as.character(n), "")
  )

g3 <- ggplot(heat, aes(x = faixa_sui, y = meio, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label_txt),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient(
    low  = "#D6EAF8",
    high = cor_primaria,
    name = "N de óbitos",
    guide = guide_colorbar(
      barwidth = 10, barheight = 0.6,
      title.position = "top", title.hjust = 0.5
    )
  ) +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  labs(
    title    = paste0("Método de suicídio por faixa etária  |  N = ", n_sui),
    subtitle = paste0("15ª RS | SIM/DATASUS, ", ANO, " | Ambos os sexos"),
    x = NULL, y = NULL,
    caption = rodape
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13,
                                   color = cor_primaria, hjust = 0),
    plot.subtitle   = element_text(size = 9.5, color = "grey40",
                                   margin = margin(b = 8)),
    plot.caption    = element_text(size = 7.5, color = "grey55",
                                   hjust = 0, lineheight = 1.3,
                                   margin = margin(t = 10)),
    axis.text.x     = element_text(size = 9.5),
    axis.text.y     = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "bottom",
    legend.title    = element_text(size = 9),
    plot.margin     = margin(14, 14, 14, 14)
  )

ggsave(
  file.path(dir_saida, paste0("suicidio_g3_calor_", ANO, ".png")),
  g3, width = 14, height = 8, dpi = 200, bg = "white"
)
message("G3 salvo.")

# -----------------------------------------------------------------------------
# 7. EXCEL
# -----------------------------------------------------------------------------

wb <- createWorkbook()
estilo_cab <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#004561",
  halign = "LEFT", textDecoration = "bold"
)

abas <- list(
  list("Método x Sexo",
       sui %>% count(meio, sexo_label, name = "n") %>%
         group_by(sexo_label) %>%
         mutate(prop = round(n / sum(n) * 100, 1)) %>% ungroup() %>%
         arrange(desc(n)) %>%
         select(Método = meio, Sexo = sexo_label, N = n, Prop_no_sexo = prop)),
  list("Faixa etária x Sexo",
       sui %>% count(faixa_sui, sexo_label, name = "n") %>%
         arrange(faixa_sui, sexo_label) %>%
         select(Faixa_etaria = faixa_sui, Sexo = sexo_label, N = n)),
  list("Faixa etária x Método",
       sui %>% count(faixa_sui, meio, name = "n") %>%
         arrange(faixa_sui, desc(n)) %>%
         select(Faixa_etaria = faixa_sui, Método = meio, N = n)),
  list("Cruzamento completo",
       sui %>% count(faixa_sui, meio, sexo_label, name = "n") %>%
         arrange(desc(n)) %>%
         select(Faixa_etaria = faixa_sui, Método = meio,
                Sexo = sexo_label, N = n))
)

for (aba in abas) {
  addWorksheet(wb, aba[[1]])
  writeData(wb, aba[[1]],
            paste0("15ª RS | Suicídios (X60-X84) | ", ANO), startRow = 1)
  writeDataTable(wb, aba[[1]], aba[[2]], startRow = 3,
                 tableStyle = "TableStyleLight2", headerStyle = estilo_cab)
  setColWidths(wb, aba[[1]], cols = 1:ncol(aba[[2]]), widths = "auto")
}

saveWorkbook(wb,
             file.path(dir_saida, paste0("suicidio_tabelas_15rs_", ANO, ".xlsx")),
             overwrite = TRUE)
message("Excel salvo.")

cat("\n", rep("=", 50), "\n", sep = "")
cat("SUICÍDIOS — 15ª RS |", ANO, "\n")
cat(rep("=", 50), "\n", sep = "")
cat("Total:     ", n_sui, "\n")
cat("Masculino: ", n_sui_m, sprintf("(%.1f%%)\n", n_sui_m/n_sui*100))
cat("Feminino:  ", n_sui_f, sprintf("(%.1f%%)\n", n_sui_f/n_sui*100))
cat("\nPor método:\n")
print(sui %>% count(meio, sort = TRUE))
cat(rep("=", 50), "\n\n", sep = "")
