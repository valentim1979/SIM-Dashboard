# ==============================================================================
# SIVEP-GRIPE — 15ª REGIONAL DE SAÚDE DE MARINGÁ
# Aplicação Shiny com filtros interativos
# Autor: Valentim Sala Junior | 2025
# ==============================================================================
# Para rodar localmente: clique em "Run App" no RStudio
# Para publicar online:  rsconnect::deployApp() após criar conta em shinyapps.io
# ==============================================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(foreign)
library(scales)

# ==============================================================================
# 0. CONFIGURAÇÃO — EDITE ESTES CAMINHOS
# ==============================================================================
# Instale os pacotes necessários (executar uma vez):
#install.packages(c("shiny","shinydashboard","plotly","dplyr","tidyr",
#                    "readr","foreign","scales","DT","sf","geobr","leaflet"))

DIRETORIO_DBF <- "C:/SIVEPGRIPE/BaseDBF"

ARQUIVO_IBGE  <- "C:/Users/valentim.junior/OneDrive/Área de Trabalho/sivep_15rs/ibge_cnv_pop.csv"
# ==============================================================================
# 1. CARREGAMENTO DO IBGE (executado uma vez ao iniciar o app)
# ==============================================================================

carregar_ibge <- function(caminho) {
  df <- readr::read_delim(
    caminho,
    delim          = ";",
    skip           = 6,
    locale         = readr::locale(encoding = "latin1"),
    col_names      = c("municipio_raw", "populacao"),
    col_types      = readr::cols(municipio_raw = "c", populacao = "d"),
    show_col_types = FALSE
  ) %>%
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
    filter(!is.na(codigo_ibge)) %>%
    select(codigo_ibge, nome_municipio, populacao)
  return(df)
}

ibge_15rs            <- carregar_ibge(ARQUIVO_IBGE)
POPULACAO_TOTAL_RS   <- sum(ibge_15rs$populacao)
LISTA_MUNICIPIOS     <- c("Toda a 15ª Regional", sort(ibge_15rs$nome_municipio))


# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================

rodape_texto <- function() {
  paste0("Fonte: SIVEP-Gripe | Dados extraídos em: ",
         format(Sys.Date(), "%d/%m/%Y"))
}

carregar_sivep <- function(diretorio, ano) {
  nomes <- c(
    paste0("SRAGHOSP", ano, ".dbf"),
    paste0("SRAGHOSP", ano, ".DBF"),
    paste0("sraghosp", ano, ".dbf")
  )
  caminho <- NULL
  for (n in nomes) {
    candidato <- file.path(diretorio, n)
    if (file.exists(candidato)) { caminho <- candidato; break }
  }
  if (is.null(caminho)) return(list(dados = NULL, erro = paste0(
    "Arquivo não encontrado em: ", diretorio,
    "\nEsperado: SRAGHOSP", ano, ".dbf"
  )))

  tryCatch({
    df <- foreign::read.dbf(caminho, as.is = TRUE)
    list(dados = df, erro = NULL)
  }, error = function(e) {
    list(dados = NULL, erro = as.character(e))
  })
}

aplicar_filtro_municipio <- function(base, municipio) {
  if (municipio == "Toda a 15ª Regional") return(base)
  mun_upper  <- toupper(trimws(municipio))
  codigo_mun <- ibge_15rs %>%
    filter(toupper(nome_municipio) == mun_upper) %>%
    pull(codigo_ibge)

  if (length(codigo_mun) > 0 && "CO_MUN_RES" %in% names(base)) {
    base %>% filter(as.integer(CO_MUN_RES) == codigo_mun[1])
  } else if ("ID_MUNICIP" %in% names(base)) {
    base %>% filter(grepl(mun_upper, toupper(ID_MUNICIP), fixed = TRUE))
  } else {
    base
  }
}

populacao_do_escopo <- function(municipio) {
  if (municipio == "Toda a 15ª Regional") return(POPULACAO_TOTAL_RS)
  ibge_15rs %>%
    filter(toupper(nome_municipio) == toupper(trimws(municipio))) %>%
    pull(populacao) %>%
    { if (length(.) == 0) POPULACAO_TOTAL_RS else .[1] }
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

ORDEM_FAIXAS <- c(
  "0-6 meses","6-11 meses","1-4 anos","5-9 anos","10-14 anos",
  "15-19 anos","20-29 anos","30-39 anos","40-49 anos","50-59 anos",
  "60 anos e mais","Em branco/Ignorado","Erro/Outro"
)

# ==============================================================================
# 3. INTERFACE (UI)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "SRAG — 15ª RS Maringá", titleWidth = 280),

  # --- BARRA LATERAL (filtros) ---
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("📊 Painel Principal",  tabName = "painel",     icon = icon("chart-bar")),
      menuItem("🦠 Circulação Viral",  tabName = "viral",      icon = icon("virus")),
      menuItem("👥 Perfil dos Casos",  tabName = "perfil",     icon = icon("users")),
      menuItem("🏥 Estabelecimentos",  tabName = "estab",      icon = icon("hospital")),
      menuItem("⚙️  Configurações",    tabName = "config",     icon = icon("cog"))
    ),
    hr(),
    # Filtros globais
    selectInput("ano", "📅 Ano de análise",
                choices  = 2026:2019,
                selected = 2026),
    selectInput("municipio", "🏙️ Município",
                choices  = LISTA_MUNICIPIOS,
                selected = "Toda a 15ª Regional"),
    uiOutput("sel_estabelecimento"),
    hr(),
    uiOutput("info_escopo"),
    hr(),
    actionButton("carregar", "🔄 Carregar / Atualizar",
                 class = "btn-primary btn-block")
  ),

  # --- CONTEÚDO PRINCIPAL ---
  dashboardBody(
    tabItems(

      # ── ABA: PAINEL PRINCIPAL ──────────────────────────────────────────────
      tabItem(tabName = "painel",
        uiOutput("metricas_topo"),
        fluidRow(
          box(title = "Notificações por Semana Epidemiológica",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_semana", height = "350px"))
        ),
        fluidRow(
          box(title = "Classificação Final",
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_classif", height = "320px")),
          box(title = "Evolução dos Casos (Desfecho)",
              width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("graf_evolucao", height = "320px"))
        ),
        fluidRow(
          box(title = "Notificações por Sexo",
              width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("graf_sexo", height = "280px")),
          box(title = "Raça / Cor",
              width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("graf_raca", height = "280px"))
        )
      ),

      # ── ABA: CIRCULAÇÃO VIRAL ─────────────────────────────────────────────
      tabItem(tabName = "viral",
        fluidRow(
          box(title = "Vírus Identificados por RT-PCR (Total)",
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_virus_total", height = "380px")),
          box(title = "Tipos e Linhagens de Influenza",
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_influenza", height = "380px"))
        ),
        fluidRow(
          box(title = "Tendência Semanal dos Vírus Respiratórios",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_virus_semana", height = "380px"))
        )
      ),

      # ── ABA: PERFIL DOS CASOS ─────────────────────────────────────────────
      tabItem(tabName = "perfil",
        fluidRow(
          box(title = "Pirâmide Etária — Notificados",
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("piramide_notif", height = "420px")),
          box(title = "Pirâmide Etária — Óbitos",
              width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("piramide_obitos", height = "420px"))
        ),
        fluidRow(
          box(title = "Faixa Etária: Notificados vs Confirmados",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_faixa", height = "380px"))
        ),
        fluidRow(
          box(title = "Escolaridade (> 18 anos)",
              width = 12, status = "warning", solidHeader = TRUE,
              plotlyOutput("graf_escol", height = "320px"))
        )
      ),

      # ── ABA: ESTABELECIMENTOS ─────────────────────────────────────────────
      tabItem(tabName = "estab",
        fluidRow(
          box(title = "Top Estabelecimentos por Notificações",
              width = 8, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_estab_total", height = "500px")),
          box(title = "Resumo do Estabelecimento Selecionado",
              width = 4, status = "info", solidHeader = TRUE,
              uiOutput("resumo_estab"))
        ),
        fluidRow(
          box(title = "Notificações por Semana Epidemiológica — Estabelecimento Selecionado",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_estab_semana", height = "320px"))
        ),
        fluidRow(
          box(title = "Classificação Final — Estabelecimento Selecionado",
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("graf_estab_classif", height = "300px")),
          box(title = "Evolução (Desfecho) — Estabelecimento Selecionado",
              width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("graf_estab_evolucao", height = "300px"))
        ),
        fluidRow(
          box(title = "Tabela Completa de Estabelecimentos",
              width = 12, status = "success", solidHeader = TRUE,
              DT::dataTableOutput("tabela_estab"))
        )
      ),

      # ── ABA: CONFIGURAÇÕES ────────────────────────────────────────────────
      tabItem(tabName = "config",
        box(title = "Configurações de Caminho", width = 6,
            status = "warning", solidHeader = TRUE,
            textInput("dir_dbf", "Diretório das bases DBF",
                      value = DIRETORIO_DBF),
            p("⚠️ Após alterar o diretório, clique em 'Carregar / Atualizar'
               na barra lateral."),
            hr(),
            verbatimTextOutput("diagnostico_dir"))
      )

    ) # fim tabItems
  ) # fim dashboardBody
) # fim dashboardPage

# ==============================================================================
# 4. SERVIDOR (lógica dos gráficos)
# ==============================================================================

server <- function(input, output, session) {

  # --- Base reativa: carrega ao clicar ou ao mudar ano/município ---
  base_sivep <- eventReactive(
    list(input$carregar, input$ano, input$municipio), {
      dir_atual <- isolate(input$dir_dbf)
      if (is.null(dir_atual) || !nzchar(dir_atual)) dir_atual <- DIRETORIO_DBF

      resultado <- carregar_sivep(dir_atual, input$ano)

      if (!is.null(resultado$erro)) {
        showNotification(resultado$erro, type = "error", duration = 10)
        return(NULL)
      }

      base <- resultado$dados %>%
        filter(
          grepl("15RS",    ID_REGIONA, ignore.case = TRUE),
          grepl("MARINGA", ID_REGIONA, ignore.case = TRUE)
        ) %>%
        aplicar_filtro_municipio(input$municipio)

      if (nrow(base) == 0) {
        showNotification("Nenhum registro encontrado para os filtros selecionados.",
                         type = "warning")
        return(NULL)
      }
      base
    },
    ignoreNULL = FALSE
  )

  # --- Seletor dinâmico de estabelecimento (populado após carregar a base) ---
  output$sel_estabelecimento <- renderUI({
    req(base_sivep())
    col_estab <- intersect(c("ID_UNIDADE","NO_UNIDADE","NM_UNIDADE"), names(base_sivep()))
    if (length(col_estab) == 0) return(NULL)
    col <- col_estab[1]
    opcoes <- c("Todos os Estabelecimentos",
                sort(unique(trimws(as.character(base_sivep()[[col]])))))
    opcoes <- opcoes[nzchar(opcoes) & opcoes != "NA"]
    selectInput("estabelecimento", "🏥 Estabelecimento",
                choices  = opcoes,
                selected = "Todos os Estabelecimentos")
  })

  # --- Base filtrada por estabelecimento ---
  base_estab <- reactive({
    req(base_sivep())
    base <- base_sivep()
    col_estab <- intersect(c("ID_UNIDADE","NO_UNIDADE","NM_UNIDADE"), names(base))
    estab_sel <- input$estabelecimento
    if (is.null(estab_sel) || estab_sel == "Todos os Estabelecimentos" ||
        length(col_estab) == 0) {
      return(base)
    }
    col <- col_estab[1]
    base %>% filter(trimws(as.character(.data[[col]])) == estab_sel)
  })

  # --- Informações do escopo na sidebar ---
  output$info_escopo <- renderUI({
    req(base_sivep())
    pop <- populacao_do_escopo(input$municipio)
    n   <- nrow(base_sivep())
    tagList(
      tags$small(
        tags$b("Registros carregados:"), br(),
        format(n, big.mark = "."), " notificações", br(),
        tags$b("Pop. IBGE 2025:"), br(),
        format(pop, big.mark = "."), " hab."
      )
    )
  })

  # --- Métricas do topo ---
  output$metricas_topo <- renderUI({
    req(base_sivep())
    base <- base_sivep()
    pop  <- populacao_do_escopo(input$municipio)
    n    <- nrow(base)
    ob   <- if ("EVOLUCAO" %in% names(base)) sum(base$EVOLUCAO == 2, na.rm = TRUE) else 0
    pcr  <- if ("PCR_RESUL" %in% names(base)) sum(base$PCR_RESUL == 1, na.rm = TRUE) else 0
    tx   <- round(n / pop * 100000, 1)
    let  <- if (n > 0) round(ob / n * 100, 2) else 0

    fluidRow(
      valueBox(format(n,  big.mark = "."), "Total Notificações", icon = icon("file-medical"),  color = "blue",   width = 2),
      valueBox(tx,                         "Taxa /100k hab.",    icon = icon("chart-line"),    color = "aqua",   width = 2),
      valueBox(format(pcr,big.mark = "."), "Confirmados PCR",    icon = icon("vial"),          color = "green",  width = 2),
      valueBox(format(ob, big.mark = "."), "Óbitos",             icon = icon("heart-broken"),  color = "red",    width = 2),
      valueBox(paste0(let, "%"),           "Letalidade",         icon = icon("percent"),       color = "orange", width = 2)
    )
  })

  # --- Diagnóstico do diretório ---
  output$diagnostico_dir <- renderText({
    dir <- input$dir_dbf
    if (!dir.exists(dir)) return(paste("❌ Diretório não encontrado:", dir))
    arqs <- list.files(dir, pattern = "(?i)\\.dbf$")
    if (length(arqs) == 0) return(paste("⚠️ Nenhum .dbf encontrado em:", dir))
    paste0("✅ ", length(arqs), " arquivo(s) .dbf encontrado(s):\n",
           paste(arqs, collapse = "\n"))
  })

  # ── GRÁFICO: Semana Epidemiológica ────────────────────────────────────────
  output$graf_semana <- renderPlotly({
    req(base_sivep(), "SEM_NOT" %in% names(base_sivep()))
    d <- base_sivep() %>%
      group_by(SEM_NOT) %>% summarise(total = n(), .groups = "drop")
    plot_ly(d, x = ~SEM_NOT, y = ~total, type = "bar",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(
        title  = paste0("SRAG por Semana Epidemiológica — ", input$municipio,
                        " (N=", format(nrow(base_sivep()), big.mark="."), ")"),
        xaxis  = list(title = "Semana Epidemiológica"),
        yaxis  = list(title = "Notificações"),
        showlegend = FALSE
      )
  })

  # ── GRÁFICO: Classificação Final ─────────────────────────────────────────
  output$graf_classif <- renderPlotly({
    req(base_sivep(), "CLASSI_FIN" %in% names(base_sivep()))
    mapa <- c("1"="SRAG por Influenza","2"="SRAG por Outro Vírus",
              "3"="SRAG por Outro Agente","4"="SRAG Não Especificado",
              "5"="SRAG por Covid-19")
    d <- base_sivep() %>%
      mutate(CLASS_FIN = ifelse(is.na(CLASSI_FIN), "Em Investigação",
                                mapa[as.character(CLASSI_FIN)])) %>%
      mutate(CLASS_FIN = replace(CLASS_FIN, is.na(CLASS_FIN), "Em Investigação")) %>%
      group_by(CLASS_FIN) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~CLASS_FIN, type = "bar", orientation = "h",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Total"),
             yaxis = list(title = ""),
             margin = list(l = 200))
  })

  # ── GRÁFICO: Evolução ─────────────────────────────────────────────────────
  output$graf_evolucao <- renderPlotly({
    req(base_sivep(), "EVOLUCAO" %in% names(base_sivep()))
    mapa <- c("1"="Cura","2"="Óbito por SRAG","3"="Óbito Outras Causas","9"="Ignorado")
    d <- base_sivep() %>%
      mutate(evolucao_label = ifelse(
        is.na(EVOLUCAO) | !as.character(EVOLUCAO) %in% names(mapa),
        "Não Registrado", mapa[as.character(EVOLUCAO)])) %>%
      group_by(evolucao_label) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~evolucao_label, type = "bar", orientation = "h",
            marker = list(color = "#A30000"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Casos"),
             yaxis = list(title = ""),
             margin = list(l = 180))
  })

  # ── GRÁFICO: Sexo ─────────────────────────────────────────────────────────
  output$graf_sexo <- renderPlotly({
    req(base_sivep(), "CS_SEXO" %in% names(base_sivep()))
    d <- base_sivep() %>%
      filter(CS_SEXO %in% c("F","M")) %>%
      mutate(sexo = ifelse(CS_SEXO == "F", "Feminino", "Masculino")) %>%
      group_by(sexo) %>% summarise(total = n(), .groups = "drop") %>%
      mutate(pct = round(total / sum(total) * 100, 1),
             rotulo = paste0(format(total, big.mark="."), " (", pct, "%)"))
    plot_ly(d, x = ~total, y = ~sexo, type = "bar", orientation = "h",
            marker = list(color = c("#E91E8C","#0057A3")),
            text = ~rotulo, textposition = "outside") %>%
      layout(xaxis = list(title = "Notificações"),
             yaxis = list(title = ""), showlegend = FALSE)
  })

  # ── GRÁFICO: Raça/Cor ─────────────────────────────────────────────────────
  output$graf_raca <- renderPlotly({
    req(base_sivep(), "CS_RACA" %in% names(base_sivep()))
    mapa <- c("1"="Branca","2"="Preta","3"="Amarela","4"="Parda","5"="Indígena")
    d <- base_sivep() %>%
      mutate(raca = ifelse(
        is.na(CS_RACA) | !as.character(CS_RACA) %in% names(mapa),
        "Ignorado/Outros", mapa[as.character(CS_RACA)])) %>%
      group_by(raca) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~raca, type = "bar", orientation = "h",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Casos"),
             yaxis = list(title = ""), margin = list(l = 130))
  })

  # ── GRÁFICO: Vírus Total ──────────────────────────────────────────────────
  output$graf_virus_total <- renderPlotly({
    req(base_sivep())
    mapa_v <- c(POS_PCRFLU="Influenza", PCR_SARS2="Covid-19", PCR_VSR="VSR",
                PCR_RINO="Rinovírus", PCR_ADENO="Adenovírus", PCR_METAP="Metapneumovírus",
                PCR_PARA1="Parainfluenza 1", PCR_PARA2="Parainfluenza 2",
                PCR_PARA3="Parainfluenza 3", PCR_BOCA="Bocavírus")
    cols_ok <- intersect(names(mapa_v), names(base_sivep()))
    req(length(cols_ok) > 0)
    d <- sapply(cols_ok, function(c) sum(base_sivep()[[c]] == 1, na.rm = TRUE))
    df <- data.frame(virus = mapa_v[names(d)], total = d) %>%
      filter(total > 0) %>% arrange(total)
    plot_ly(df, x = ~total, y = ~virus, type = "bar", orientation = "h",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Casos Positivos"),
             yaxis = list(title = ""), margin = list(l = 150))
  })

  # ── GRÁFICO: Influenza ────────────────────────────────────────────────────
  output$graf_influenza <- renderPlotly({
    req(base_sivep(), "POS_PCRFLU" %in% names(base_sivep()))
    d <- base_sivep() %>%
      filter(POS_PCRFLU == 1) %>%
      mutate(classificacao = case_when(
        TP_FLU_PCR == 1 & PCR_FLUASU == 1 ~ "A(H1N1)pdm09",
        TP_FLU_PCR == 1 & PCR_FLUASU == 2 ~ "A(H3N2)",
        TP_FLU_PCR == 1 & PCR_FLUASU == 3 ~ "A não subtipado",
        TP_FLU_PCR == 1 & PCR_FLUASU == 4 ~ "A não subtipável",
        TP_FLU_PCR == 2 & PCR_FLUBLI == 1 ~ "B – Victoria",
        TP_FLU_PCR == 2 & PCR_FLUBLI == 2 ~ "B – Yamagata",
        TP_FLU_PCR == 2 & PCR_FLUBLI == 3 ~ "B – Não realizado",
        TP_FLU_PCR == 2 & PCR_FLUBLI == 4 ~ "B – Inconclusivo",
        TRUE ~ "Ignorado"
      )) %>%
      group_by(classificacao) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~classificacao, type = "bar", orientation = "h",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Casos"),
             yaxis = list(title = ""), margin = list(l = 160))
  })

  # ── GRÁFICO: Tendência viral semanal ─────────────────────────────────────
  output$graf_virus_semana <- renderPlotly({
    req(base_sivep(), "SEM_NOT" %in% names(base_sivep()))
    mapa_v <- c(POS_PCRFLU="Influenza", PCR_SARS2="Covid-19", PCR_VSR="VSR",
                PCR_RINO="Rinovírus", PCR_ADENO="Adenovírus", PCR_METAP="Metapneumovírus")
    cols_ok <- intersect(names(mapa_v), names(base_sivep()))
    req(length(cols_ok) > 0)
    d <- base_sivep() %>%
      select(SEM_NOT, all_of(cols_ok)) %>%
      pivot_longer(cols = all_of(cols_ok), names_to = "virus", values_to = "resultado") %>%
      filter(resultado == 1) %>%
      mutate(virus = mapa_v[virus]) %>%
      group_by(SEM_NOT, virus) %>% summarise(total = n(), .groups = "drop")
    plot_ly(d, x = ~SEM_NOT, y = ~total, color = ~virus,
            type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = "Semana Epidemiológica"),
             yaxis = list(title = "Casos Positivos"),
             legend = list(orientation = "h", y = -0.2))
  })

  # ── GRÁFICO: Pirâmide Notificados ─────────────────────────────────────────
  output$piramide_notif <- renderPlotly({
    req(base_sivep(), "COD_IDADE" %in% names(base_sivep()),
        "CS_SEXO" %in% names(base_sivep()))
    d <- base_sivep() %>%
      criar_faixa_etaria() %>%
      mutate(sexo = case_when(
        CS_SEXO %in% c("M","m","1",1) ~ "Masculino",
        CS_SEXO %in% c("F","f","2",2) ~ "Feminino",
        TRUE ~ "Ignorado")) %>%
      filter(sexo != "Ignorado") %>%
      group_by(faixa_etaria, sexo) %>% summarise(n = n(), .groups = "drop") %>%
      mutate(faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS),
             value = ifelse(sexo == "Masculino", -n, n))

    plot_ly(d, x = ~value, y = ~faixa_etaria, color = ~sexo, type = "bar",
            orientation = "h",
            colors = c("Feminino" = "#E91E8C", "Masculino" = "#0057A3")) %>%
      layout(barmode  = "relative",
             xaxis    = list(title = "Casos", tickvals = seq(-200,200,50),
                             ticktext = as.character(abs(seq(-200,200,50)))),
             yaxis    = list(title = "", categoryorder = "array",
                             categoryarray = ORDEM_FAIXAS),
             legend   = list(orientation = "h", y = -0.15))
  })

  # ── GRÁFICO: Pirâmide Óbitos ──────────────────────────────────────────────
  output$piramide_obitos <- renderPlotly({
    req(base_sivep(), "EVOLUCAO" %in% names(base_sivep()))
    d <- base_sivep() %>%
      filter(EVOLUCAO == 2) %>%
      criar_faixa_etaria() %>%
      mutate(sexo = case_when(
        CS_SEXO %in% c("M","m","1",1) ~ "Masculino",
        CS_SEXO %in% c("F","f","2",2) ~ "Feminino",
        TRUE ~ "Ignorado")) %>%
      filter(sexo != "Ignorado") %>%
      group_by(faixa_etaria, sexo) %>% summarise(n = n(), .groups = "drop") %>%
      mutate(faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS),
             value = ifelse(sexo == "Masculino", -n, n))

    if (nrow(d) == 0) {
      return(plot_ly() %>%
               layout(title = "Sem óbitos no período selecionado"))
    }
    plot_ly(d, x = ~value, y = ~faixa_etaria, color = ~sexo, type = "bar",
            orientation = "h",
            colors = c("Feminino" = "#E91E8C", "Masculino" = "#0057A3")) %>%
      layout(barmode = "relative",
             xaxis   = list(title = "Óbitos", tickvals = seq(-50,50,10),
                            ticktext = as.character(abs(seq(-50,50,10)))),
             yaxis   = list(title = "", categoryorder = "array",
                            categoryarray = ORDEM_FAIXAS),
             legend  = list(orientation = "h", y = -0.15))
  })

  # ── GRÁFICO: Faixa etária notif. vs confirmados ───────────────────────────
  output$graf_faixa <- renderPlotly({
    req(base_sivep(), "COD_IDADE" %in% names(base_sivep()))
    base_f  <- base_sivep() %>% criar_faixa_etaria() %>%
      mutate(faixa_etaria = factor(faixa_etaria, levels = ORDEM_FAIXAS))
    notif   <- base_f %>% count(faixa_etaria, name="total") %>% mutate(tipo="Notificados")
    conf    <- base_f %>% filter(CLASSI_FIN %in% c(1,2,3,5)) %>%
      count(faixa_etaria, name="total") %>% mutate(tipo="Confirmados")
    d       <- bind_rows(notif, conf)
    plot_ly(d, x = ~total, y = ~faixa_etaria, color = ~tipo, type = "bar",
            orientation = "h", barmode = "group",
            colors = c("Notificados"="#0057A3","Confirmados"="#27AE60"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis  = list(title = "Quantidade"),
             yaxis  = list(title = ""),
             legend = list(orientation = "h", y = -0.15),
             margin = list(l = 130))
  })

  # ── GRÁFICO: Escolaridade ─────────────────────────────────────────────────
  output$graf_escol <- renderPlotly({
    req(base_sivep(), "CS_ESCOL_N" %in% names(base_sivep()))
    mapa <- c("0"="Analfabeto","1"="Fund. 1º Ciclo","2"="Fund. 2º Ciclo",
              "3"="Ensino Médio","4"="Ensino Superior","5"="Não se aplica","9"="Ignorado")
    d <- base_sivep() %>%
      filter(if ("NU_IDADE_N" %in% names(.)) NU_IDADE_N > 18 else TRUE) %>%
      mutate(escol = ifelse(
        is.na(CS_ESCOL_N) | !as.character(CS_ESCOL_N) %in% names(mapa),
        "Não Registrado", mapa[as.character(CS_ESCOL_N)])) %>%
      group_by(escol) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~escol, type = "bar", orientation = "h",
            marker = list(color = "#FF8C00"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis = list(title = "Casos"),
             yaxis = list(title = ""), margin = list(l = 180))
  })


  # ── ABA ESTABELECIMENTOS ──────────────────────────────────────────────────

  # Helper: detecta coluna de nome/ID do estabelecimento
  col_estab_nome <- reactive({
    req(base_sivep())
    intersect(c("NO_UNIDADE","NM_UNIDADE","ID_UNIDADE"), names(base_sivep()))[1]
  })

  # Gráfico: top estabelecimentos (usa base_sivep sem filtro de estab)
  output$graf_estab_total <- renderPlotly({
    req(base_sivep(), !is.null(col_estab_nome()))
    col <- col_estab_nome()

    d <- base_sivep() %>%
      mutate(estab = trimws(as.character(.data[[col]]))) %>%
      filter(!is.na(estab), nzchar(estab), estab != "NA") %>%
      group_by(estab) %>%
      summarise(total = n(), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 20) %>%       # top 20
      arrange(total)               # crescente para barras horizontais

    # Destaca o selecionado
    estab_sel <- input$estabelecimento
    d <- d %>%
      mutate(cor = ifelse(
        !is.null(estab_sel) && estab_sel != "Todos os Estabelecimentos" & estab == estab_sel,
        "#E74C3C", "#0057A3"
      ))

    plot_ly(d, x = ~total, y = ~estab, type = "bar", orientation = "h",
            marker = list(color = ~cor),
            text   = ~total, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Notificações: %{x}<extra></extra>") %>%
      layout(
        title  = paste0("Top 20 Estabelecimentos — ", input$municipio,
                        " (", input$ano, ")"),
        xaxis  = list(title = "Notificações"),
        yaxis  = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 250),
        showlegend = FALSE
      )
  })

  # Resumo do estabelecimento selecionado
  output$resumo_estab <- renderUI({
    req(base_estab())
    estab_sel <- input$estabelecimento
    if (is.null(estab_sel) || estab_sel == "Todos os Estabelecimentos") {
      return(tagList(
        tags$p("Selecione um estabelecimento na barra lateral para ver o resumo detalhado.")
      ))
    }
    base <- base_estab()
    n     <- nrow(base)
    ob    <- if ("EVOLUCAO" %in% names(base)) sum(base$EVOLUCAO == 2, na.rm=TRUE) else 0
    pcr   <- if ("PCR_RESUL" %in% names(base)) sum(base$PCR_RESUL == 1, na.rm=TRUE) else 0
    let   <- if (n > 0) round(ob/n*100, 1) else 0
    tagList(
      tags$h4(estab_sel, style="font-size:13px; word-break:break-word;"),
      tags$hr(),
      tags$p(tags$b("Total notificações: "), format(n, big.mark=".")),
      tags$p(tags$b("Confirmados PCR: "),    format(pcr, big.mark=".")),
      tags$p(tags$b("Óbitos: "),             format(ob, big.mark=".")),
      tags$p(tags$b("Letalidade: "),         paste0(let, "%")),
      tags$hr(),
      tags$small("Os gráficos abaixo refletem este estabelecimento.")
    )
  })

  # Semana epidemiológica do estabelecimento
  output$graf_estab_semana <- renderPlotly({
    req(base_estab(), "SEM_NOT" %in% names(base_estab()))
    estab_sel <- input$estabelecimento
    titulo <- if (!is.null(estab_sel) && estab_sel != "Todos os Estabelecimentos") {
      paste0("Semana Epi — ", estab_sel)
    } else {
      paste0("Semana Epi — Todos os Estabelecimentos")
    }
    d <- base_estab() %>%
      group_by(SEM_NOT) %>% summarise(total = n(), .groups = "drop")
    plot_ly(d, x = ~SEM_NOT, y = ~total, type = "bar",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(title  = titulo,
             xaxis  = list(title = "Semana Epidemiológica"),
             yaxis  = list(title = "Notificações"),
             showlegend = FALSE)
  })

  # Classificação final do estabelecimento
  output$graf_estab_classif <- renderPlotly({
    req(base_estab(), "CLASSI_FIN" %in% names(base_estab()))
    mapa <- c("1"="SRAG por Influenza","2"="SRAG por Outro Vírus",
              "3"="SRAG por Outro Agente","4"="SRAG Não Especificado",
              "5"="SRAG por Covid-19")
    d <- base_estab() %>%
      mutate(CLASS_FIN = ifelse(
        is.na(CLASSI_FIN) | !as.character(CLASSI_FIN) %in% names(mapa),
        "Em Investigação", mapa[as.character(CLASSI_FIN)])) %>%
      group_by(CLASS_FIN) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~CLASS_FIN, type = "bar", orientation = "h",
            marker = list(color = "#0057A3"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis  = list(title = "Total"),
             yaxis  = list(title = ""),
             margin = list(l = 200),
             showlegend = FALSE)
  })

  # Evolução (desfecho) do estabelecimento
  output$graf_estab_evolucao <- renderPlotly({
    req(base_estab(), "EVOLUCAO" %in% names(base_estab()))
    mapa <- c("1"="Cura","2"="Óbito por SRAG",
              "3"="Óbito Outras Causas","9"="Ignorado")
    d <- base_estab() %>%
      mutate(evolucao_label = ifelse(
        is.na(EVOLUCAO) | !as.character(EVOLUCAO) %in% names(mapa),
        "Em Investigação", mapa[as.character(EVOLUCAO)])) %>%
      group_by(evolucao_label) %>% summarise(total = n(), .groups = "drop") %>%
      arrange(total)
    plot_ly(d, x = ~total, y = ~evolucao_label, type = "bar", orientation = "h",
            marker = list(color = "#A30000"),
            text = ~total, textposition = "outside") %>%
      layout(xaxis  = list(title = "Casos"),
             yaxis  = list(title = ""),
             margin = list(l = 160),
             showlegend = FALSE)
  })

  # Tabela completa de estabelecimentos
  output$tabela_estab <- DT::renderDataTable({
    req(base_sivep(), !is.null(col_estab_nome()))
    col <- col_estab_nome()
    d <- base_sivep() %>%
      mutate(estab = trimws(as.character(.data[[col]]))) %>%
      filter(!is.na(estab), nzchar(estab), estab != "NA") %>%
      group_by(estab) %>%
      summarise(
        `Notificações`   = n(),
        `Confirmados PCR`= if("PCR_RESUL" %in% names(.)) sum(PCR_RESUL==1, na.rm=TRUE) else NA_integer_,
        `Óbitos`         = if("EVOLUCAO"  %in% names(.)) sum(EVOLUCAO==2,  na.rm=TRUE) else NA_integer_,
        .groups = "drop"
      ) %>%
      mutate(
        `Letalidade (%)` = round(`Óbitos` / `Notificações` * 100, 1)
      ) %>%
      arrange(desc(`Notificações`)) %>%
      rename(Estabelecimento = estab)

    DT::datatable(
      d,
      filter  = "top",
      options = list(
        pageLength = 15,
        language   = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")
      )
    )
  })

} # fim server

# ==============================================================================
# 5. INICIAR A APLICAÇÃO
# ==============================================================================

shinyApp(ui = ui, server = server)
