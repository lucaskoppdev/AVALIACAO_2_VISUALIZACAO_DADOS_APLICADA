library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(readr)

#carrega os dados tratados do "prepara_dados.R"
df <- read_rds("dados_llm.rds")

# cores feitas em CSS pra melhorar a qualidade / ter mais liberdad
dark_dt_css <- HTML("
  table.dataTable {
    color: #e0e0e0 !important;
    background-color: #1e1e2d !important;
    border-collapse: collapse !important;
  }
  table.dataTable thead th {
    background-color: #2b2b40 !important;
    color: #ffffff !important;
    border-bottom: 2px solid #3d3d5c !important;
    font-weight: 600;
  }
  table.dataTable tbody tr {
    background-color: #1e1e2d !important;
    color: #d1d1e0 !important;
  }
  table.dataTable tbody tr:hover {
    background-color: #2a2a3c !important;
  }
  table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd {
    background-color: #181824 !important;
  }
  table.dataTable tbody tr.selected, table.dataTable.display tbody tr.selected {
    background-color: #3b82f6 !important;
    color: #ffffff !important;
  }
  .dataTables_wrapper .aria-label, .dataTables_wrapper input, .dataTables_wrapper select {
    background-color: #2b2b40 !important;
    color: #ffffff !important;
    border: 1px solid #444466 !important;
    border-radius: 4px;
    padding: 4px 8px;
  }
  .dataTables_wrapper .dataTables_info, 
  .dataTables_wrapper .dataTables_paginate,
  .dataTables_wrapper .dataTables_length label,
  .dataTables_wrapper .dataTables_filter label {
    color: #a0a0c0 !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #ffffff !important;
  }
")

ui <- dashboardPage(
  title = "LLM Preço vs. Arena",
  dark = TRUE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "LLM Analytics",
      color = "primary"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "abas",
      menuItem("Visão Geral", tabName = "tab_geral", icon = icon("chart-line")),
      menuItem("Eficiência (Quadrantes)", tabName = "tab_eficiencia", icon = icon("crosshairs")),
      menuItem("Comparador de Modelos", tabName = "tab_comp", icon = icon("balance-scale")),
      menuItem("Tabela de Dados", tabName = "tab_dados", icon = icon("table"))
    ),
    hr(),
    selectInput(
      "filtro_provedor",
      "Provedor:",
      choices = sort(unique(na.omit(df$provedor))),
      selected = NULL,
      multiple = TRUE
    ),
    selectInput(
      "filtro_tier",
      "Faixa de Preço (Tier):",
      choices = c("Todos", sort(unique(na.omit(df$price_tier)))),
      selected = "Todos"
    ),
    selectInput(
      "filtro_cat",
      "Categoria Arena:",
      choices = c("Todas", sort(unique(na.omit(df$categoria_arena)))),
      selected = "Todas"
    ),
    br(),
    div(
      style = "padding: 0 12px;",
      actionButton("reset_filtros", "Resetar filtros", icon = icon("rotate-left"), width = "100%")
    )
  ),
  
  body = dashboardBody(
    tags$head(tags$style(dark_dt_css)),
    
    tabItems(
      # Aba 1 - Aba inicial/visao geral
      tabItem(
        tabName = "tab_geral",
        fluidRow(
          valueBoxOutput("kpi_modelos", width = 4),
          valueBoxOutput("kpi_top_rating", width = 4),
          valueBoxOutput("kpi_menor_input", width = 4)
        ),
        fluidRow(
          box(
            title = "Custo Input vs. Output (USD / 1M tokens)",
            width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_custo", height = "380px")
          ),
          box(
            title = "Top Modelos por Arena Rating",
            width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_rating", height = "380px")
          )
        )
      ),
      
      # ABA 2: EFICIENCIA - QUADRANTES DE RATING DO MODELO vs Custo
      tabItem(
        tabName = "tab_eficiencia",
        fluidRow(
          valueBoxOutput("kpi_melhor_eficiencia", width = 6),
          valueBoxOutput("kpi_mediana_rating", width = 6)
        ),
        fluidRow(
          box(
            title = "Quadrantes de Eficiência: Arena Rating vs. Custo (Cenário 10k Input / 1k Output)",
            width = 12, status = "purple", solidHeader = TRUE,
            plotlyOutput("plot_quadrantes", height = "460px")
          )
        )
      ),
      
      # ABA 3 - COMPARA MODELOS - 5 POR VEZ (por enquanto)
      tabItem(
        tabName = "tab_comp",
        fluidRow(
          box(
            title = "Controles de Comparação",
            width = 12, status = "warning", solidHeader = TRUE,
            selectizeInput(
              "modelos_sel",
              "Escolha até 5 modelos para comparar:",
              choices = sort(unique(df$modelo)),
              selected = head(sort(unique(df$modelo)), 4),
              multiple = TRUE,
              options = list(maxItems = 5)
            )
          )
        ),
        fluidRow(
          box(
            title = "Comparativo de Arena Rating e Custo-Benefício",
            width = 12, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_comparativo", height = "400px")
          )
        )
      ),
      
      # ABA 4 - TABELA COM OS DADOS UTILIZADOS
      tabItem(
        tabName = "tab_dados",
        fluidRow(
          box(
            title = "Explorador Interativo de Modelos",
            width = 12, status = "gray-dark", solidHeader = TRUE,
            DTOutput("tabela_dt")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "filtro_provedor", selected = character(0))
    updateSelectInput(session, "filtro_tier", selected = "Todos")
    updateSelectInput(session, "filtro_cat", selected = "Todas")
  })
  
  dados_filtrados <- reactive({
    d <- df
    if (length(input$filtro_provedor) > 0) {
      d <- d %>% filter(provedor %in% input$filtro_provedor)
    }
    if (!is.null(input$filtro_tier) && input$filtro_tier != "Todos") {
      d <- d %>% filter(price_tier == input$filtro_tier)
    }
    if (!is.null(input$filtro_cat) && input$filtro_cat != "Todas") {
      d <- d %>% filter(categoria_arena == input$filtro_cat)
    }
    d
  })
  
  # Base auxiliar com custo do cenário (10k input + 1k output) e eficiência
  dados_eficiencia <- reactive({
    dados_filtrados() %>%
      mutate(
        custo_cenario = dplyr::coalesce(
          scenario_10k_1k_usd,
          (input_usd_per_1m * 0.01) + (output_usd_per_1m * 0.001)
        ),
        eficiencia = dplyr::case_when(
          !is.na(arena_rating_per_usd_10k_1k) & is.finite(arena_rating_per_usd_10k_1k) ~ arena_rating_per_usd_10k_1k,
          !is.na(arena_rating) & !is.na(custo_cenario) & custo_cenario > 0 ~ arena_rating / custo_cenario,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(arena_rating), !is.na(custo_cenario), is.finite(custo_cenario))
  })
  
  # KPI's da visao geral
  output$kpi_modelos <- renderValueBox({
    valueBox(
      value = n_distinct(dados_filtrados()$modelo),
      subtitle = "Modelos no recorte",
      icon = icon("microchip"),
      color = "primary"
    )
  })
  
  output$kpi_top_rating <- renderValueBox({
    top_row <- dados_filtrados() %>%
      filter(!is.na(arena_rating)) %>%
      arrange(desc(arena_rating)) %>%
      slice(1)
    
    nome <- if (nrow(top_row) > 0) top_row$modelo else "Sem rating"
    valueBox(
      value = nome,
      subtitle = "Maior Arena Rating",
      icon = icon("award"),
      color = "warning"
    )
  })
  
  output$kpi_menor_input <- renderValueBox({
    barato <- dados_filtrados() %>%
      filter(!is.na(input_usd_per_1m)) %>%
      arrange(input_usd_per_1m) %>%
      slice(1)
    
    val <- if (nrow(barato) > 0) sprintf("$%.2f", barato$input_usd_per_1m) else "N/A"
    valueBox(
      value = val,
      subtitle = "Menor Input (USD/1M)",
      icon = icon("tag"),
      color = "success"
    )
  })
  
  # KPIs de Eficiencia
  output$kpi_melhor_eficiencia <- renderValueBox({
    d <- dados_eficiencia() %>%
      filter(!is.na(eficiencia), is.finite(eficiencia)) %>%
      arrange(desc(eficiencia)) %>%
      slice(1)
    
    nome <- if (nrow(d) > 0) d$modelo else "Sem dados"
    valueBox(
      value = nome,
      subtitle = "Maior Rating por Dólar no Cenário",
      icon = icon("bolt"),
      color = "purple"
    )
  })
  
  output$kpi_mediana_rating <- renderValueBox({
    d <- dados_eficiencia()
    med <- if (nrow(d) > 0) sprintf("%.1f", median(d$arena_rating, na.rm = TRUE)) else "—"
    valueBox(
      value = med,
      subtitle = "Mediana de Arena Rating (Linha de Corte)",
      icon = icon("sliders"),
      color = "info"
    )
  })
  
  # Gráfico 1: Dispersão Custo Input vs Output
  output$plot_custo <- renderPlotly({
    d <- dados_filtrados() %>%
      filter(!is.na(input_usd_per_1m), !is.na(output_usd_per_1m))
    
    validate(need(nrow(d) > 0, "Sem dados de preço para o filtro atual."))
    
    plot_ly(
      d,
      x = ~input_usd_per_1m,
      y = ~output_usd_per_1m,
      color = ~provedor,
      text = ~paste0(
        "<b>", modelo, "</b><br>",
        "Provedor: ", provedor, "<br>",
        "Input: $", input_usd_per_1m, " | Output: $", output_usd_per_1m, "<br>",
        "Arena Rating: ", ifelse(is.na(arena_rating), "—", round(arena_rating, 1))
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, opacity = 0.85)
    ) %>%
      layout(
        xaxis = list(title = "Input (USD / 1M tokens)"),
        yaxis = list(title = "Output (USD / 1M tokens)")
      )
  })
  
  # Gráfico 2: Ranking Arena Rating
  output$plot_rating <- renderPlotly({
    d <- dados_filtrados() %>%
      filter(!is.na(arena_rating)) %>%
      arrange(desc(arena_rating)) %>%
      distinct(modelo, .keep_all = TRUE) %>%
      head(12)
    
    validate(need(nrow(d) > 0, "Sem pontuações de Arena para o filtro atual."))
    
    plot_ly(
      d,
      x = ~arena_rating,
      y = ~reorder(modelo, arena_rating),
      type = "bar",
      orientation = "h",
      text = ~round(arena_rating, 1),
      textposition = "auto"
    ) %>%
      layout(
        xaxis = list(title = "Arena Rating"),
        yaxis = list(title = "")
      )
  })
  
  # Novo Gráfico: Quadrantes de Eficiência (Rating vs. Custo do Cenario da base auxiliar)
  output$plot_quadrantes <- renderPlotly({
    d <- dados_eficiencia()
    validate(need(nrow(d) >= 2, "Poucos modelos com custo e Arena Rating válidos para montar os quadrantes."))
    
    med_x <- median(d$custo_cenario, na.rm = TRUE)
    med_y <- median(d$arena_rating, na.rm = TRUE)
    
    d <- d %>%
      mutate(
        quadrante = case_when(
          arena_rating >= med_y & custo_cenario <= med_x ~ "Custo-Benefício (Rating Alto / Custo Baixo)",
          arena_rating >= med_y & custo_cenario >  med_x ~ "Premium (Rating Alto / Custo Alto)",
          arena_rating <  med_y & custo_cenario <= med_x ~ "Econômico (Rating Baixo / Custo Baixo)",
          TRUE                                           ~ "Menor Eficiência (Rating Baixo / Custo Alto)"
        )
      )
    
    min_x <- min(d$custo_cenario, na.rm = TRUE)
    max_x <- max(d$custo_cenario, na.rm = TRUE)
    min_y <- min(d$arena_rating, na.rm = TRUE)
    max_y <- max(d$arena_rating, na.rm = TRUE)
    
    plot_ly(
      d,
      x = ~custo_cenario,
      y = ~arena_rating,
      color = ~quadrante,
      text = ~paste0(
        "<b>", modelo, "</b> (", provedor, ")<br>",
        "Quadrante: ", quadrante, "<br>",
        "Arena Rating: ", round(arena_rating, 1), "<br>",
        "Custo Cenário (10k/1k): $", sprintf("%.4f", custo_cenario)
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(size = 11, opacity = 0.88)
    ) %>%
      layout(
        xaxis = list(title = "Custo Estimado no Cenário 10k Input / 1k Output (USD)"),
        yaxis = list(title = "Arena Rating"),
        shapes = list(
          list(
            type = "line", x0 = med_x, x1 = med_x, y0 = min_y, y1 = max_y,
            line = list(dash = "dash", width = 1.5, color = "#9ca3af")
          ),
          list(
            type = "line", x0 = min_x, x1 = max_x, y0 = med_y, y1 = med_y,
            line = list(dash = "dash", width = 1.5, color = "#9ca3af")
          )
        ),
        annotations = list(
          list(x = min_x, y = max_y, text = "<b>Custo-Benefício</b>", showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 11)),
          list(x = max_x, y = max_y, text = "<b>Premium</b>", showarrow = FALSE, xanchor = "right", yanchor = "top", font = list(size = 11)),
          list(x = min_x, y = min_y, text = "<b>Econômico</b>", showarrow = FALSE, xanchor = "left", yanchor = "bottom", font = list(size = 11)),
          list(x = max_x, y = min_y, text = "<b>Menor Eficiência</b>", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 11))
        )
      )
  })
  
  # Gráfico 3: COmparacao dos modelos escolhidos 
  output$plot_comparativo <- renderPlotly({
    req(input$modelos_sel)
    
    d <- df %>%
      filter(modelo %in% input$modelos_sel) %>%
      distinct(modelo, .keep_all = TRUE) %>%
      select(
        modelo,
        `Arena Rating` = arena_rating,
        `Custo Cenário 10k/1k ($)` = scenario_10k_1k_usd,
        `Rating / $ (10k/1k)` = arena_rating_per_usd_10k_1k
      ) %>%
      pivot_longer(cols = -modelo, names_to = "Metrica", values_to = "Valor") %>%
      filter(!is.na(Valor))
    
    validate(need(nrow(d) > 0, "Sem métricas preenchidas para os modelos escolhidos."))
    
    plot_ly(
      d,
      x = ~Metrica,
      y = ~Valor,
      color = ~modelo,
      type = "bar",
      barmode = "group"
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Valor")
      )
  })
  
  # Tabela DT dos modelos
  output$tabela_dt <- renderDT({
    dados_formatados <- dados_filtrados() %>%
      select(
        Modelo = modelo,
        Provedor = provedor,
        `Família` = model_family,
        Modalidade = modality,
        Tier = price_tier,
        Data = data_snap,
        `Input ($/1M)` = input_usd_per_1m,
        `Output ($/1M)` = output_usd_per_1m,
        `Arena Rating` = arena_rating,
        `Arena Rank` = arena_rank
      )
    
    datatable(
      dados_formatados,
      filter = "top",
      rownames = FALSE,
      style = "bootstrap4",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "lfrtip",
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ até _END_ de _TOTAL_ registros",
          paginate = list(previous = "Anterior", "next" = "Próximo")
        )
      )
    ) %>%
      formatCurrency(c("Input ($/1M)", "Output ($/1M)"), currency = "$", digits = 2) %>%
      formatRound("Arena Rating", digits = 1)
  })
}

shinyApp(ui, server)
