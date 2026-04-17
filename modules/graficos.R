# ===================================================================================
# OBJETIVO PRINCIPAL: MÓDULO DE GRÁFICOS E KPIs
# ===================================================================================

### --- UI do Módulo de Gráficos --- ###
graficosUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$button(
      id = ns("btn_close_graficos"),
      type = "button",
      class = "btn-close-menu action-button",
      icon("times")
    ),
    
    div(class = "pill-toggle-container",
        actionButton(ns("tab_btn_destaques"), HTML("<i class='fa fa-star'></i> Destaques"), class = "pill-btn active"),
        actionButton(ns("tab_btn_graficos"), HTML("<i class='fa fa-chart-bar'></i> Gráficos"), class = "pill-btn")
    ),
    
    div(id = ns("tab_content_destaques"),
        uiOutput(ns("kpi_area")),
        uiOutput(ns("kpi_distrito")),
        uiOutput(ns("kpi_concedente")),
        uiOutput(ns("metricas_distritos"))
    ),
    
    shinyjs::hidden(
      div(id = ns("tab_content_graficos"),
          uiOutput(ns("view_selector_container")),
          withSpinner(
            uiOutput(ns("dynamic_content_area")),
            type = 6, 
            color = "#D35400"
          )
      )
    )
  )
}

### --- Server do Módulo de Gráficos --- ###
graficosServer <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$tab_btn_destaques, {
      shinyjs::show("tab_content_destaques")
      shinyjs::hide("tab_content_graficos")
      shinyjs::addClass("tab_btn_destaques", "active")
      shinyjs::removeClass("tab_btn_graficos", "active")
    })
    
    observeEvent(input$tab_btn_graficos, {
      shinyjs::show("tab_content_graficos")
      shinyjs::hide("tab_content_destaques")
      shinyjs::addClass("tab_btn_graficos", "active")
      shinyjs::removeClass("tab_btn_destaques", "active")
    })
    
    tooltip_kpi_text <- "Este valor é recalculado dinamicamente com base nos filtros selecionados."
    
    data_par_mod <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> filter(!is.na(lote), !is.na(ppp_modali)) |> distinct(lote, .keep_all = TRUE) |> count(ppp_modali, name = "Quantidade") |> arrange(desc(Quantidade)) })
    data_par_con <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> filter(!is.na(lote), !is.na(ppp_conced)) |> distinct(lote, .keep_all = TRUE) |> count(ppp_conced, name = "Quantidade") |> mutate(ppp_conced_display = str_replace(ppp_conced, "^Secretaria Municipal", "Sec Mun")) |> arrange(desc(Quantidade)) |> top_n(10, Quantidade) })
    data_par_zona <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> filter(!is.na(zona), !is.na(lote)) |> group_by(zona) |> summarise(Quantidade = n_distinct(lote)) |> arrange(desc(Quantidade)) })
    
    data_eq_mod <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> count(ppp_modali, name = "Quantidade") |> arrange(desc(Quantidade)) })
    data_eq_con <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_conced)) |> count(ppp_conced, name = "Quantidade") |> top_n(10, Quantidade) |> mutate(ppp_conced_display = str_replace(ppp_conced, "^Secretaria Municipal", "Sec Mun")) |> arrange(desc(Quantidade)) })
    data_eq_zona <- reactive({ req(dados_filtrados()); dados_filtrados() |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(zona)) |> count(zona, name = "Quantidade") |> arrange(desc(Quantidade)) })
    
    output$kpi_area <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      maior_area <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_area)) |> arrange(desc(ppp_area)) |> slice(1)
      valor <- if (nrow(maior_area) == 0) "N/A" else maior_area$ppp_nome
      
      div(class = "kpi-card", 
          tags$div(class = "kpi-header", h5("Maior área de concessão (m²)"), bslib::tooltip(tags$span(icon("question-circle")), tooltip_kpi_text, placement = "bottom")), 
          p(valor))
    })
    
    output$kpi_distrito <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      contagem_distritos <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(NM_DIST)) |> count(NM_DIST, name = "n", sort = TRUE)
      req(nrow(contagem_distritos) > 0)
      top_distritos <- contagem_distritos |> filter(n == contagem_distritos$n[1])
      valor <- paste(top_distritos$NM_DIST, collapse = ", ")
      titulo <- if(nrow(top_distritos) > 1) "Distritos em destaque" else "Distrito em destaque"
      
      div(class = "kpi-card", 
          tags$div(class = "kpi-header", h5(titulo), bslib::tooltip(tags$span(icon("question-circle")), tooltip_kpi_text, placement = "bottom")), 
          p(valor))
    })
    
    output$kpi_concedente <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      contagem_concedente <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_conced)) |> count(ppp_conced, name = "n", sort = TRUE)
      req(nrow(contagem_concedente) > 0)
      top_concedentes <- contagem_concedente |> filter(n == contagem_concedente$n[1])
      valor <- paste(top_concedentes$ppp_conced, collapse = ", ")
      
      titulo <- if(nrow(top_concedentes) > 1) "Poderes concedentes em destaque" else "Poder concedente em destaque"
      
      div(class = "kpi-card", 
          tags$div(class = "kpi-header", h5(titulo), bslib::tooltip(tags$span(icon("question-circle")), tooltip_kpi_text, placement = "bottom")), 
          p(valor))
    })
    
    output$metricas_distritos <- renderUI({
      dados <- dados_filtrados()
      req(nrow(dados) > 0)
      
      contagem_com <- length(unique(na.omit(dados$NM_DIST)))
      contagem_sem <- 96 - contagem_com
      
      # Ícone de tooltip padronizado para esta seção
      icone_tooltip <- bslib::tooltip(
        tags$span(icon("question-circle"), style = "color: #b0b0b0; font-size: 13px; cursor: help; margin-left: 4px;"), 
        tooltip_kpi_text, 
        placement = "bottom"
      )
      
      div(class = "district-metrics",
          div(class = "metric-row",
              div(class = "metric-label", "Nº de distritos", tags$br(), tags$strong("com equipamentos"), icone_tooltip),
              div(class = "metric-value", contagem_com)
          ),
          tags$hr(),
          div(class = "metric-row",
              div(class = "metric-label", "Nº de distritos", tags$br(), tags$strong("sem equipamentos"), icone_tooltip),
              div(class = "metric-value", contagem_sem)
          )
      )
    })
    
    output$view_selector_container <- renderUI({
      req(dados_filtrados())
      dados <- dados_filtrados()
      n_equipamentos <- dados |> st_drop_geometry() |> distinct(ppp_nome) |> nrow()
      
      if (n_equipamentos > 1) {
        div(style = "margin-bottom: 20px;", 
            radioButtons(
              inputId = session$ns("view_selector"),
              label = NULL,
              choices = c("Visão por parcerias" = "parceria", 
                          "Visão por equipamentos" = "equipamento"),
              selected = "parceria"
            )
        )
      } else { NULL }
    })
    
    output$dynamic_content_area <- renderUI({
      ns <- session$ns
      dados <- dados_filtrados()
      visao <- if(is.null(input$view_selector)) "parceria" else input$view_selector
      
      if (nrow(dados) == 0) return(tags$h4("Nenhum dado encontrado.", style = "color: #888; text-align: center; margin-top: 20px;"))
      
      n_equipamentos <- dados |> st_drop_geometry() |> distinct(ppp_nome) |> nrow()
      
      if (n_equipamentos == 1) {
        dados_info <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE)
        info_html <- dados_info$label_html[1]
        
        return(
          div(class = "single-info-card", 
              tags$h4("Informações do Equipamento", class = "section-title", style = "text-align: center;"), 
              tags$div(HTML(info_html), class = "info-content"), 
              tags$p(tags$em("Selecione mais itens nos filtros para ver os gráficos."), style = "text-align: center; font-size: 12px; color: #888; margin-top: 25px; border-top: 1px solid #eee; padding-top: 15px;")
          )
        )
      } else {
        make_chart_title <- function(title_text) {
          tags$div(class = "chart-title-wrapper", tags$h5(title_text, class = "chart-title"))
        }
        
        graficos_lista <- list()
        
        if (visao == "parceria") {
          if (nrow(data_par_mod()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Parcerias por modalidade"), highchartOutput(ns("plot_parceria_modalidade"), height = "220px"))
          if (nrow(data_par_con()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Parcerias por poder concedente"), highchartOutput(ns("plot_parceria_concedente"), height = "220px"))
          if (nrow(data_par_zona()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Parcerias por zona"), highchartOutput(ns("plot_lote_zona"), height = "220px"))
        } else {
          if (nrow(data_eq_mod()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Equipamentos por modalidade"), highchartOutput(ns("plot_modalidade"), height = "220px"))
          if (nrow(data_eq_con()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Equipamentos por poder concedente"), highchartOutput(ns("plot_concedente"), height = "220px"))
          if (nrow(data_eq_zona()) > 0) graficos_lista[[length(graficos_lista)+1]] <- div(class = "chart-box", make_chart_title("Equipamentos por zona"), highchartOutput(ns("plot_zona"), height = "220px"))
        }
        tagList(graficos_lista)
      }
    })
    
    # =========================================================================
    # --- RENDERIZAÇÃO DOS GRÁFICOS (DEGRADÊ HEX DINÂMICO E CÓDIGO ESTÁVEL) ---
    # =========================================================================
    
    gerar_paleta_laranja <- function(n_barras) {
      if(n_barras <= 1) return(c("#D35400"))
      colorRampPalette(c("#D35400", "#FDEBD0"))(n_barras)
    }
    
    common_hc_opts <- function(hc, n_barras) {
      hc |> 
        hc_colors(gerar_paleta_laranja(n_barras)) |> 
        hc_xAxis(
          title = list(text = ""), 
          labels = list(
            style = list(
              color = "#000", 
              fontWeight = "500", 
              fontSize = "12px", 
              width = "130px",
              wordBreak = "break-word",
              whiteSpace = "normal"
            )
          )
        ) |>
        hc_yAxis(visible = FALSE) |>
        hc_plotOptions(bar = list(
          colorByPoint = TRUE, 
          dataLabels = list(
            enabled = TRUE, 
            allowOverlap = TRUE, 
            align = "left", 
            inside = FALSE, 
            style = list(
              color = "black", 
              fontWeight = "600", 
              fontSize = "12px", 
              textOutline = "none"
            ), 
            x = 5
          ), 
          borderWidth = 0
        )) |>
        hc_legend(enabled = FALSE)
    }
    
    output$plot_modalidade <- renderHighchart({ hchart(data_eq_mod(), "bar", hcaes(x="ppp_modali", y="Quantidade")) |> common_hc_opts(nrow(data_eq_mod())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> equipamentos') })
    output$plot_concedente <- renderHighchart({ hchart(data_eq_con(), "bar", hcaes(x="ppp_conced_display", y="Quantidade")) |> common_hc_opts(nrow(data_eq_con())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> equipamento(s)') })
    output$plot_zona <- renderHighchart({ hchart(data_eq_zona(), "bar", hcaes(x="zona", y="Quantidade")) |> common_hc_opts(nrow(data_eq_zona())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> equipamento(s)') })
    output$plot_parceria_modalidade <- renderHighchart({ hchart(data_par_mod(), "bar", hcaes(x="ppp_modali", y="Quantidade")) |> common_hc_opts(nrow(data_par_mod())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> parceria(s)') })
    output$plot_parceria_concedente <- renderHighchart({ hchart(data_par_con(), "bar", hcaes(x="ppp_conced_display", y="Quantidade")) |> common_hc_opts(nrow(data_par_con())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> parceria(s)') })
    output$plot_lote_zona <- renderHighchart({ hchart(data_par_zona(), "bar", hcaes(x="zona", y="Quantidade")) |> common_hc_opts(nrow(data_par_zona())) |> hc_tooltip(pointFormat = '<b>{point.y}</b> parceria(s)') })
  })
}