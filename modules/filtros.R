# --- FUNÇÃO HELPER: Cria os filtros sanfonados (Dropdowns) com SVG Nativo ---
criar_filtro_sanfona <- function(titulo, input_obj, is_open = FALSE) {
  display_style <- if(is_open) "display: block;" else "display: none;"
  chevron_class <- if(is_open) "chevron-icon open" else "chevron-icon"
  
  # Construção do ícone SVG idêntico ao Figma (círculo cinza + chevron)
  svg_icon <- HTML(paste0('
    <svg class="', chevron_class, '" width="22" height="22" viewBox="0 0 24 24" fill="none" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
      <circle cx="12" cy="12" r="10" stroke="#b0b0b0"></circle>
      <polyline points="7 10 12 15 17 10" stroke="#000000"></polyline>
    </svg>
  '))
  
  div(class = "filter-accordion-group",
      div(class = "filter-accordion-header",
          onclick = "
            var icone = $(this).find('svg.chevron-icon');
            icone.toggleClass('open');
            $(this).next('.filter-accordion-body').slideToggle(200);
          ",
          tags$span(titulo), svg_icon
      ),
      div(class = "filter-accordion-body", style = display_style,
          input_obj
      )
  )
}

### --- UI do Módulo de Filtros --- ###
filtrosUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$button(
      id = ns("btn_close"),
      type = "button",
      class = "btn-close-menu action-button",
      icon("times")
    ),
    
    div(class = "pill-toggle-container",
        actionButton(ns("tab_btn_filtros"), HTML("<i class='fa fa-sliders-h'></i> Filtros"), class = "pill-btn active"),
        actionButton(ns("tab_btn_downloads"), HTML("<i class='fa fa-download'></i> Downloads"), class = "pill-btn")
    ),
    
    div(id = ns("tab_content_filtros"),
        tags$h4("Filtros", class = "section-title"),
        
        criar_filtro_sanfona("Zona", selectizeInput(ns("filtro_zona"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        criar_filtro_sanfona("Distritos", selectizeInput(ns("filtro_distrito"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        criar_filtro_sanfona("Parceria", selectizeInput(ns("filtro_lote"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        criar_filtro_sanfona("Equipamento", selectizeInput(ns("filtro_nome"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        criar_filtro_sanfona("Modalidade", selectizeInput(ns("filtro_modalidade"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        criar_filtro_sanfona("Poder concedente", selectizeInput(ns("filtro_concedente"), NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos', plugins = list('remove_button')))),
        
        div(style="margin-top: 20px;",
            actionButton(
              inputId = ns("reset_filtros"),
              label = "Remover filtros",
              icon = icon("trash-alt"),
              class = "btn-outline-action"
            )
        ),
        
        tags$h4("Aparência", class = "section-title"),
        checkboxInput(
          ns("check_exibir_pins"), 
          label = tags$strong("Exibir equipamentos (pins)"), 
          value = TRUE
        ),
        
        radioButtons(
          ns("radio_cor_pin"),
          label = "Colorir equipamentos por:",
          choices = c(
            "Nenhum" = "nenhum",
            "Zona" = "zona",
            "Modalidade" = "modalidade",
            "Poder concedente" = "concedente"
          ),
          selected = "nenhum"
        )
    ),
    
    shinyjs::hidden(
      div(id = ns("tab_content_downloads"),
          tags$h4("Arquivos disponíveis", class = "section-title"),
          
          downloadButton(
            outputId = ns("download_dados"),
            label = "Dados filtrados (.xlsx)", 
            icon = icon("file-excel"), 
            class = "btn-outline-action"
          ),
          
          downloadButton(
            outputId = ns("download_shp"),
            label = "Shapefile (.shp)", 
            icon = icon("map"), 
            class = "btn-outline-action"
          )
      )
    )
  )
}

### --- Server do Módulo de Filtros --- ###
filtrosServer <- function(id, dados_brutos) {
  moduleServer(id, function(input, output, session) {
    
    # --- Controle das Abas Visuais Internas ---
    observeEvent(input$tab_btn_filtros, {
      shinyjs::show("tab_content_filtros")
      shinyjs::hide("tab_content_downloads")
      shinyjs::addClass("tab_btn_filtros", "active")
      shinyjs::removeClass("tab_btn_downloads", "active")
    })
    
    observeEvent(input$tab_btn_downloads, {
      shinyjs::show("tab_content_downloads")
      shinyjs::hide("tab_content_filtros")
      shinyjs::addClass("tab_btn_downloads", "active")
      shinyjs::removeClass("tab_btn_filtros", "active")
    })
    
    observeEvent(input$check_exibir_pins, {
      if (input$check_exibir_pins) {
        shinyjs::enable("radio_cor_pin")
      } else {
        shinyjs::disable("radio_cor_pin")
      }
    }, ignoreNULL = FALSE)
    
    # --- Leitura da URL para inicialização de estado ---
    query <- parseQueryString(isolate(session$clientData$url_search))
    parse_param <- function(param) {
      if (is.null(param)) return(NULL)
      unlist(strsplit(URLdecode(param), "\\|\\|"))
    }
    
    p_zona <- parse_param(query$zona)
    p_distrito <- parse_param(query$distrito)
    p_lote <- parse_param(query$lote)
    p_nome <- parse_param(query$nome)
    p_modal <- parse_param(query$modalidade)
    p_conced <- parse_param(query$concedente)
    
    observe({
      if (!is.null(query$cor)) updateRadioButtons(session, "radio_cor_pin", selected = query$cor)
      if (!is.null(query$pins)) updateCheckboxInput(session, "check_exibir_pins", value = as.logical(query$pins))
    })
    
    # --- Reativos de Filtro Cruzado ---
    get_choices <- function(column) { c("Todos" = "Todos", sort(unique(na.omit(column)))) }
    
    data_para_zona <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { df <- df |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { df <- df |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { df <- df |> filter(ppp_nome %in% input$filtro_nome) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { df <- df |> filter(ppp_modali %in% input$filtro_modalidade) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { df <- df |> filter(ppp_conced %in% input$filtro_concedente) }
      df
    })
    
    data_para_distrito <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { df <- df |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { df <- df |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { df <- df |> filter(ppp_nome %in% input$filtro_nome) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { df <- df |> filter(ppp_modali %in% input$filtro_modalidade) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { df <- df |> filter(ppp_conced %in% input$filtro_concedente) }
      df
    })
    
    data_para_lote <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { df <- df |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { df <- df |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { df <- df |> filter(ppp_nome %in% input$filtro_nome) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { df <- df |> filter(ppp_modali %in% input$filtro_modalidade) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { df <- df |> filter(ppp_conced %in% input$filtro_concedente) }
      df
    })
    
    data_para_nome <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { df <- df |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { df <- df |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { df <- df |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { df <- df |> filter(ppp_modali %in% input$filtro_modalidade) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { df <- df |> filter(ppp_conced %in% input$filtro_concedente) }
      df
    })
    
    data_para_modalidade <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { df <- df |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { df <- df |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { df <- df |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { df <- df |> filter(ppp_nome %in% input$filtro_nome) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { df <- df |> filter(ppp_conced %in% input$filtro_concedente) }
      df
    })
    
    data_para_concedente <- reactive({
      df <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { df <- df |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { df <- df |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { df <- df |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { df <- df |> filter(ppp_nome %in% input$filtro_nome) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { df <- df |> filter(ppp_modali %in% input$filtro_modalidade) }
      df
    })
    
    # Lógica que decide o valor inicial (da URL ou "Todos")
    get_initial_selection <- function(current_sel, new_choices, url_param) {
      if (is.null(current_sel) || length(current_sel) == 0) {
        if (!is.null(url_param) && all(url_param %in% new_choices)) { return(url_param) }
        return("Todos")
      }
      if (length(current_sel) > 1 && "Todos" %in% current_sel) {
        return(setdiff(current_sel, "Todos"))
      } else if (!all(current_sel %in% new_choices) && !"Todos" %in% current_sel) {
        return("Todos")
      } else {
        return(current_sel)
      }
    }
    
    update_choices <- function(input_id, new_choices, current_selection, url_param) {
      selected_val <- get_initial_selection(current_selection, new_choices, url_param)
      updateSelectizeInput(session, input_id, choices = new_choices, selected = selected_val)
    }
    
    observe({ update_choices("filtro_zona", get_choices(data_para_zona()$zona), input$filtro_zona, p_zona) })
    observe({ update_choices("filtro_distrito", get_choices(data_para_distrito()$NM_DIST), input$filtro_distrito, p_distrito) })
    observe({ update_choices("filtro_lote", get_choices(data_para_lote()$lote), input$filtro_lote, p_lote) })
    observe({ update_choices("filtro_nome", get_choices(data_para_nome()$ppp_nome), input$filtro_nome, p_nome) })
    observe({ update_choices("filtro_modalidade", get_choices(data_para_modalidade()$ppp_modali), input$filtro_modalidade, p_modal) })
    observe({ update_choices("filtro_concedente", get_choices(data_para_concedente()$ppp_conced), input$filtro_concedente, p_conced) })
    
    reset_trigger <- reactiveVal(0)
    
    observeEvent(input$reset_filtros, {
      updateSelectizeInput(session, "filtro_zona", selected = "Todos")
      updateSelectizeInput(session, "filtro_distrito", selected = "Todos")
      updateSelectizeInput(session, "filtro_lote", selected = "Todos")
      updateSelectizeInput(session, "filtro_modalidade", selected = "Todos")
      updateSelectizeInput(session, "filtro_concedente", selected = "Todos")
      updateSelectizeInput(session, "filtro_nome", selected = "Todos")
      reset_trigger(reset_trigger() + 1)
    })
    
    dados_filtrados_reativos <- reactive({
      dados_filtrados <- dados_brutos
      if (!is.null(input$filtro_zona) && !"Todos" %in% input$filtro_zona) { dados_filtrados <- dados_filtrados |> filter(zona %in% input$filtro_zona) }
      if (!is.null(input$filtro_distrito) && !"Todos" %in% input$filtro_distrito) { dados_filtrados <- dados_filtrados |> filter(NM_DIST %in% input$filtro_distrito) }
      if (!is.null(input$filtro_lote) && !"Todos" %in% input$filtro_lote) { dados_filtrados <- dados_filtrados |> filter(lote %in% input$filtro_lote) }
      if (!is.null(input$filtro_modalidade) && !"Todos" %in% input$filtro_modalidade) { dados_filtrados <- dados_filtrados |> filter(ppp_modali %in% input$filtro_modalidade) }
      if (!is.null(input$filtro_concedente) && !"Todos" %in% input$filtro_concedente) { dados_filtrados <- dados_filtrados |> filter(ppp_conced %in% input$filtro_concedente) }
      if (!is.null(input$filtro_nome) && !"Todos" %in% input$filtro_nome) { dados_filtrados <- dados_filtrados |> filter(ppp_nome %in% input$filtro_nome) }
      return(dados_filtrados)
    })
    
    output$download_dados <- downloadHandler(
      filename = function() { paste0("dados_filtrados_", Sys.Date(), ".xlsx") },
      content = function(file) {
        dados_para_salvar <- dados_filtrados_reativos() |>
          sf::st_drop_geometry() |>
          select(Equipamento = ppp_nome, Modalidade = ppp_modali, `Poder Concedente` = ppp_conced, Parceria = lote, Distrito = NM_DIST, Zona = zona, `Área (m2)` = ppp_area, `Investimento (R$)` = ppp_invest, `Concessionária` = ppp_conces, `Contrato` = ppp_contra, `Ano Assinatura` = ppp_assina, `Link` = ppp_link1)
        writexl::write_xlsx(dados_para_salvar, file)
      }
    )
    
    output$download_shp <- downloadHandler(
      filename = function() { paste0("shapefile_parcerias_", Sys.Date(), ".zip") },
      content = function(file) {
        temp_dir <- tempdir()
        shp_dir <- file.path(temp_dir, "shp_export")
        if (dir.exists(shp_dir)) { unlink(shp_dir, recursive = TRUE) }
        dir.create(shp_dir)
        
        dados_espaciais <- dados_filtrados_reativos() |>
          select(
            Equipament = ppp_nome, Modalidade = ppp_modali, Concedente = ppp_conced, Parceria = lote, Distrito = NM_DIST, Zona = zona, Area_m2 = ppp_area, Invest = ppp_invest, Concession = ppp_conces, Contrato = ppp_contra, Ano_Assina = ppp_assina, Link = ppp_link1
          )
        
        caminho_shp <- file.path(shp_dir, "parcerias.shp")
        sf::st_write(obj = dados_espaciais, dsn = caminho_shp, driver = "ESRI Shapefile", delete_layer = TRUE, quiet = TRUE)
        
        owd <- setwd(shp_dir)
        on.exit(setwd(owd))
        zip::zip(zipfile = file, files = list.files())
      },
      contentType = "application/zip"
    )
    
    return(list(dados_filtrados = dados_filtrados_reativos, exibir_pins = reactive({ input$check_exibir_pins }), categorizar_cor = reactive({ input$radio_cor_pin }), reset_trigger = reset_trigger))
  })
}