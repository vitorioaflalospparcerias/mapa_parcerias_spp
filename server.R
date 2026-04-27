# ===================================================================================
# LÓGICA DO SERVIDOR DO APLICATIVO SHINY
# ===================================================================================

server <- function(input, output, session) {
  
  observeEvent(input$btn_fullscreen, {
    url <- "?fullscreen=1"
    
    add_param <- function(u, p, v) {
      if (!is.null(v) && !("Todos" %in% v)) {
        v_enc <- URLencode(paste(v, collapse = "||"), reserved = TRUE)
        return(paste0(u, "&", p, "=", v_enc))
      }
      return(u)
    }
    
    url <- add_param(url, "zona", input[["filtros_app-filtro_zona"]])
    url <- add_param(url, "distrito", input[["filtros_app-filtro_distrito"]])
    url <- add_param(url, "lote", input[["filtros_app-filtro_lote"]])
    url <- add_param(url, "nome", input[["filtros_app-filtro_nome"]])
    url <- add_param(url, "modalidade", input[["filtros_app-filtro_modalidade"]])
    url <- add_param(url, "concedente", input[["filtros_app-filtro_concedente"]])
    
    url <- paste0(url, "&cor=", input[["filtros_app-radio_cor_pin"]])
    url <- paste0(url, "&pins=", as.character(input[["filtros_app-check_exibir_pins"]]))
    
    shinyjs::runjs(paste0("window.open('", url, "', '_blank');"))
  })
  
  observeEvent(input$btn_expand, {
    shinyjs::toggleClass(selector = "body", class = "widescreen-mode")
    
    shinyjs::runjs("
      var icon = $('#btn_expand i');
      if(icon.hasClass('fa-expand')) {
        icon.removeClass('fa-expand').addClass('fa-compress');
        $('#btn_expand').attr('title', 'Restaurar layout original');
      } else {
        icon.removeClass('fa-compress').addClass('fa-expand');
        $('#btn_expand').attr('title', 'Tela cheia: Oculta os painéis e o cabeçalho para focar apenas no mapa');
      }
    ")
  })
  
  observeEvent(input$btn_open_filtros, {
    shinyjs::hide("floating_triggers")
    shinyjs::show("painel_wrapper")    
    shinyjs::click("filtros_app-tab_btn_filtros")
  })
  
  observeEvent(input$btn_open_downloads, {
    shinyjs::hide("floating_triggers")
    shinyjs::show("painel_wrapper")    
    shinyjs::click("filtros_app-tab_btn_downloads") 
  })
  
  observeEvent(input[["filtros_app-btn_close"]], {
    shinyjs::hide("painel_wrapper")     
    shinyjs::show("floating_triggers")  
  })
  
  observeEvent(input$btn_open_destaques, {
    shinyjs::hide("floating_triggers_right")
    shinyjs::show("painel_wrapper_right")    
    shinyjs::click("graficos_main-tab_btn_destaques")
  })
  
  observeEvent(input$btn_open_graficos, {
    shinyjs::hide("floating_triggers_right")
    shinyjs::show("painel_wrapper_right")    
    shinyjs::click("graficos_main-tab_btn_graficos")
  })
  
  observeEvent(input[["graficos_main-btn_close_graficos"]], {
    shinyjs::hide("painel_wrapper_right")     
    shinyjs::show("floating_triggers_right")  
  })
  
  filtros_retorno <- filtrosServer(id = "filtros_app", dados_brutos = projetos)
  projetos_filtrados <- filtros_retorno$dados_filtrados
  
  distrito_selecionado_reativo <- reactive({ input[["filtros_app-filtro_distrito"]] })
  projeto_selecionado_reativo <- reactive({ input[["filtros_app-filtro_nome"]] })
  lote_selecionado_reativo <- reactive({ input[["filtros_app-filtro_lote"]] })
  
  mapaServer(
    id = "mapa",
    projetos_reativos = projetos_filtrados,
    distritos_estaticos = distritos,
    distrito_selecionado = distrito_selecionado_reativo,
    projeto_selecionado = projeto_selecionado_reativo,
    lote_selecionado = lote_selecionado_reativo,
    distritos_sem_equip_estaticos = distritos_sem_equipamentos,
    exibir_pins = filtros_retorno$exibir_pins,
    categorizar_cor = filtros_retorno$categorizar_cor,
    reset_trigger = filtros_retorno$reset_trigger
  )
  
  graficosServer(id = "graficos_main", dados_filtrados = projetos_filtrados)
}