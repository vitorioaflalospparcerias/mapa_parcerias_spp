# ===================================================================================
# LÓGICA DO SERVIDOR DO APLICATIVO SHINY
# ===================================================================================

server <- function(input, output, session) {
  
  # Inicialização de Módulos
  filtros_retorno <- filtrosServer(id = "filtros_app", dados_brutos = projetos)
  projetos_filtrados <- filtros_retorno$dados_filtrados
  
  # Construção da URL e abertura da nova aba
  observeEvent(input$btn_fullscreen, {
    url <- "?fullscreen=1"
    
    add_param <- function(u, p, v) {
      if (!is.null(v) && !("Todos" %in% v)) {
        v_enc <- URLencode(paste(v, collapse = "||"), reserved = TRUE)
        return(paste0(u, "&", p, "=", v_enc))
      }
      return(u)
    }
    
    sel <- filtros_retorno$selecoes()
    
    url <- add_param(url, "zona", sel$zona)
    url <- add_param(url, "distrito", sel$distrito)
    url <- add_param(url, "lote", sel$lote)
    url <- add_param(url, "nome", sel$nome)
    url <- add_param(url, "modalidade", sel$modalidade)
    url <- add_param(url, "concedente", sel$concedente)
    
    url <- paste0(url, "&cor=", filtros_retorno$categorizar_cor())
    url <- paste0(url, "&pins=", as.character(filtros_retorno$exibir_pins()))
    
    shinyjs::runjs(paste0("window.open('", url, "', '_blank');"))
  })
  
  # Lógica de tela cheia na própria aba
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
  
  # Controle de abertura de menus
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
  
  distrito_selecionado_reativo <- reactive({ if (is.null(filtros_retorno$selecoes()$distrito)) "Todos" else filtros_retorno$selecoes()$distrito })
  projeto_selecionado_reativo <- reactive({ if (is.null(filtros_retorno$selecoes()$nome)) "Todos" else filtros_retorno$selecoes()$nome })
  lote_selecionado_reativo <- reactive({ if (is.null(filtros_retorno$selecoes()$lote)) "Todos" else filtros_retorno$selecoes()$lote })
  
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