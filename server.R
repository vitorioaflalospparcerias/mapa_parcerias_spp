# ===================================================================================
# OBJETIVO PRINCIPAL: LÓGICA DO SERVIDOR DO APLICATIVO SHINY
# ===================================================================================

server <- function(input, output, session) {
  
  # =================================================================================
  # --- LOGICA DE ABERTURA DOS MENUS FLUTUANTES ---
  # =================================================================================
  
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
  
  # =================================================================================
  # --- LOGICA DO WIDESCREEN ---
  # =================================================================================
  observeEvent(input$btn_fullscreen, {
    shinyjs::toggleClass(selector = "body", class = "widescreen-mode")
    
    shinyjs::runjs("
      var btnIcon = $('#btn_fullscreen i');
      var isWide = $('body').hasClass('widescreen-mode');
      
      if(isWide) {
        btnIcon.removeClass('fa-expand').addClass('fa-times');
        $('#painel_wrapper').hide();
        $('#floating_triggers').hide();
        $('#painel_wrapper_right').hide();
        $('#floating_triggers_right').hide();
        
        // Retorna botão para a ponta
        $('#btn_fullscreen').css('right', '15px');
      } else {
        btnIcon.removeClass('fa-times').addClass('fa-expand');
        $('#floating_triggers').show();
        $('#floating_triggers_right').show();
        $('#btn_fullscreen').css('right', '65px');
      }
      
      setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 300);
    ")
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