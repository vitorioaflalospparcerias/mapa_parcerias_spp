# --- UI DO MÓDULO ---
mapaUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("mapa_distritos"), height = "100%", width = "100%")
  )
}

# --- SERVER DO MÓDULO ---
mapaServer <- function(id, projetos_reativos, distritos_estaticos, distrito_selecionado, projeto_selecionado, lote_selecionado, distritos_sem_equip_estaticos, exibir_pins, categorizar_cor, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    
    zoom_ja_feito <- reactiveVal(FALSE)
    
    # --- MAPA BASE ---
    output$mapa_distritos <- renderLeaflet({
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addTiles() |>
        addPolygons(
          data = distritos_estaticos,
          fillColor = "#FFE5B4",
          color = "black", 
          weight = 1, 
          opacity = 1, 
          fillOpacity = 0.5,
          label = ~NM_DIST, 
          layerId = ~NM_DIST
        ) |>
        addPolygons(
          data = distritos_sem_equip_estaticos,
          fillColor = "#EAEAEA",
          color = "black", 
          weight = 1, 
          opacity = 1, 
          fillOpacity = 0.7,
          label = ~paste(NM_DIST, "(sem equipamentos)")
        ) |>
        addLegend(
          position = "bottomright",
          colors = c("#FFE5B4", "#EAEAEA"), 
          labels = c("Distrito com equipamento(s)", "Distrito sem equipamentos"),
          title = "Legenda de Distritos"
        ) |>
        onRender("function(el, x) {
          L.control.zoom({position: 'bottomright'}).addTo(this);
        }")
    })
    
    observe({
      proxy <- leafletProxy("mapa_distritos")
      
      proxy |> clearGroup("projetos") |> clearMarkerClusters() |> removeControl("legenda_pins_dinamica")
      req(isTRUE(exibir_pins()))
      
      dados_para_plotar <- projetos_reativos()
      req(nrow(dados_para_plotar) > 0)
      
      categoria_selecionada <- categorizar_cor()
      
      if (categoria_selecionada == "nenhum") {
        proxy |>
          addAwesomeMarkers(
            data = dados_para_plotar,
            group = "projetos",
            clusterOptions = markerClusterOptions(),
            popup = ~lapply(label_html, HTML), 
            icon = awesomeIcons(
              icon = 'info-sign', 
              library = 'glyphicon', 
              markerColor = 'orange', 
              iconColor = '#FFFFFF'
            )
          )
        
      } else {
        coluna_para_cor <- switch(
          categoria_selecionada,
          "modalidade" = "ppp_modali",
          "zona" = "zona",
          "concedente" = "ppp_conced"
        )
        
        dados_filtrados <- dados_para_plotar |> filter(!is.na(.data[[coluna_para_cor]]))
        
        if (nrow(dados_filtrados) > 0) {
          
          categorias_unicas <- sort(unique(dados_filtrados[[coluna_para_cor]]))
          
          mapa_nomes <- setNames(rep_len(CORES_VALIDAS, length.out = length(categorias_unicas)), categorias_unicas)
          mapa_hex <- setNames(CORES_HEX[mapa_nomes], categorias_unicas)
          
          vetor_cores_marcadores <- unname(mapa_nomes[as.character(dados_filtrados[[coluna_para_cor]])])
          
          proxy |>
            addAwesomeMarkers(
              data = dados_filtrados,
              group = "projetos",
              clusterOptions = markerClusterOptions(), 
              popup = ~lapply(label_html, HTML), 
              icon = awesomeIcons(
                icon = 'info-sign',
                library = 'glyphicon',
                markerColor = vetor_cores_marcadores,
                iconColor = '#FFFFFF'
              )
            ) |>
            addLegend(
              position = "bottomleft",
              colors = unname(mapa_hex),
              labels = names(mapa_hex),
              title = switch(categoria_selecionada,
                             "modalidade" = "Modalidade",
                             "zona" = "Zona",
                             "concedente" = "Poder Concedente"),
              layerId = "legenda_pins_dinamica"
            )
        }
      }
    })
    
    # --- LÓGICA DE ZOOM ---
    zoom_out_mapa <- function(proxy) {
      bbox <- st_bbox(distritos_estaticos) %>% as.vector()
      proxy %>% flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
      zoom_ja_feito(TRUE) 
    }
    
    observeEvent(reset_trigger(), {
      req(reset_trigger() > 0)
      mapa_proxy <- leafletProxy("mapa_distritos")
      zoom_out_mapa(mapa_proxy)
      zoom_ja_feito(FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(projeto_selecionado(), {
      selecionados <- projeto_selecionado()
      if ("Todos" %in% selecionados) { zoom_ja_feito(FALSE); return() }
      
      mapa_proxy <- leafletProxy("mapa_distritos")
      
      if (length(selecionados) == 1 && !zoom_ja_feito()) {
        projeto_zoom <- projetos_reativos() %>% filter(ppp_nome == selecionados)
        if (nrow(projeto_zoom) > 0) {
          coords <- sf::st_coordinates(projeto_zoom)
          mapa_proxy %>% flyTo(lng = coords[1], lat = coords[2], zoom = 17)
          zoom_ja_feito(TRUE)
        }
      } else if (length(selecionados) > 1) { zoom_out_mapa(mapa_proxy) }
    }, ignoreInit = TRUE)
    
    observeEvent(lote_selecionado(), {
      if (length(projeto_selecionado()) > 1 || !"Todos" %in% projeto_selecionado()) { return() }
      selecionados_lote <- lote_selecionado()
      if ("Todos" %in% selecionados_lote) { zoom_ja_feito(FALSE); return() }
      
      mapa_proxy <- leafletProxy("mapa_distritos")
      
      if (length(selecionados_lote) == 1 && !zoom_ja_feito()) {
        dados_do_lote <- projetos_reativos()
        if (nrow(dados_do_lote) > 0) {
          n_equip <- nrow(dados_do_lote)
          if(n_equip == 1) {
            coords <- sf::st_coordinates(dados_do_lote)
            mapa_proxy %>% flyTo(lng = coords[1], lat = coords[2], zoom = 17)
          } else {
            bbox <- st_bbox(dados_do_lote) %>% as.vector()
            mapa_proxy %>% flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
          }
          zoom_ja_feito(TRUE)
        }
      } else if (length(selecionados_lote) > 1) { zoom_out_mapa(mapa_proxy) }
    }, ignoreInit = TRUE)
    
    observeEvent(distrito_selecionado(), {
      if ( (length(projeto_selecionado()) > 1 || !"Todos" %in% projeto_selecionado()) || (length(lote_selecionado()) > 1 || !"Todos" %in% lote_selecionado()) ) { return() }
      selecionados <- distrito_selecionado()
      if ("Todos" %in% selecionados) { zoom_ja_feito(FALSE); return() }
      
      mapa_proxy <- leafletProxy("mapa_distritos")
      
      if (length(selecionados) == 1 && !zoom_ja_feito()) {
        distrito_zoom <- distritos_estaticos %>% filter(NM_DIST == selecionados)
        if (nrow(distrito_zoom) > 0) {
          bbox <- st_bbox(distrito_zoom) %>% as.vector()
          mapa_proxy %>% flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
          zoom_ja_feito(TRUE)
        }
      } else if (length(selecionados) > 1) { zoom_out_mapa(mapa_proxy) }
    }, ignoreInit = TRUE)
    
  })
}