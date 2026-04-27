# ===================================================================================
# DEFINIÇÃO DA INTERFACE DO USUÁRIO (FRONT-END)
# ===================================================================================

ui <- function(req) {
  query <- parseQueryString(req$QUERY_STRING)
  is_fullscreen <- !is.null(query$fullscreen)
  
  fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(version = 5),
    fluid = TRUE,
    # Aplica a classe widescreen automaticamente se abrir via nova aba
    class = if(is_fullscreen) "widescreen-mode" else "",
    
    tags$head(
      tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-H6VB5LQZKZ"),
      tags$script(HTML("
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-H6VB5LQZKZ'); 
      ")),
      
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css?v=3"),
      tags$script(src = "script.js?v=3")
    ),
    
    div(class = "custom-title-panel",
        div(class = "title-wrapper",
            div(class = "title-left",
                tags$img(src = "imgs/logo_spp.png", height = "35px"),
                tags$img(src = "imgs/logo_pref.png", height = "35px")
            ),
            div(class = "title-center", tags$h1("Mapa das Parcerias")),
            div(class = "title-right") 
        )
    ),
    
    div(id = "main_content_grid", class = "main-content-wrapper",
        div(class = "map-column",
            
            # Botões agora são universais (aparecem em ambas as abas)
            tagList(
              tags$a(
                id = "btn_fullscreen",
                class = "btn-fullscreen action-button",
                icon("external-link-alt"),
                title = "Abrir em nova aba: Espelha o mapa e os filtros atuais em uma nova guia",
                style = "display: flex; align-items: center; justify-content: center; text-decoration: none;"
              ),
              tags$a(
                id = "btn_expand",
                class = "btn-fullscreen action-button",
                icon(if(is_fullscreen) "compress" else "expand"),
                title = if(is_fullscreen) "Restaurar layout original" else "Tela cheia: Oculta os painéis e o cabeçalho para focar apenas no mapa",
                style = "top: 54px; display: flex; align-items: center; justify-content: center; text-decoration: none;"
              )
            ),
            
            div(id = "floating_triggers", class = "floating-triggers",
                actionButton("btn_open_filtros", label = NULL, icon = icon("sliders-h")),
                actionButton("btn_open_downloads", label = NULL, icon = icon("download"))
            ),
            
            div(id = "painel_wrapper", filtrosUI("filtros_app")),
            
            div(id = "floating_triggers_right", class = "floating-triggers-right",
                actionButton("btn_open_destaques", label = NULL, icon = icon("star")),
                actionButton("btn_open_graficos", label = NULL, icon = icon("chart-bar"))
            ),
            
            div(id = "painel_wrapper_right", graficosUI("graficos_main")),
            
            div(class = "map-panel", mapaUI("mapa"))
        )
    )
  )
}