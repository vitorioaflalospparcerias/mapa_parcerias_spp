# ===================================================================================
# OBJETIVO PRINCIPAL: DEFINIÇÃO DA INTERFACE DO USUÁRIO (FRONT-END)
# ===================================================================================

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5),
  fluid = TRUE,
  
  tags$head(
    # --- GOOGLE ANALYTICS ---
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-H6VB5LQZKZ"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-H6VB5LQZKZ'); 
    ")),
    
    # --- ARQUIVOS EXTERNOS (Pasta /www) ---
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js")
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
          
          tags$button(
            id = "btn_fullscreen",
            class = "btn-fullscreen action-button",
            icon("expand")
          ),
          
          div(id = "floating_triggers", class = "floating-triggers",
              actionButton("btn_open_filtros", label = NULL, icon = icon("sliders-h")),
              actionButton("btn_open_downloads", label = NULL, icon = icon("download"))
          ),
          
          div(id = "painel_wrapper",
              filtrosUI("filtros_app")
          ),
          
          div(id = "floating_triggers_right", class = "floating-triggers-right",
              actionButton("btn_open_destaques", label = NULL, icon = icon("star")),
              actionButton("btn_open_graficos", label = NULL, icon = icon("chart-bar"))
          ),
          
          div(id = "painel_wrapper_right",
              graficosUI("graficos_main")
          ),
          
          div(class = "map-panel", mapaUI("mapa"))
      )
  )
)