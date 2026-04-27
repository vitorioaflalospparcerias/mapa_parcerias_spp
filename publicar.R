library(rsconnect)
rsconnect::setAccountInfo(name='SEU NOME',
                          token='SEU TOKEN',
                          secret='SEU SEGREDO')



rsconnect::deployApp(
  appDir = getwd(),
  appName = "parcerias-mapa-sp", 
  account = "saopaulo-parcerias",
  forceUpdate = TRUE
)

