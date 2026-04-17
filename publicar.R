library(rsconnect)

rsconnect::setAccountInfo(name='SEU NOME',
                          token='SEU TOKEN',
                          secret='SEU SEGREDO')

rsconnect::deployApp(
  appDir = getwd(),
  appName = "SEU APP NAME", 
  account = "SUA CONTA",
  forceUpdate = TRUE
)

