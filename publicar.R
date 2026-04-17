library(rsconnect)
#rsconnect::setAccountInfo(name = 'saopaulo-parcerias',
#                          token='56EE1707084C7F0979772937C8F07318',
#                          secret='BM316BYtkhoNLClscYbassBoJAbUFLICD1LaPX4E')


rsconnect::setAccountInfo(name='vitorioaflalo',
                          token='7A27EAC28E7D90E00CDBB049BEB5E484',
                          secret='lnlbEWUAoCwUvSLaJ2Wmmce8mjC2POlbTLufd4YO')

rsconnect::deployApp(
  appDir = getwd(),
  appName = "parcerias-mapa-sp", 
  account = "vitorioaflalo",
  forceUpdate = TRUE
)

