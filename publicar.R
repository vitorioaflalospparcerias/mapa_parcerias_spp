library(rsconnect)
rsconnect::setAccountInfo(name='saopaulo-parcerias',
                          token='5516061E976CA6462D88359A35243FB9',
                          secret='45kkAUSpXYPFiykuFZ92erBH+Rpm7ZkPkMtwdvQU')


#rsconnect::setAccountInfo(name='vitorioaflalo',
#                          token='7A27EAC28E7D90E00CDBB049BEB5E484',
#                          secret='lnlbEWUAoCwUvSLaJ2Wmmce8mjC2POlbTLufd4YO')

rsconnect::deployApp(
  appDir = getwd(),
  appName = "parcerias-mapa-sp", 
  account = "saopaulo-parcerias",
  forceUpdate = TRUE
)

