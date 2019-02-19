# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")
library("RColorBrewer")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")

#BOD totale
tsCaricoBODINtotal<-(na.omit(xts(x=Caprino[,15], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoBODINtotal<-mean(tsCaricoBODINtotal)
tsCaricoBODOUTtotal<-(na.omit(xts(x=Caprino[,16], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoBODOUTtotal<-mean(tsCaricoBODOUTtotal)
rendBODtotal<-(mediaCaricoBODINtotal - mediaCaricoBODOUTtotal)/mediaCaricoBODINtotal*100
aeBODtotal<-mediaCaricoBODINtotal/60*1000
#BOD 2015
tsCaricoBODIN2015<-tsCaricoBODINtotal["2015"]
mediaCaricoBODIN2015<-mean(tsCaricoBODIN2015)
tsCaricoBODOUT2015<-tsCaricoBODOUTtotal["2015"]
mediaCaricoBODOUT2015<-mean(tsCaricoBODOUT2015)
rendBOD2015<-(mediaCaricoBODIN2015 - mediaCaricoBODOUT2015)/mediaCaricoBODIN2015*100
aeBOD2015<-mediaCaricoBODIN2015/60*1000
percentileBOD2015<-quantile(tsCaricoBODIN2015/60*1000,c(0.90))
#BOD 2016
tsCaricoBODIN2016<-tsCaricoBODINtotal["2016"]
mediaCaricoBODIN2016<-mean(tsCaricoBODIN2016)
tsCaricoBODOUT2016<-tsCaricoBODOUTtotal["2016"]
mediaCaricoBODOUT2016<-mean(tsCaricoBODOUT2016)
rendBOD2016<-(mediaCaricoBODIN2016 - mediaCaricoBODOUT2016)/mediaCaricoBODIN2016*100
aeBOD2016<-mediaCaricoBODIN2016/60*1000
percentileBOD2016<-quantile(tsCaricoBODIN2016/60*1000,c(0.90))
#BOD 2017
tsCaricoBODIN2017<-tsCaricoBODINtotal["2017"]
mediaCaricoBODIN2017<-mean(tsCaricoBODIN2017)
tsCaricoBODOUT2017<-tsCaricoBODOUTtotal["2017"]
mediaCaricoBODOUT2017<-mean(tsCaricoBODOUT2017)
rendBOD2017<-(mediaCaricoBODIN2017 - mediaCaricoBODOUT2017)/mediaCaricoBODIN2017*100
aeBOD2017<-mediaCaricoBODIN2017/60*1000
percentileBOD2017<-quantile(tsCaricoBODIN2017/60*1000,c(0.90))
#BOD 2018
tsCaricoBODIN2018<-tsCaricoBODINtotal["2018"]
mediaCaricoBODIN2018<-mean(tsCaricoBODIN2018)
tsCaricoBODOUT2018<-tsCaricoBODOUTtotal["2018"]
mediaCaricoBODOUT2018<-mean(tsCaricoBODOUT2018)
rendBOD2018<-(mediaCaricoBODIN2018 - mediaCaricoBODOUT2018)/mediaCaricoBODIN2018*100
aeBOD2018<-mediaCaricoBODIN2018/60*1000
percentileBOD2018<-quantile(tsCaricoBODIN2018/60*1000,c(0.90))

rendimentiBOD<-c(rendBOD2015,rendBOD2016,rendBOD2017,rendBOD2018)
aeBOD<-c(aeBOD2015,aeBOD2016,aeBOD2017,aeBOD2018)


#COD totale
tsCaricoCODINtotal<-(na.omit(xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoCODINtotal<-mean(tsCaricoCODINtotal)
tsCaricoCODOUTtotal<-(na.omit(xts(x=Caprino[,18], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoCODOUTtotal<-mean(tsCaricoCODOUTtotal)
rendCODtotal<-(mediaCaricoCODINtotal - mediaCaricoCODOUTtotal)/mediaCaricoCODINtotal*100
aeCODtotal<-mediaCaricoCODINtotal/120*1000
#COD 2015
tsCaricoCODIN2015<-tsCaricoCODINtotal["2015"]
mediaCaricoCODIN2015<-mean(tsCaricoCODIN2015)
tsCaricoCODOUT2015<-tsCaricoCODOUTtotal["2015"]
mediaCaricoCODOUT2015<-mean(tsCaricoCODOUT2015)
rendCOD2015<-(mediaCaricoCODIN2015 - mediaCaricoCODOUT2015)/mediaCaricoCODIN2015*100
aeCOD2015<-mediaCaricoCODIN2015/120*1000
percentileCOD2015<-quantile(tsCaricoCODIN2015/120*1000,c(0.92))
#COD 2016
tsCaricoCODIN2016<-tsCaricoCODINtotal["2016"]
mediaCaricoCODIN2016<-mean(tsCaricoCODIN2016)
tsCaricoCODOUT2016<-tsCaricoCODOUTtotal["2016"]
mediaCaricoCODOUT2016<-mean(tsCaricoCODOUT2016)
rendCOD2016<-(mediaCaricoCODIN2016 - mediaCaricoCODOUT2016)/mediaCaricoCODIN2016*100
aeCOD2016<-mediaCaricoCODIN2016/120*1000
percentileCOD2016<-quantile(tsCaricoCODIN2016/120*1000,c(0.92))
#COD 2017
tsCaricoCODIN2017<-tsCaricoCODINtotal["2017"]
mediaCaricoCODIN2017<-mean(tsCaricoCODIN2017)
tsCaricoCODOUT2017<-tsCaricoCODOUTtotal["2017"]
mediaCaricoCODOUT2017<-mean(tsCaricoCODOUT2017)
rendCOD2017<-(mediaCaricoCODIN2017 - mediaCaricoCODOUT2017)/mediaCaricoCODIN2017*100
aeCOD2017<-mediaCaricoCODIN2017/120*1000
percentileCOD2017<-quantile(tsCaricoCODIN2017/120*1000,c(0.92))
#COD 2018
tsCaricoCODIN2018<-tsCaricoCODINtotal["2018"]
mediaCaricoCODIN2018<-mean(tsCaricoCODIN2018)
tsCaricoCODOUT2018<-tsCaricoCODOUTtotal["2018"]
mediaCaricoCODOUT2018<-mean(tsCaricoCODOUT2018)
rendCOD2018<-(mediaCaricoCODIN2018 - mediaCaricoCODOUT2018)/mediaCaricoCODIN2018*100
aeCOD2018<-mediaCaricoCODIN2018/120*1000
percentileCOD2018<-quantile(tsCaricoCODIN2018/120*1000,c(0.92))

rendimentiCOD<-c(rendCOD2015,rendCOD2016,rendCOD2017,rendCOD2018)
aeCOD<-c(aeCOD2015,aeCOD2016,aeCOD2017,aeCOD2018)


#Ntot totale
tsCaricoNtotINtotal<-(na.omit(xts(x=Caprino[,29], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoNtotINtotal<-mean(tsCaricoNtotINtotal)
tsCaricoNtotOUTtotal<-(na.omit(xts(x=Caprino[,30], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoNtotOUTtotal<-mean(tsCaricoNtotOUTtotal)
rendNtottotal<-(mediaCaricoNtotINtotal - mediaCaricoNtotOUTtotal)/mediaCaricoNtotINtotal*100
aeNtottotal<-mediaCaricoNtotINtotal/12*1000
#Ntot 2015
tsCaricoNtotIN2015<-tsCaricoNtotINtotal["2015"]
mediaCaricoNtotIN2015<-mean(tsCaricoNtotIN2015)
tsCaricoNtotOUT2015<-tsCaricoNtotOUTtotal["2015"]
mediaCaricoNtotOUT2015<-mean(tsCaricoNtotOUT2015)
rendNtot2015<-(mediaCaricoNtotIN2015 - mediaCaricoNtotOUT2015)/mediaCaricoNtotIN2015*100
aeNtot2015<-mediaCaricoNtotIN2015/12*1000
percentileN2015<-quantile(tsCaricoNtotIN2015/12*1000,c(0.92))
#Ntot 2016
tsCaricoNtotIN2016<-tsCaricoNtotINtotal["2016"]
mediaCaricoNtotIN2016<-mean(tsCaricoNtotIN2016)
tsCaricoNtotOUT2016<-tsCaricoNtotOUTtotal["2016"]
mediaCaricoNtotOUT2016<-mean(tsCaricoNtotOUT2016)
rendNtot2016<-(mediaCaricoNtotIN2016 - mediaCaricoNtotOUT2016)/mediaCaricoNtotIN2016*100
aeNtot2016<-mediaCaricoNtotIN2016/12*1000
percentileN2016<-quantile(tsCaricoNtotIN2016/12*1000,c(0.92))
#Ntot 2017
tsCaricoNtotIN2017<-tsCaricoNtotINtotal["2017"]
mediaCaricoNtotIN2017<-mean(tsCaricoNtotIN2017)
tsCaricoNtotOUT2017<-tsCaricoNtotOUTtotal["2017"]
mediaCaricoNtotOUT2017<-mean(tsCaricoNtotOUT2017)
rendNtot2017<-(mediaCaricoNtotIN2017 - mediaCaricoNtotOUT2017)/mediaCaricoNtotIN2017*100
aeNtot2017<-mediaCaricoNtotIN2017/12*1000
percentileN2017<-quantile(tsCaricoNtotIN2017/12*1000,c(0.92))
#Ntot 2018
tsCaricoNtotIN2018<-tsCaricoNtotINtotal["2018"]
mediaCaricoNtotIN2018<-mean(tsCaricoNtotIN2018)
tsCaricoNtotOUT2018<-tsCaricoNtotOUTtotal["2018"]
mediaCaricoNtotOUT2018<-mean(tsCaricoNtotOUT2018)
rendNtot2018<-(mediaCaricoNtotIN2018 - mediaCaricoNtotOUT2018)/mediaCaricoNtotIN2018*100
aeNtot2018<-mediaCaricoNtotIN2018/12*1000
percentileN2018<-quantile(tsCaricoNtotIN2018/12*1000,c(0.92))

rendimentiNtot<-c(rendNtot2015,rendNtot2016,rendNtot2017,rendNtot2018)
aeNtot<-c(aeNtot2015,aeNtot2016,aeNtot2017,aeNtot2018)


#Ptot totale
tsCaricoPtotINtotal<-(na.omit(xts(x=Caprino[,27], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoPtotINtotal<-mean(tsCaricoPtotINtotal)
tsCaricoPtotOUTtotal<-(na.omit(xts(x=Caprino[,28], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoPtotOUTtotal<-mean(tsCaricoPtotOUTtotal)
rendPtottotal<-(mediaCaricoPtotINtotal - mediaCaricoPtotOUTtotal)/mediaCaricoPtotINtotal*100
aePtottotal<-mediaCaricoPtotINtotal/1.2*1000
#Ptot 2015
tsCaricoPtotIN2015<-tsCaricoPtotINtotal["2015"]
mediaCaricoPtotIN2015<-mean(tsCaricoPtotIN2015)
tsCaricoPtotOUT2015<-tsCaricoPtotOUTtotal["2015"]
mediaCaricoPtotOUT2015<-mean(tsCaricoPtotOUT2015)
rendPtot2015<-(mediaCaricoPtotIN2015 - mediaCaricoPtotOUT2015)/mediaCaricoPtotIN2015*100
aePtot2015<-mediaCaricoPtotIN2015/1.2*1000
percentileP2015<-quantile(tsCaricoPtotIN2015/1.2*1000,c(0.92))
#Ptot 2016
tsCaricoPtotIN2016<-tsCaricoPtotINtotal["2016"]
mediaCaricoPtotIN2016<-mean(tsCaricoPtotIN2016)
tsCaricoPtotOUT2016<-tsCaricoPtotOUTtotal["2016"]
mediaCaricoPtotOUT2016<-mean(tsCaricoPtotOUT2016)
rendPtot2016<-(mediaCaricoPtotIN2016 - mediaCaricoPtotOUT2016)/mediaCaricoPtotIN2016*100
aePtot2016<-mediaCaricoPtotIN2016/1.2*1000
percentileP2016<-quantile(tsCaricoPtotIN2016/1.2*1000,c(0.92))
#Ptot 2017
tsCaricoPtotIN2017<-tsCaricoPtotINtotal["2017"]
mediaCaricoPtotIN2017<-mean(tsCaricoPtotIN2017)
tsCaricoPtotOUT2017<-tsCaricoPtotOUTtotal["2017"]
mediaCaricoPtotOUT2017<-mean(tsCaricoPtotOUT2017)
rendPtot2017<-(mediaCaricoPtotIN2017 - mediaCaricoPtotOUT2017)/mediaCaricoPtotIN2017*100
aePtot2017<-mediaCaricoPtotIN2017/1.2*1000
percentileP2017<-quantile(tsCaricoPtotIN2017/1.2*1000,c(0.92))
#Ptot 2018
tsCaricoPtotIN2018<-tsCaricoPtotINtotal["2018"]
mediaCaricoPtotIN2018<-mean(tsCaricoPtotIN2018)
tsCaricoPtotOUT2018<-tsCaricoPtotOUTtotal["2018"]
mediaCaricoPtotOUT2018<-mean(tsCaricoPtotOUT2018)
rendPtot2018<-(mediaCaricoPtotIN2018 - mediaCaricoPtotOUT2018)/mediaCaricoPtotIN2018*100
aePtot2018<-mediaCaricoPtotIN2018/1.2*1000
percentileP2018<-quantile(tsCaricoPtotIN2018/1.2*1000,c(0.92))

rendimentiPtot<-c(rendPtot2015,rendPtot2016,rendPtot2017,rendPtot2018)
aePtot<-c(aePtot2015,aePtot2016,aePtot2017,aePtot2018)

#percentili<-c(percentileBOD2015,percentileCOD2015,percentileN2015,percentileP2015,percentileBOD2016,percentileCOD2016,percentileN2016,percentileP2016,percentileBOD2017,percentileCOD2017,percentileN2017,percentileP2017,percentileBOD2018,percentileCOD2018,percentileN2018,percentileP2018)
percentiliBOD<-c(percentileBOD2015,NA,NA,NA,percentileBOD2016,NA,NA,NA,percentileBOD2017,NA,NA,NA,percentileBOD2018,NA,NA,NA)
percentiliCOD<-c(NA,percentileCOD2015,NA,NA,NA,percentileCOD2016,NA,NA,NA,percentileCOD2017,NA,NA,NA,percentileCOD2018,NA,NA)

# #SST totale
# tsCaricoSSTINtotal<-(na.omit(xts(x=Caprino[,13], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
# mediaCaricoSSTINtotal<-mean(tsCaricoSSTINtotal)
# tsCaricoSSTOUTtotal<-(na.omit(xts(x=Caprino[,14], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
# mediaCaricoSSTOUTtotal<-mean(tsCaricoSSTOUTtotal)
# rendSSTtotal<-(mediaCaricoSSTINtotal - mediaCaricoSSTOUTtotal)/mediaCaricoSSTINtotal*100
# #SST 2015
# tsCaricoSSTIN2015<-tsCaricoSSTINtotal["2015"]
# mediaCaricoSSTIN2015<-mean(tsCaricoSSTIN2015)
# tsCaricoSSTOUT2015<-tsCaricoSSTOUTtotal["2015"]
# mediaCaricoSSTOUT2015<-mean(tsCaricoSSTOUT2015)
# rendSST2015<-(mediaCaricoSSTIN2015 - mediaCaricoSSTOUT2015)/mediaCaricoSSTIN2015*100
# #SST 2016
# tsCaricoSSTIN2016<-tsCaricoSSTINtotal["2016"]
# mediaCaricoSSTIN2016<-mean(tsCaricoSSTIN2016)
# tsCaricoSSTOUT2016<-tsCaricoSSTOUTtotal["2016"]
# mediaCaricoSSTOUT2016<-mean(tsCaricoSSTOUT2016)
# rendSST2016<-(mediaCaricoSSTIN2016 - mediaCaricoSSTOUT2016)/mediaCaricoSSTIN2016*100
# #SST 2017
# tsCaricoSSTIN2017<-tsCaricoSSTINtotal["2017"]
# mediaCaricoSSTIN2017<-mean(tsCaricoSSTIN2017)
# tsCaricoSSTOUT2017<-tsCaricoSSTOUTtotal["2017"]
# mediaCaricoSSTOUT2017<-mean(tsCaricoSSTOUT2017)
# rendSST2017<-(mediaCaricoSSTIN2017 - mediaCaricoSSTOUT2017)/mediaCaricoSSTIN2017*100
# #SST 2018
# tsCaricoSSTIN2018<-tsCaricoSSTINtotal["2018"]
# mediaCaricoSSTIN2018<-mean(tsCaricoSSTIN2018)
# tsCaricoSSTOUT2018<-tsCaricoSSTOUTtotal["2018"]
# mediaCaricoSSTOUT2018<-mean(tsCaricoSSTOUT2018)
# rendSST2018<-(mediaCaricoSSTIN2018 - mediaCaricoSSTOUT2018)/mediaCaricoSSTIN2018*100
# 
# rendimentiSST<-c(rendSST2015,rendSST2016,rendSST2017,rendSST2018)


#NIT totale
tsCaricoTKNINtotal<-(na.omit(xts(x=Caprino[,31], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoTKNINtotal<-mean(tsCaricoTKNINtotal)
tsCaricoNNH4OUTtotal<-(na.omit(xts(x=Caprino[,20], order.by=datestotal))*0.78*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoNNH4OUTtotal<-mean(tsCaricoNNH4OUTtotal)
tsCaricoNASStotal<-0.05*(tsCaricoBODINtotal - tsCaricoBODOUTtotal)
mediaCaricoNASStotal<-mean(tsCaricoNASStotal)
rendNITtotal<-(mediaCaricoTKNINtotal - mediaCaricoNNH4OUTtotal - mediaCaricoNASStotal)/(mediaCaricoTKNINtotal - mediaCaricoNASStotal)*100
#NIT 2015
tsCaricoTKNIN2015<-tsCaricoTKNINtotal["2015"]
mediaCaricoTKNIN2015<-mean(tsCaricoTKNIN2015)
tsCaricoNNH4OUT2015<-tsCaricoNNH4OUTtotal["2015"]
mediaCaricoNNH4OUT2015<-mean(tsCaricoNNH4OUT2015)
tsCaricoNASS2015<-0.05*(tsCaricoBODIN2015 - tsCaricoBODOUT2015)
mediaCaricoNASS2015<-mean(tsCaricoNASS2015)
rendNIT2015<-(mediaCaricoTKNIN2015 - mediaCaricoNNH4OUT2015 - mediaCaricoNASS2015)/(mediaCaricoTKNIN2015 - mediaCaricoNASS2015)*100
#NIT 2016
tsCaricoTKNIN2016<-tsCaricoTKNINtotal["2016"]
mediaCaricoTKNIN2016<-mean(tsCaricoTKNIN2016)
tsCaricoNNH4OUT2016<-tsCaricoNNH4OUTtotal["2016"]
mediaCaricoNNH4OUT2016<-mean(tsCaricoNNH4OUT2016)
tsCaricoNASS2016<-0.05*(tsCaricoBODIN2016 - tsCaricoBODOUT2016)
mediaCaricoNASS2016<-mean(tsCaricoNASS2016)
rendNIT2016<-(mediaCaricoTKNIN2016 - mediaCaricoNNH4OUT2016 - mediaCaricoNASS2016)/(mediaCaricoTKNIN2016 - mediaCaricoNASS2016)*100
#NIT 2017
tsCaricoTKNIN2017<-tsCaricoTKNINtotal["2017"]
mediaCaricoTKNIN2017<-mean(tsCaricoTKNIN2017)
tsCaricoNNH4OUT2017<-tsCaricoNNH4OUTtotal["2017"]
mediaCaricoNNH4OUT2017<-mean(tsCaricoNNH4OUT2017)
tsCaricoNASS2017<-0.05*(tsCaricoBODIN2017 - tsCaricoBODOUT2017)
mediaCaricoNASS2017<-mean(tsCaricoNASS2017)
rendNIT2017<-(mediaCaricoTKNIN2017 - mediaCaricoNNH4OUT2017 - mediaCaricoNASS2017)/(mediaCaricoTKNIN2017 - mediaCaricoNASS2017)*100
#NIT 2018
tsCaricoTKNIN2018<-tsCaricoTKNINtotal["2018"]
mediaCaricoTKNIN2018<-mean(tsCaricoTKNIN2018)
tsCaricoNNH4OUT2018<-tsCaricoNNH4OUTtotal["2018"]
mediaCaricoNNH4OUT2018<-mean(tsCaricoNNH4OUT2018)
tsCaricoNASS2018<-0.05*(tsCaricoBODIN2018 - tsCaricoBODOUT2018)
mediaCaricoNASS2018<-mean(tsCaricoNASS2018)
rendNIT2018<-(mediaCaricoTKNIN2018 - mediaCaricoNNH4OUT2018 - mediaCaricoNASS2018)/(mediaCaricoTKNIN2018 - mediaCaricoNASS2018)*100

rendimentiNIT<-c(rendNIT2015,rendNIT2016,rendNIT2017,rendNIT2018)


#DEN totale
tsCaricoNNO3OUTtotal<-(na.omit(xts(x=Caprino[,24], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoNNO3OUTtotal<-mean(tsCaricoNNO3OUTtotal)
tsCaricoNNO2OUTtotal<-(na.omit(xts(x=Caprino[,22], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
mediaCaricoNNO2OUTtotal<-mean(tsCaricoNNO2OUTtotal)
rendDENtotal<-(mediaCaricoNtotINtotal - mediaCaricoNNH4OUTtotal-mediaCaricoNNO3OUTtotal-mediaCaricoNNO2OUTtotal - mediaCaricoNASStotal)/(mediaCaricoNtotINtotal - mediaCaricoNNH4OUTtotal - mediaCaricoNASStotal)*100
#DENT 2015
tsCaricoNNO3OUT2015<-tsCaricoNNO3OUTtotal["2015"]
mediaCaricoNNO3OUT2015<-mean(tsCaricoNNO3OUT2015)
tsCaricoNNO2OUT2015<-tsCaricoNNO2OUTtotal["2015"]
mediaCaricoNNO2OUT2015<-mean(tsCaricoNNO2OUT2015)
rendDEN2015<-(mediaCaricoNtotIN2015 - mediaCaricoNNH4OUT2015-mediaCaricoNNO3OUT2015-mediaCaricoNNO2OUT2015 - mediaCaricoNASS2015)/(mediaCaricoNtotIN2015 - mediaCaricoNNH4OUT2015 - mediaCaricoNASS2015)*100
#DEN 2016
tsCaricoNNO3OUT2016<-tsCaricoNNO3OUTtotal["2016"]
mediaCaricoNNO3OUT2016<-mean(tsCaricoNNO3OUT2016)
tsCaricoNNO2OUT2016<-tsCaricoNNO2OUTtotal["2016"]
mediaCaricoNNO2OUT2016<-mean(tsCaricoNNO2OUT2016)
rendDEN2016<-(mediaCaricoNtotIN2016 - mediaCaricoNNH4OUT2016-mediaCaricoNNO3OUT2016-mediaCaricoNNO2OUT2016 - mediaCaricoNASS2016)/(mediaCaricoNtotIN2016 - mediaCaricoNNH4OUT2016 - mediaCaricoNASS2016)*100
#DEN 2017
tsCaricoNNO3OUT2017<-tsCaricoNNO3OUTtotal["2017"]
mediaCaricoNNO3OUT2017<-mean(tsCaricoNNO3OUT2017)
tsCaricoNNO2OUT2017<-tsCaricoNNO2OUTtotal["2017"]
mediaCaricoNNO2OUT2017<-mean(tsCaricoNNO2OUT2017)
rendDEN2017<-(mediaCaricoNtotIN2017 - mediaCaricoNNH4OUT2017-mediaCaricoNNO3OUT2017-mediaCaricoNNO2OUT2017 - mediaCaricoNASS2017)/(mediaCaricoNtotIN2017 - mediaCaricoNNH4OUT2017 - mediaCaricoNASS2017)*100
#DEN 2018
tsCaricoNNO3OUT2018<-tsCaricoNNO3OUTtotal["2018"]
mediaCaricoNNO3OUT2018<-mean(tsCaricoNNO3OUT2018)
tsCaricoNNO2OUT2018<-tsCaricoNNO2OUTtotal["2018"]
mediaCaricoNNO2OUT2018<-mean(tsCaricoNNO2OUT2018)
rendDEN2018<-(mediaCaricoNtotIN2018 - mediaCaricoNNH4OUT2018-mediaCaricoNNO3OUT2018-mediaCaricoNNO2OUT2018 - mediaCaricoNASS2018)/(mediaCaricoNtotIN2018 - mediaCaricoNNH4OUT2018 - mediaCaricoNASS2018)*100

rendimentiDEN<-c(rendDEN2015,rendDEN2016,rendDEN2017,rendDEN2018)

df<-setNames(data.frame(matrix(ncol=4,nrow=0)),c(2015,2016,2017,2018))
df[1,]<-rendimentiBOD
df[2,]<-rendimentiCOD
df[3,]<-rendimentiNtot
df[4,]<-rendimentiPtot
df[5,]<-rendimentiNIT
df[6,]<-rendimentiDEN

windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(OutDec = ",")
bp<-barplot(as.matrix(df),beside=T,ylim=c(0,100),col=brewer.pal(6,"Set3"),las=1,yaxt="n",xaxt="n")
grid(nx=NA,ny=5,col="grey")
barplot(as.matrix(df),beside=T,ylim=c(0,100),col=brewer.pal(6,"Set3"),ylab="Rendimento [%]",ad=T,las=1)
abline(h=100,lty=3,col="grey")
abline(h=0)
text(bp,as.matrix(df)/2,as.character(as.matrix(format(round(df,digits=1))),nsmall=1),srt=90)

legend("bottom",ncol=4, c(expression("BOD"[5]),"COD",expression("N"[tot]),expression("P"[tot]),"Nitrificazione","Denitrificazione"),pch=c(22,22,22,22,22,22),pt.bg=brewer.pal(6,"Set3"),pt.cex = 2,bg="white")

windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(3,1,0)) #margini e distanza etichette-asse

rendimentitotal<-rev(c(rendBODtotal,rendCODtotal,rendNtottotal,rendPtottotal,rendNITtotal,rendDENtotal))
bpt<-barplot(rendimentitotal,horiz = T, xlim=c(0,100),space=c(0,0),xlab="Rendimento [%]",ylab="2015 - 2018",col=rev(brewer.pal(6,"Set3")))
text(rendimentitotal/2,bpt,as.character(format(round(rendimentitotal,digits=1)),nsmall=1))
legend("left", c(expression("BOD"[5]),"COD",expression("N"[tot]),expression("P"[tot]),"Nitrificazione","Denitrificazione"),pch=c(22,22,22,22,22,22),pt.bg=brewer.pal(6,"Set3"),pt.cex = 2,bg="white")
abline(v=0)

#AE
df1<-setNames(data.frame(matrix(ncol=4,nrow=0)),c(2015,2016,2017,2018))
df1[1,]<-aeBOD
df1[2,]<-aeCOD
df1[3,]<-aeNtot
df1[4,]<-aePtot

windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

bp1<-barplot(as.matrix(df1),beside=T,ylim=c(0,35000),col=brewer.pal(4,"Set3"),las=1,yaxt="n",xaxt="n")
grid(nx=NA,ny=7,col="grey")
barplot(as.matrix(df1),beside=T,ylim=c(0,35000),col=brewer.pal(4,"Set3"),yaxt="n",ylab=expression(paste("BOD"[5-IN],", COD"[IN],", N"[tot-IN],", P"[tot-IN]," [AE]")),ad=T,las=1)
arrows(bp1, percentiliBOD, bp1, unlist(df1), angle = 90, code = 1, length = 0.05)
arrows(bp1, percentiliCOD, bp1, unlist(df1), angle = 90, code = 1, length = 0.05,col="blue")
axis(side=2,at=seq(from = 0, to = 35000, by = 5000), las=2,format(seq(from = 0,to = 35000,by = 5000), big.mark = ".", decimal.mark = ","))
abline(h=35000,lty=3,col="grey")
abline(h=0)
text(bp1,as.matrix(df1)/2,as.character(as.matrix(format(round(df1),big.mark=".", decimal.mark = ","))),srt=90)
text(bp1,percentiliBOD+2000,as.character(format(round(percentiliBOD),big.mark=".", decimal.mark = ",")),srt=90)
text(bp1,percentiliCOD+2000,as.character(format(round(percentiliCOD),big.mark=".", decimal.mark = ",")),srt=90)

legend(x=9,y=32500,ncol=3, c(expression("BOD"[5-IN]),expression("COD"[IN]),expression("N"[tot-IN]),expression("P"[tot-IN]),"90° percentile","92° percentile"),pch=c(22,22,22,22,NA,NA),col=c(NA,NA,NA,NA,"black","blue"),lty=c(NA,NA,NA,NA,1,1),pt.bg=brewer.pal(4,"Set3"),pt.cex = 2,bg="white")

windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(3,1,0)) #margini e distanza etichette-asse

aetotal<-rev(c(aeBODtotal,aeCODtotal,aeNtottotal,aePtottotal))
bpt<-barplot(aetotal,horiz = T, xlim=c(0,15000),space=c(0,0),xaxt="n",xlab=expression(paste("BOD"[5-IN],", COD"[IN],", N"[tot-IN],", P"[tot-IN]," [AE]")),ylab="2015 - 2018",col=rev(brewer.pal(4,"Set3")))
axis(side=1,at=seq(from = 0, to = 14000, by = 2000), las=1,format(seq(from = 0,to = 14000,by = 2000), big.mark = ".", decimal.mark = ","))
text(aetotal/2,bpt,as.character(format(round(aetotal), big.mark = ".", decimal.mark = ",")))
legend(x=12500,y=3.5, c(expression("BOD"[5-IN]),expression("COD"[IN]),expression("N"[tot-IN]),expression("P"[tot-IN])),pch=c(22,22,22,22),pt.bg=brewer.pal(4,"Set3"),pt.cex = 2,bg="white")
abline(v=0)