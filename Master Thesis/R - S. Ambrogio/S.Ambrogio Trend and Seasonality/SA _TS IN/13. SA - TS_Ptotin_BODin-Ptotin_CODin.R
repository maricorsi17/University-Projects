# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")
library("tsbox")


# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsPtotINtotal_original<-xts(x=SAmbrogio[,29], order.by=datestotal)
tsBODINtotal_original<-xts(x=SAmbrogio[,17], order.by=datestotal)
tsCODINtotal_original<-xts(x=SAmbrogio[,19], order.by=datestotal)

tsPtotINtotal_spazi<-na.approx(xts(x=SAmbrogio[,29], order.by=datestotal))


tsBODINtotal<-tsBODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsPtotINtotal_BOD<-tsPtotINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsCODINtotal<-tsCODINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsPtotINtotal_COD<-tsPtotINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]

rapportoP_BOD<-tsPtotINtotal_BOD/tsBODINtotal
rapportoP_BOD_NA<-tsPtotINtotal_original/tsBODINtotal_original
MArapportoP_BOD<-na.omit(rollapply(rapportoP_BOD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaP_BOD<-mean(rapportoP_BOD)

rapportoP_COD<-tsPtotINtotal_COD/tsCODINtotal
rapportoP_COD_NA<-tsPtotINtotal_original/tsCODINtotal_original
MArapportoP_COD<-na.omit(rollapply(rapportoP_COD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaP_COD<-mean(rapportoP_COD)


## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsPtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsPtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsPtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsPtotINtotal_spazi["2018"]



# RAPPORTO P_BOD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_BOD),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/BOD"[5-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.08),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.08,by = 0.01),las=2,format(seq(from=0,to=0.08,by=0.01),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(rapportoP_BOD),col="darkslategrey")
lines(as.zoo(ts_trend(rapportoP_BOD)),lwd=2)
a<-lm(rapportoP_BOD~index(rapportoP_BOD))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)

text(x=index(tsPtotIN2015[182,]),y=0.075,label="2015")
text(x=index(tsPtotIN2016[182,]),y=0.075,label="2016")
text(x=index(tsPtotIN2017[182,]),y=0.075,label="2017")
text(x=index(tsPtotIN2018[90,]),y=0.075,label="2018")
text(x=index(tsPtotIN2016[181,]),y=0.005, label=paste("Variazione = ",format(round(perc_a,digits=1),nsmall=1,decimal.mark = ","),"%"),col="red")
legend(x=index(tsPtotIN2015[30,]),y=0.072, c(expression(paste("P"[tot-IN],"/BOD"[5-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




# RAPPORTO P_COD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_COD),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.04),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.04,by = 0.01),las=2,format(seq(from = 0,to = 0.04,by = 0.01), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(rapportoP_COD),col="darkslategrey")
lines(as.zoo(ts_trend(rapportoP_COD)),lwd=2)
a1<-lm(rapportoP_COD~index(rapportoP_COD))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)

text(x=index(tsPtotIN2015[182,]),y=0.0375,label="2015")
text(x=index(tsPtotIN2016[182,]),y=0.0375,label="2016")
text(x=index(tsPtotIN2017[182,]),y=0.0375,label="2017")
text(x=index(tsPtotIN2018[90,]),y=0.0375,label="2018")
text(x=index(tsPtotIN2016[181,]),y=0.005, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsPtotIN2015[40,]),y=0.035, c(expression(paste("P"[tot-IN],"/COD"[IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
rapportoP_BODDET<-rapportoP_BOD-(ts_trend(rapportoP_BOD))
mediarapp_PBODDET<-mean(rapportoP_BODDET)

rapportoP_CODDET<-rapportoP_COD-(ts_trend(rapportoP_COD))
mediarapp_PCODDET<-mean(rapportoP_CODDET)

# STAGIONALITA'
rapportoP_BODAGG<-apply.weekly(rapportoP_BODDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoP_BODAGG,lag.max = 60, main=expression(paste("P"[tot-IN],"/BOD"[5-IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

rapportoP_CODAGG<-apply.weekly(rapportoP_CODDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoP_CODAGG,lag.max = 60, main=expression(paste("P"[tot-IN],"/COD"[IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)
