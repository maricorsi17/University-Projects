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
tsNtotINtotal_original<-xts(x=SAmbrogio[,31], order.by=datestotal)
tsBODINtotal_original<-xts(x=SAmbrogio[,17], order.by=datestotal)
tsCODINtotal_original<-xts(x=SAmbrogio[,19], order.by=datestotal)

tsBODINtotal_spazi<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))


tsBODINtotal<-tsBODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsNtotINtotal_BOD<-tsNtotINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsCODINtotal<-tsCODINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsNtotINtotal_COD<-tsNtotINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]

rapportoN_BOD<-tsNtotINtotal_BOD/tsBODINtotal
rapportoN_BOD_NA<-tsNtotINtotal_original/tsBODINtotal_original
MArapportoN_BOD<-na.omit(rollapply(rapportoN_BOD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaN_BOD<-mean(rapportoN_BOD)

rapportoN_COD<-tsNtotINtotal_COD/tsCODINtotal
rapportoN_COD_NA<-tsNtotINtotal_original/tsCODINtotal_original
MArapportoN_COD<-na.omit(rollapply(rapportoN_COD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaN_COD<-mean(rapportoN_COD)


## Creare un oggetto con le date (2015)
tsBODIN2015<-tsBODINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsBODINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsBODINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsBODINtotal_spazi["2018"]



#RAPPORTO N_BOD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoN_BOD),type="n",xlab="Mesi",ylab=expression(paste("N"[tot-IN],"/BOD"[5-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.8),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.1),las=2,format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
par(new=T)
lines(as.zoo(rapportoN_BOD),col="darkslategrey")
lines(as.zoo(ts_trend(rapportoN_BOD)),lwd=2)
a<-lm(rapportoN_BOD~index(rapportoN_BOD))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=0.75,label="2015")
text(x=index(tsBODIN2016[182,]),y=0.75,label="2016")
text(x=index(tsBODIN2017[182,]),y=0.75,label="2017")
text(x=index(tsBODIN2018[90,]),y=0.75,label="2018")
text(x=index(tsBODIN2016[181,]),y=0.05, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2015[30,]),y=0.68, c(expression(paste("N"[tot-IN],"/BOD"[5-IN])),"Regressione","LOESS"), col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




#RAPPORTO N_COD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoN_COD),type="n",xlab="Mesi",ylab=expression(paste("N"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.4),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.1),las=2,format(seq(from = 0,to = 0.4,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
par(new=T)
lines(as.zoo(rapportoN_COD),col="darkslategrey")
lines(as.zoo(ts_trend(rapportoN_COD)),lwd=2)
a1<-lm(rapportoN_COD~index(rapportoN_COD))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=0.375,label="2015")
text(x=index(tsBODIN2016[182,]),y=0.375,label="2016")
text(x=index(tsBODIN2017[182,]),y=0.375,label="2017")
text(x=index(tsBODIN2018[90,]),y=0.375,label="2018")
text(x=index(tsBODIN2016[181,]),y=0.05, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2015[30,]),y=0.35, c(expression(paste("N"[tot-IN],"/COD"[IN])),"Regressione","LOESS"), col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
rapportoN_BODDET<-rapportoN_BOD-(ts_trend(rapportoN_BOD))
mediarappN_BODDET<-mean(rapportoN_BODDET)

rapportoN_CODDET<-rapportoN_COD-(ts_trend(rapportoN_COD))
mediarapp_NCODDET<-mean(rapportoN_CODDET)

# STAGIONALITA'
rapportoN_BODAGG<-apply.weekly(rapportoN_BODDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoN_BODAGG,lag.max = 60, main=expression(paste("N"[tot-IN],"/BOD"[5-IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

rapportoN_CODAGG<-apply.weekly(rapportoN_CODDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoN_CODAGG,lag.max = 60, main=expression(paste("N"[tot-IN],"/COD"[IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)