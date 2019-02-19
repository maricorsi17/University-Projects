# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("tsbox")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/NoOutliers_Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsPtotINtotal<-(xts(x=Caprino[,27], order.by=datestotal))
tsPtotINtotalNA<-tsPtotINtotal
tsPtotINtotal<-na.omit(tsPtotINtotalNA)
tsCaricoPtotINtotal<-(tsPtotINtotal*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoPtotINtotal<-na.omit(rollapply(tsPtotINtotalNA*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsCaricoPtotINtotal_spazi<-na.approx(xts(x=Caprino[,27], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsCaricoPtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsCaricoPtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsCaricoPtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsCaricoPtotINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,50),col="grey")
drawTimeAxis(as.zoo(tsCaricoPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 50,by = 10),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsCaricoPtotINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCaricoPtotINtotal)),lwd=2)
a<-lm(tsCaricoPtotINtotal~index(tsCaricoPtotINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
text(x=index(tsPtotIN2015[182,]),y=47.5,label="2015")
text(x=index(tsPtotIN2016[182,]),y=47.5,label="2016")
text(x=index(tsPtotIN2017[182,]),y=47.5,label="2017")
text(x=index(tsPtotIN2018[90,]),y=47.5,label="2018")
text(x=index(tsPtotIN2016[181,]),y=5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsPtotIN2017[110,]),y=44.5, c(expression(paste("P"[tot-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsCaricoPtotINtotalDET<-tsCaricoPtotINtotal-(ts_trend(tsCaricoPtotINtotal))
mediaDET<-mean(tsCaricoPtotINtotalDET)

# STAGIONALITA'
tsCaricoPtotINtotalAGG<-apply.weekly(tsCaricoPtotINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCaricoPtotINtotalAGG,lag.max = 60, main=expression("Carico P"[tot-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)