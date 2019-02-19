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
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/NoOutliers_Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsCaricoNtotINtotal_spazi<-na.approx(xts(x=Caprino[,29], order.by=datestotal))


tsCaricoNtotINtotal<-(na.omit(xts(x=Caprino[,29], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoNtotINtotal<-na.omit(rollapply(xts(x=Caprino[,29], order.by=datestotal)*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsCaricoNtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsCaricoNtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsCaricoNtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsCaricoNtotINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsCaricoNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,250),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsCaricoNtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 250,by = 50),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsCaricoNtotINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCaricoNtotINtotal)),lwd=2)
a<-lm(tsCaricoNtotINtotal~index(tsCaricoNtotINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=230,label="2015")
text(x=index(tsNtotIN2016[182,]),y=230,label="2016")
text(x=index(tsNtotIN2017[182,]),y=230,label="2017")
text(x=index(tsNtotIN2018[90,]),y=230,label="2018")
text(x=index(tsNtotIN2016[181,]),y=25, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[110,]),y=215, c(expression(paste("N"[tot-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsCaricoNtotINtotalDET<-tsCaricoNtotINtotal-(ts_trend(tsCaricoNtotINtotal))
mediaDET<-mean(tsCaricoNtotINtotalDET)

# STAGIONALITA'
tsCaricoNtotINtotalAGG<-apply.weekly(tsCaricoNtotINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCaricoNtotINtotalAGG,lag.max = 60, main=expression("Carico N"[tot-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)