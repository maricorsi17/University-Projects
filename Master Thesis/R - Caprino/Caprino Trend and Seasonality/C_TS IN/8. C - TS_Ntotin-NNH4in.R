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
tsNtotINtotal_original<-na.approx(xts(x=Caprino[,29], order.by=datestotal))
tsNtotINtotal<-na.omit(xts(x=Caprino[,29], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=Caprino[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNH4INtotal_original<-xts(x=Caprino[,41], order.by=datestotal)
tsNH4INtotal_original[which(tsNH4INtotal_original==0)]=NA
tsNH4INtotal<-na.omit(tsNH4INtotal_original)
MAtsNH4INtotal<-na.omit(rollapply(tsNH4INtotal_original, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsNtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsNtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsNtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsNtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot-IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,140),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 140,by = 20),las=2)
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tsNtotINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNtotINtotal)),lwd=2)
a<-lm(tsNtotINtotal~index(tsNtotINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=130,label="2015")
text(x=index(tsNtotIN2016[182,]),y=130,label="2016")
text(x=index(tsNtotIN2017[182,]),y=130,label="2017")
text(x=index(tsNtotIN2018[90,]),y=130,label="2018")
text(x=index(tsNtotIN2016[181,]),y=10, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[120,]),y=125, c(expression(paste("N"[tot-IN])), "Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NH"[4-IN]^"+"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,140),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 140,by = 20),las=2)
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tsNH4INtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNH4INtotal)),lwd=2)
a1<-lm(tsNH4INtotal~index(tsNH4INtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=130,label="2015")
text(x=index(tsNtotIN2016[182,]),y=130,label="2016")
text(x=index(tsNtotIN2017[182,]),y=130,label="2017")
text(x=index(tsNtotIN2018[90,]),y=130,label="2018")
text(x=index(tsNtotIN2016[181,]),y=10, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[120,]),y=125, c(expression(paste("N-NH"[4-IN]^"+")),"Reggressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsNtotINtotalDET<-tsNtotINtotal-(ts_trend(tsNtotINtotal))
mediaNtotINDET<-mean(tsNtotINtotalDET)

tsNH4INtotalDET<-tsNH4INtotal-(ts_trend(tsNH4INtotal))
mediaNH4inDET<-mean(tsNH4INtotalDET)

# STAGIONALITA'
tsNtotINtotalAGG<-apply.weekly(tsNtotINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNtotINtotalAGG,lag.max = 60, main=expression("N"[tot-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsNH4INtotalAGG<-apply.weekly(tsNH4INtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNH4INtotalAGG,lag.max = 60, main=expression(paste("N-NH"[4-IN]^"+")),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)