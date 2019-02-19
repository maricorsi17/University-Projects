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


tsNO3INtotal<-na.omit(xts(x=Caprino[,23], order.by=datestotal))
MAtsNO3INtotal<-na.omit(rollapply(xts(x=Caprino[,23], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNO2INtotal<-na.omit(xts(x=Caprino[,21], order.by=datestotal))
MAtsNO2INtotal<-na.omit(rollapply(xts(x=Caprino[,21], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



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

plot(as.zoo(tsNO3INtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NO"[3-IN]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,2.5),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 2.5,by = 0.5),las=2,format(seq(from = 0,to = 2.5,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsNO3INtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNO3INtotal)),lwd=2)
a<-lm(tsNO3INtotal~index(tsNO3INtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=2.4,label="2015")
text(x=index(tsNtotIN2016[182,]),y=2.4,label="2016")
text(x=index(tsNtotIN2017[182,]),y=2.4,label="2017")
text(x=index(tsNtotIN2018[90,]),y=2.4,label="2018")
text(x=index(tsNtotIN2016[181,]),y=1.25, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2015[30,]),y=2.3, c(expression(paste("N-NO"[3-IN]^"-")),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsNO3INtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NO"[2-IN]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,1),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 1,by = 0.2),las=2,format(seq(from = 0,to = 1,by = 0.2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsNO2INtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNO2INtotal)),lwd=2)
a1<-lm(tsNO2INtotal~index(tsNO2INtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=0.9,label="2015")
text(x=index(tsNtotIN2016[182,]),y=0.9,label="2016")
text(x=index(tsNtotIN2017[182,]),y=0.9,label="2017")
text(x=index(tsNtotIN2018[90,]),y=0.9,label="2018")
text(x=index(tsNtotIN2016[181,]),y=0.7, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2015[30,]),y=0.85, c(expression(paste("N-NO"[2-IN]^"-")),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



# DETRENDING
tsNO2INtotalDET<-tsNO2INtotal-(ts_trend(tsNO2INtotal))
mediaNO2inDET<-mean(tsNO2INtotalDET)

# DETRENDING
tsNO3INtotalDET<-tsNO3INtotal-(ts_trend(tsNO3INtotal))
mediaNO3inDET<-mean(tsNO3INtotalDET)

# STAGIONALITA'
tsNO2INtotalAGG<-apply.weekly(tsNO2INtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNO2INtotalAGG,lag.max = 60, main=expression("N-NO"[2-IN]^"-"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsNO3INtotalAGG<-apply.weekly(tsNO3INtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNO3INtotalAGG,lag.max = 60, main=expression("N-NO"[3-IN]^"-"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)