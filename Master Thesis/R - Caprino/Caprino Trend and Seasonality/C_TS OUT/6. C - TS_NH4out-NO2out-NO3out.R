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
tsNtotINtotal<-na.omit(xts(x=Caprino[,29], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=Caprino[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNH4OUTtotal_original<-na.approx(xts(x=Caprino[,20], order.by=datestotal))
tsNH4OUTtotal<-na.omit(xts(x=Caprino[,20], order.by=datestotal))*0.78
MAtsNH4OUTtotal<-na.omit(rollapply(xts(x=Caprino[,20], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNO2OUTtotal<-na.omit(xts(x=Caprino[,22], order.by=datestotal))
MAtsNO2OUTtotal<-na.omit(rollapply(xts(x=Caprino[,22], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNO3OUTtotal<-na.omit(xts(x=Caprino[,24], order.by=datestotal))
MAtsNO3OUTtotal<-na.omit(rollapply(xts(x=Caprino[,24], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsNH4OUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsNH4OUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsNH4OUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsNH4OUTtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsNH4OUTtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NH"[4-OUT]^"+"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,10),col="orange")
drawTimeAxis(as.zoo(tsNH4OUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 10,by = 2.5),las=2,format(seq(from = 0,to = 10,by = 2.5),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsNH4OUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNH4OUTtotal)),lwd=2)
a<-lm(tsNH4OUTtotal~index(tsNH4OUTtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100



abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
#abline(h=15*0.78,col="red",lty=2)
text(x=index(tsNtotIN2015[182,]),y=9.5,label="2015")
text(x=index(tsNtotIN2016[182,]),y=9.5,label="2016")
text(x=index(tsNtotIN2017[182,]),y=9.5,label="2017")
text(x=index(tsNtotIN2018[90,]),y=9.5,label="2018")
text(x=index(tsNtotIN2016[181,]),y=1.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[200,]),y=8.5, c(expression(paste("N-NH"[4-OUT]^"+")), "Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsNH4OUTtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NO"[2-OUT]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.75),col="orange")
drawTimeAxis(as.zoo(tsNH4OUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.75,by = 0.25),las=2,format(seq(from = 0,to = 0.75,by = 0.25),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=3,col="grey")
lines(as.zoo(tsNO2OUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNO2OUTtotal)),lwd=2)
a1<-lm(tsNO2OUTtotal~index(tsNO2OUTtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
#abline(h=0.6,col="darkgreen",lty=2)
text(x=index(tsNtotIN2015[182,]),y=0.7,label="2015")
text(x=index(tsNtotIN2016[182,]),y=0.7,label="2016")
text(x=index(tsNtotIN2017[182,]),y=0.7,label="2017")
text(x=index(tsNtotIN2018[90,]),y=0.7,label="2018")
text(x=index(tsNtotIN2016[181,]),y=0.130, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[200,]),y=0.65, c(expression(paste("N-NO"[2-OUT]^"-")),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsNH4OUTtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NO"[3-OUT]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,25),col="orange")
drawTimeAxis(as.zoo(tsNH4OUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsNO3OUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsNO3OUTtotal)),lwd=2)
a2<-lm(tsNO3OUTtotal~index(tsNO3OUTtotal))
abline(a2,col="red",lwd=2,lty=5)

perc_a2<--(1-coredata(a2$fitted.values[length(a2$fitted.values)])/coredata(a2$fitted.values[1]))*100


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
#abline(h=20,col="purple",lty=2)
text(x=index(tsNtotIN2015[182,]),y=22.5,label="2015")
text(x=index(tsNtotIN2016[182,]),y=22.5,label="2016")
text(x=index(tsNtotIN2017[182,]),y=22.5,label="2017")
text(x=index(tsNtotIN2018[90,]),y=22.5,label="2018")
text(x=index(tsNtotIN2016[181,]),y=12.5, label=paste("Variazione = ",format(round(perc_a2,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2017[200,]),y=21, c(expression(paste("N-NO"[3-OUT]^"-")),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsNH4OUTtotalDET<-tsNH4OUTtotal-(ts_trend(tsNH4OUTtotal))
mediaNH4outDET<-mean(tsNH4OUTtotalDET)

# DETRENDING
tsNO2OUTtotalDET<-tsNO2OUTtotal-(ts_trend(tsNO2OUTtotal))
mediaNO2outDET<-mean(tsNO2OUTtotalDET)

# DETRENDING
tsNO3OUTtotalDET<-tsNO3OUTtotal-(ts_trend(tsNO3OUTtotal))
mediaNO3outDET<-mean(tsNO3OUTtotalDET)


# STAGIONALITA'
tsNH4OUTtotalAGG<-apply.weekly(tsNH4OUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNH4OUTtotalAGG,lag.max = 60, main=expression("N-NH"[4-OUT]^"+"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsNO2OUTtotalAGG<-apply.weekly(tsNO2OUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNO2OUTtotalAGG,lag.max = 60, main=expression("N-NO"[2-OUT]^"-"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsNO3OUTtotalAGG<-apply.weekly(tsNO3OUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNO3OUTtotalAGG,lag.max = 60, main=expression("N-NO"[3-OUT]^"-"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)