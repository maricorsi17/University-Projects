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

tsSSTINRIC1total<-na.approx(xts(x=SAmbrogio[,42], order.by=datestotal))
tsSSTINRIC2total<-na.approx(xts(x=SAmbrogio[,46], order.by=datestotal))
tsSSTINRIC3total<-na.approx(xts(x=SAmbrogio[,50], order.by=datestotal))

## Creare un oggetto con le date (2015)
tsSSTINRIC2015<-tsSSTINRIC1total["2015"]
## Creare un oggetto con le date (2016)
tsSSTINRIC2016<-tsSSTINRIC1total["2016"]
## Creare un oggetto con le date (2017)
tsSSTINRIC2017<-tsSSTINRIC1total["2017"]
## Creare un oggetto con le date (2018)
tsSSTINRIC2018<-tsSSTINRIC1total["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINRIC1total), xlab="Mesi",ylab=expression(paste("SST"[RIC1]," [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,16), type="n")
drawTimeAxis(as.zoo(tsSSTINRIC1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 16, by = 2), las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo((tsSSTINRIC1total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINRIC1total)),lwd=2)
a<-lm(tsSSTINRIC1total~index(tsSSTINRIC1total))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=15,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=15,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=15,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=15,label="2018")
text(x=index(tsSSTINRIC2016[181,]),y=1, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=as.Date("2015-02-14"),y=13.5, c(expression("SST"[RIC1]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINRIC1total), xlab="Mesi",ylab=expression(paste("SST"[RIC2]," [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,16), type="n")
drawTimeAxis(as.zoo(tsSSTINRIC1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 16, by = 2), las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo((tsSSTINRIC2total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINRIC2total)),lwd=2)
a1<-lm(tsSSTINRIC2total~index(tsSSTINRIC2total))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100

abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=15,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=15,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=15,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=15,label="2018")
text(x=index(tsSSTINRIC2016[181,]),y=1, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=as.Date("2015-02-14"),y=13.5, c(expression("SST"[RIC2]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINRIC1total), xlab="Mesi",ylab=expression(paste("SST"[RIC3]," [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,16), type="n")
drawTimeAxis(as.zoo(tsSSTINRIC1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 16, by = 2), las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo((tsSSTINRIC3total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINRIC3total)),lwd=2)
a2<-lm(tsSSTINRIC3total~index(tsSSTINRIC3total))
abline(a2,col="red",lwd=2,lty=5)

perc_a2<--(1-coredata(a2$fitted.values[length(a2$fitted.values)])/coredata(a2$fitted.values[1]))*100

abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=15,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=15,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=15,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=15,label="2018")
text(x=index(tsSSTINRIC2016[181,]),y=2.5, label=paste("Variazione = ",format(round(perc_a2,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=as.Date("2015-02-14"),y=13.5, c(expression("SST"[RIC3]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsSSTINRIC1totalDET<-tsSSTINRIC1total-(ts_trend(tsSSTINRIC1total))
mediaSSTRIC1DET<-mean(tsSSTINRIC1totalDET)

# DETRENDING
tsSSTINRIC2totalDET<-tsSSTINRIC2total-(ts_trend(tsSSTINRIC2total))
mediaSSTRIC2DET<-mean(tsSSTINRIC2totalDET)

# DETRENDING
tsSSTINRIC3totalDET<-tsSSTINRIC3total-(ts_trend(tsSSTINRIC3total))
mediaSSTRIC3DET<-mean(tsSSTINRIC3totalDET)

# STAGIONALITA'
tsSSTINRIC1totalAGG<-apply.weekly(tsSSTINRIC1totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINRIC1totalAGG,lag.max = 60, main=expression("SST"[RIC1]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSTINRIC2totalAGG<-apply.weekly(tsSSTINRIC2totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINRIC2totalAGG,lag.max = 60, main=expression("SST"[RIC2]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSTINRIC3totalAGG<-apply.weekly(tsSSTINRIC3totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINRIC3totalAGG,lag.max = 60, main=expression("SST"[RIC3]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)