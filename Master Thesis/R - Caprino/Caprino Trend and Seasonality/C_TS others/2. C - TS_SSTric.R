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

tsSSTINRICtotal<-na.approx(xts(x=Caprino[,40], order.by=datestotal))

## Creare un oggetto con le date (2015)
tsSSTINRIC2015<-tsSSTINRICtotal["2015"]
## Creare un oggetto con le date (2016)
tsSSTINRIC2016<-tsSSTINRICtotal["2016"]
## Creare un oggetto con le date (2017)
tsSSTINRIC2017<-tsSSTINRICtotal["2017"]
## Creare un oggetto con le date (2018)
tsSSTINRIC2018<-tsSSTINRICtotal["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINRICtotal), xlab="Mesi",ylab=expression(paste("SST"[RIC], " [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(4,16), type="n")
drawTimeAxis(as.zoo(tsSSTINRICtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 4, to = 16, by = 2), las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo((tsSSTINRICtotal)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINRICtotal)),lwd=2)
a<-lm(tsSSTINRICtotal~index(tsSSTINRICtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=15.5,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=15.5,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=15.5,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=15.5,label="2018")
text(x=index(tsSSTINRIC2016[181,]),y=5.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSTINRIC2017[100,]),y=14.6, c(expression("SST"[RIC]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsSSTINRICtotalDET<-tsSSTINRICtotal-(ts_trend(tsSSTINRICtotal))
mediaDET<-mean(tsSSTINRICtotalDET)

# STAGIONALITA'
tsSSTINRICtotalAGG<-apply.weekly(tsSSTINRICtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINRICtotalAGG,lag.max = 60, main=expression("SST"[RIC]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)