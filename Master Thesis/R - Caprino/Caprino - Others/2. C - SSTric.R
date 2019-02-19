# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

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
axis(side=2,at=seq(from = 4, to = 16, by = 1), las=2)
grid(nx=NA,ny=12,col="grey")
lines(as.zoo((tsSSTINRICtotal)))

abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=15.5,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=15.5,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=15.5,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=15.5,label="2018")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSSTINRICtotal),yaxt="n",ylab="[g/L]",main=expression(paste("SST"[RIC])), ylim=c(3,15),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 3,to = 15,by = 1),las=2,labels = format(seq(from = 3,to = 15,by = 1), big.mark = ".", decimal.mark = ","))
