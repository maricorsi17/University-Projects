# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

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

plot(as.zoo(tsSSTINRIC1total), xlab="Mesi",ylab=expression(paste("SST"[RIC]," [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,28), type="n")
drawTimeAxis(as.zoo(tsSSTINRIC1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 28, by = 2), las=2)
grid(nx=NA,ny=14,col="grey")
lines(as.zoo((tsSSTINRIC1total)),col="orange")
lines(as.zoo((tsSSTINRIC2total)),col="magenta")
lines(as.zoo((tsSSTINRIC3total)),col="red")

abline(v=index(tsSSTINRIC2016[1,]),lwd=2)
abline(v=index(tsSSTINRIC2017[1,]),lwd=2)
abline(v=index(tsSSTINRIC2018[1,]),lwd=2)
text(x=index(tsSSTINRIC2015[182,]),y=27,label="2015")
text(x=index(tsSSTINRIC2016[182,]),y=27,label="2016")
text(x=index(tsSSTINRIC2017[182,]),y=27,label="2017")
text(x=index(tsSSTINRIC2018[90,]),y=27,label="2018")
legend(x=as.Date("2015-02-14"),y=25, c(expression("SST"[RIC1]),expression("SST"[RIC2]),expression("SST"[RIC3])),col=c("orange","magenta","red"),lty=c(1,1,1),lwd=c(1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(3,6,4,4),mgp=c(4,1,0))

boxplot(cbind(coredata(xts(x=SAmbrogio[,42], order.by=datestotal)),coredata(xts(x=SAmbrogio[,46], order.by=datestotal)),coredata(xts(x=SAmbrogio[,50], order.by=datestotal))),yaxt="n",ylab="[g/L]",names=c(expression("SST"[RIC1]),expression("SST"[RIC2]),expression("SST"[RIC3])),main=expression(paste("SST"[RIC])), ylim=c(0,30),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 30,by = 5),las=2,labels = format(seq(from = 0,to = 30,by = 5), big.mark = ".", decimal.mark = ","))

