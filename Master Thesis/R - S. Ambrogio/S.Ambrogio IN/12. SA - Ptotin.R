# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsPtotINtotal_original<-na.approx(xts(x=SAmbrogio[,29], order.by=datestotal))
tsPtotINtotal<-na.omit(xts(x=SAmbrogio[,29], order.by=datestotal))
MAtsPtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsPtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsPtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsPtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsPtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot-IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,40),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 40,by = 5),las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsPtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsPtotINtotal[index(tsPtotINtotal)]))

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
text(x=index(tsPtotIN2015[182,]),y=37.5,label="2015")
text(x=index(tsPtotIN2016[182,]),y=37.5,label="2016")
text(x=index(tsPtotIN2017[182,]),y=37.5,label="2017")
text(x=index(tsPtotIN2018[90,]),y=37.5,label="2018")
legend(x=index(tsPtotIN2015[30]),y=35, c(expression(paste("P"[tot-IN])),expression(paste("MA P"[tot-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsPtotINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("P"[tot-IN])), ylim=c(0,40),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 40,by = 5),las=2,labels = format(seq(from = 0,to = 40,by = 5), big.mark = ".", decimal.mark = ","))
