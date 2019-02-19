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
tsCaricoPtotINtotal_original<-na.approx(xts(x=SAmbrogio[,29], order.by=datestotal))

tsCaricoPtotINtotal<-(na.omit(xts(x=SAmbrogio[,29], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoPtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,29], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsCaricoPtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsCaricoPtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsCaricoPtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsCaricoPtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoPtotINtotal),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,120),col="grey")
drawTimeAxis(as.zoo(tsCaricoPtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 120,by = 20),las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsCaricoPtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoPtotINtotal[index(tsCaricoPtotINtotal)]))

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
text(x=index(tsPtotIN2015[182,]),y=110,label="2015")
text(x=index(tsPtotIN2016[182,]),y=110,label="2016")
text(x=index(tsPtotIN2017[182,]),y=110,label="2017")
text(x=index(tsPtotIN2018[91,]),y=110,label="2018")
legend(x=index(tsPtotIN2017[50,]),y=97, c(expression(paste("P"[tot-IN])),expression(paste("MA P"[tot-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoPtotINtotal[which(.indexyear(tsCaricoPtotINtotal)>(2015-1900))]),yaxt="n",ylab="[kg/d]",main=expression(paste("P"[tot-IN])), ylim=c(0,120),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 120,by = 20),las=2,labels = format(seq(from = 0,to = 120,by = 20), big.mark = ".", decimal.mark = ","))
