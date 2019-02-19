# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsPtotINtotal<-(xts(x=Caprino[,27], order.by=datestotal))
tsPtotINtotal[which(tsPtotINtotal>30)]<-NA
tsPtotINtotalNA<-tsPtotINtotal
tsPtotINtotal<-na.omit(tsPtotINtotalNA)
tsCaricoPtotINtotal<-(tsPtotINtotal*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoPtotINtotal<-na.omit(rollapply(tsPtotINtotalNA*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsCaricoPtotINtotal_spazi<-na.approx(xts(x=Caprino[,27], order.by=datestotal))






## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsCaricoPtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsCaricoPtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsCaricoPtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsCaricoPtotINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,50),col="grey")
drawTimeAxis(as.zoo(tsCaricoPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 50,by = 10),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsCaricoPtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoPtotINtotal[index(tsCaricoPtotINtotal)]))

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
text(x=index(tsPtotIN2015[182,]),y=47.5,label="2015")
text(x=index(tsPtotIN2016[182,]),y=47.5,label="2016")
text(x=index(tsPtotIN2017[182,]),y=47.5,label="2017")
text(x=index(tsPtotIN2018[90,]),y=47.5,label="2018")
legend(x=index(tsPtotIN2017[220,]),y=44.5, c(expression(paste("P"[tot-IN])),expression(paste("MA P"[tot-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoPtotINtotal),yaxt="n",ylab="[kg/d]",main=expression(paste("P"[tot-IN])), ylim=c(0,50),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 50,by = 10),las=2,labels = format(seq(from = 0,to = 50,by = 10), big.mark = ".", decimal.mark = ","))
