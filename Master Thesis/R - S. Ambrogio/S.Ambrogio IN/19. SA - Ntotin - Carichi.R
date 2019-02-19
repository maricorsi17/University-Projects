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
## Creare la time series (tutti gli anni)
tsCaricoNtotINtotal_original<-na.approx(xts(x=SAmbrogio[,31], order.by=datestotal))

tsCaricoNtotINtotal<-(na.omit(xts(x=SAmbrogio[,31], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoNtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,31], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsCaricoNtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsCaricoNtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsCaricoNtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsCaricoNtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsCaricoNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,600),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsCaricoNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 600,by = 100),las=2,format(seq(from = 0,to = 600,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsCaricoNtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoNtotINtotal[index(tsCaricoNtotINtotal)]))

abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=560,label="2015")
text(x=index(tsNtotIN2016[182,]),y=560,label="2016")
text(x=index(tsNtotIN2017[182,]),y=560,label="2017")
text(x=index(tsNtotIN2018[91,]),y=560,label="2018")
legend(x=index(tsNtotIN2017[50,]),y=510, c(expression(paste("N"[tot-IN])), expression(paste("MA N"[tot-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoNtotINtotal[which(.indexyear(tsCaricoNtotINtotal)>(2015-1900))]),yaxt="n",ylab="[kg/d]",main=expression(paste("N"[tot-IN])), ylim=c(0,600),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 600,by = 100),las=2,labels = format(seq(from = 0,to = 600,by = 100), big.mark = ".", decimal.mark = ","))
