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
tsNtotINtotal_original<-na.approx(xts(x=SAmbrogio[,31], order.by=datestotal))
tsNtotINtotal<-na.omit(xts(x=SAmbrogio[,31], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,31], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNH4INtotal_original<-xts(x=SAmbrogio[,51], order.by=datestotal)
tsNH4INtotal_original[which(tsNH4INtotal_original==0)]=NA
tsNH4INtotal<-na.omit(tsNH4INtotal_original)
MAtsNH4INtotal<-na.omit(rollapply(tsNH4INtotal_original, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNO3INtotal<-na.omit(xts(x=SAmbrogio[,25], order.by=datestotal))
MAtsNO3INtotal<-na.omit(rollapply(xts(x=SAmbrogio[,25], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNO2INtotal<-na.omit(xts(x=SAmbrogio[,23], order.by=datestotal))
MAtsNO2INtotal<-na.omit(rollapply(xts(x=SAmbrogio[,23], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



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

plot(as.zoo(tsNO3INtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NO"[3-IN]^"-",", N-NO"[2-IN]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,2),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 2,by = 0.5),las=2,format(seq(from = 0,to = 2,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")

lines(as.zoo(tsNO3INtotal),col="darkslategrey")
lines(as.zoo(MAtsNO3INtotal[index(tsNO3INtotal)]))
lines(as.zoo(tsNO2INtotal),col="orange")
lines(as.zoo(MAtsNO2INtotal[index(tsNO2INtotal)]),col="red")


abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=1.9,label="2015")
text(x=index(tsNtotIN2016[182,]),y=1.9,label="2016")
text(x=index(tsNtotIN2017[182,]),y=1.9,label="2017")
text(x=index(tsNtotIN2018[90,]),y=1.9,label="2018")
legend(x=index(tsNtotIN2017[240,]),y=1.8, c(expression(paste("N-NO"[3-IN]^"-")),expression(paste("N-NO"[2-IN]^"-")),expression(paste("MA N-NO"[3-IN]^"-", " (mensile)")),expression(paste("MA N-NO"[2-IN]^"-"," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")


# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNO2INtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NO"[2-IN]^"-")), ylim=c(0,0.8),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.2),las=2,labels = format(seq(from = 0,to = 0.8,by = 0.2), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNO3INtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NO"[3-IN]^"-")), ylim=c(0,2),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 2,by = 0.5),las=2,labels = format(seq(from = 0,to = 2,by = 0.5), big.mark = ".", decimal.mark = ","))
