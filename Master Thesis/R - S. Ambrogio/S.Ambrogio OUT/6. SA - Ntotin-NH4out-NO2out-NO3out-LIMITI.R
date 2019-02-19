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
tsNtotINtotal<-na.omit(xts(x=SAmbrogio[,31], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,31], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



tsNH4OUTtotal_original<-na.approx(xts(x=SAmbrogio[,22], order.by=datestotal))
tsNH4OUTtotal<-na.omit(xts(x=SAmbrogio[,22], order.by=datestotal))*0.78
MAtsNH4OUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,22], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNH4INtotal<-na.omit(xts(x=SAmbrogio[,21], order.by=datestotal))*0.78



tsNO2OUTtotal<-na.omit(xts(x=SAmbrogio[,24], order.by=datestotal))
MAtsNO2OUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,24], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNO2INtotal<-na.omit(xts(x=SAmbrogio[,23], order.by=datestotal))



tsNO3OUTtotal<-na.omit(xts(x=SAmbrogio[,26], order.by=datestotal))
MAtsNO3OUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,26], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNO3INtotal<-na.omit(xts(x=SAmbrogio[,25], order.by=datestotal))




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
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

# plot(as.zoo(tsNtotINtotal), xlab="Mesi",ylab=expression(paste("N"[TOT-IN]," ,N"[TOT-OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,100),col="grey")
# drawTimeAxis(as.zoo(tsNtotINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
# axis(side=2,at=seq(from = 0,to = 100,by = 25),las=2)
# grid(nx=NA,ny=4)
# par(new=T)
# plot(as.zoo(tsNtotINtotal), xlab="Mesi",ylab=expression(paste("N"[TOT-IN]," ,N"[TOT-OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,100),col="grey")
# lines(as.zoo(MAtsNtotINtotal))

plot(as.zoo(tsNH4OUTtotal),type="n", xlab="Mesi",ylab=expression(paste("N-NH"[4-OUT]^"+"," ,N-NO"[2-OUT]^"-"," ,N-NO"[3-OUT]^"-"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,25),col="orange")
drawTimeAxis(as.zoo(tsNH4OUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsNH4OUTtotal),col="orange")
lines(as.zoo(MAtsNH4OUTtotal[index(tsNH4OUTtotal)]),col="red")
lines(as.zoo(tsNO2OUTtotal),col="green")
lines(as.zoo(MAtsNO2OUTtotal[index(tsNO2OUTtotal)]),col="darkgreen")
lines(as.zoo(tsNO3OUTtotal),col="violet")
lines(as.zoo(MAtsNO3OUTtotal[index(tsNO3OUTtotal)]),col="purple")



abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
abline(h=15*0.78,col="red",lty=2)
abline(h=0.6,col="darkgreen",lty=2)
abline(h=20,col="purple",lty=2)
text(x=index(tsNtotIN2015[182,]),y=23.5,label="2015")
text(x=index(tsNtotIN2016[182,]),y=23.5,label="2016")
text(x=index(tsNtotIN2017[182,]),y=23.5,label="2017")
text(x=index(tsNtotIN2018[90,]),y=23.5,label="2018")
legend(x=index(tsNtotIN2015[30,]),y=22, c(expression(paste("N-NH"[4-OUT]^"+")), expression(paste("N-NO"[2-OUT]^"-")),expression(paste("N-NO"[3-OUT]^"-")),expression(paste("MA NH"[4-OUT]^"+"," (mensile)")), expression(paste("MA N-NO"[2-OUT]^"-"," (mensile)")),expression(paste("MA N-NO"[3-OUT]^"-"," (mensile)")),expression(paste("Limite N-NH"[4-OUT]^"+")), expression(paste("Limite N-NO"[2-OUT]^"-")),expression(paste("Limite N-NO"[3-OUT]^"-"))),col=c("orange","green","violet","red","darkgreen","purple","red","darkgreen","purple"),lty=c(1,1,1,1,1,1,2,2,2),lwd=c(1,1,1,1,1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNH4OUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NH"[4-OUT]^"+")), ylim=c(0,12),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 12,by = 2),las=2,labels = format(seq(from = 0,to = 12,by = 2), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNO2OUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NO"[2-OUT]^"-")), ylim=c(0,1),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 1,by = 0.2),las=2,labels = format(seq(from = 0,to = 1,by = 0.2), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNO3OUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NO"[3-OUT]^"-")), ylim=c(0,18),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 18,by = 3),las=2,labels = format(seq(from = 0,to = 18,by = 3), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNH4OUTtotal),as.zoo(tsNO2OUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N-NH"[4-OUT]^"+"," [mg/L]")),ylab=expression(paste("N-NO"[2-OUT]^"-"," [mg/L]")),cex.lab="1.2",xlim=c(0,25),ylim=c(0,2.5))
axis(side=2,at=seq(from = 0,to = 2.5,by = 0.5),las=2,format(seq(from = 0,to = 2.5,by = 0.5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 25,by = 5),las=1,format(seq(from = 0,to = 25,by = 5), big.mark = ".", decimal.mark = ","))
grid(nx=5,ny=3,col="grey")


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNH4INtotal),as.zoo(tsNH4OUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N-NH"[4-IN]^"+"," [mg/L]")),ylab=expression(paste("N-NH"[4-OUT]^"+"," [mg/L]")),cex.lab="1.2",xlim=c(0,120),ylim=c(0,25))
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2,format(seq(from = 0,to = 25,by = 5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 120,by = 30),las=1,format(seq(from = 0,to = 120,by = 30), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=5,col="grey")

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNO2OUTtotal),as.zoo(tsNO2INtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N-NO"[2-IN]^"-"," [mg/L]")),ylab=expression(paste("N-NO"[2-OUT]^"-"," [mg/L]")),cex.lab="1.2",xlim=c(0,1),ylim=c(0,1))
axis(side=2,at=seq(from = 0,to = 1,by = 0.5),las=2,format(seq(from = 0,to = 1,by = 0.5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1,by = 0.5),las=1,format(seq(from = 0,to = 1,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=2,ny=2,col="grey")

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNO3OUTtotal),as.zoo(tsNO3INtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N-NO"[3-IN]^"-"," [mg/L]")),ylab=expression(paste("N-NO"[3-OUT]^"-"," [mg/L]")),cex.lab="1.2",xlim=c(0,2),ylim=c(0,20))
axis(side=2,at=seq(from = 0,to = 20,by = 5),las=2,format(seq(from = 0,to = 20,by = 5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 2,by = 0.5),las=1,format(seq(from = 0,to = 2,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")

