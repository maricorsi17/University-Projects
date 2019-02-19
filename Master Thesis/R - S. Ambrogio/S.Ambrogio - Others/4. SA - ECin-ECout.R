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
SAmbrogio[,37] = as.numeric(gsub("\\.","",SAmbrogio[,37]))
SAmbrogio[,38] = as.numeric(gsub("\\.","",SAmbrogio[,38]))


# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")

tsSSSINOX1total<-na.approx(xts(x=SAmbrogio[,39], order.by=datestotal))
tsECINtotal<-na.omit(xts(x=SAmbrogio[,37], order.by=datestotal))
MAtsECINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,37], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



tsECOUTtotal<-na.omit(xts(x=SAmbrogio[,38], order.by=datestotal))
MAtsECOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,38], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsSSSINOX2015<-tsSSSINOX1total["2015"]
## Creare un oggetto con le date (2016)
tsSSSINOX2016<-tsSSSINOX1total["2016"]
## Creare un oggetto con le date (2017)
tsSSSINOX2017<-tsSSSINOX1total["2017"]
## Creare un oggetto con le date (2018)
tsSSSINOX2018<-tsSSSINOX1total["2018"]


# Plottare la time series ECIN
windows(width = 16,height = 9)
par(mar=c(7,7,4,6),mgp=c(5,1,0)) #margini e distanza etichette-asse

options(scipen=0)
plot(as.zoo(tsECINtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[IN]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,20000000), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 20000000, by = 5000000), las=2,format(seq(from = 0, to = 20000000, by = 5000000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsECINtotal),col="darkslategrey")
lines(as.zoo(MAtsECINtotal[index(tsECINtotal)]))


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=19000000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=19000000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=19000000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=19000000,label="2018")
legend(x=index(tsSSSINOX2015[30,]),y=18000000, c(expression("E. coli"[IN]),expression(paste("MA E. coli"[IN]," (mensile)"))),col=c("darkslategrey", "black"),lty=c(1,1),lwd=c(1,1),bg="white")


# Plottare la time series ECOUT
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOX1total), xlab="Mesi",ylab=expression(paste("Escherichia coli "[OUT]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,80000), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 80000, by = 10000), las=2,format(seq(from = 0, to = 80000, by = 10000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsECOUTtotal),col="darkslategrey")
lines(as.zoo(MAtsECOUTtotal[index(tsECOUTtotal)]))
ablineclip(h=5000,x1=as.numeric(as.Date("2015-04-01")),x2=as.numeric(as.Date("2015-09-30")),col="blue",lty=2)
ablineclip(h=5000,x1=as.numeric(as.Date("2016-04-01")),x2=as.numeric(as.Date("2016-09-30")),col="blue",lty=2)
ablineclip(h=5000,x1=as.numeric(as.Date("2017-04-01")),x2=as.numeric(as.Date("2017-09-30")),col="blue",lty=2)
ablineclip(h=5000,x1=as.numeric(as.Date("2018-04-01")),x2=as.numeric(as.Date("2018-06-30")),col="blue",lty=2)


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=75000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=75000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=75000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=75000,label="2018")
legend(x=index(tsSSSINOX2015[30,]),y=69000, c(expression(paste("E. coli"[OUT])),expression(paste("MA E. coli"[OUT]," (mensile)")),"Limite - da 01/04 a 30/09"),col=c("darkslategrey", "black","blue"),lty=c(1,1,2),lwd=c(1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,7,4,4),mgp=c(5,1,0))

boxplot(coredata(tsECINtotal),yaxt="n",ylab="[UFC/100 mL]",main=expression(paste("E. coli "[IN])), ylim=c(0,55000000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0, to = 55000000, by = 5000000), las=2,format(seq(from = 0, to = 55000000, by = 5000000),big.mark = ".",decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsECOUTtotal),yaxt="n",ylab="[UFC/100 mL]",main=expression(paste("E. coli "[OUT])), ylim=c(0,80000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0, to = 80000, by = 10000), las=2,format(seq(from = 0, to = 80000, by = 10000),big.mark = ".",decimal.mark = ","))

# SCATTERPLOT
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0))
plot(as.zoo(tsECINtotal),as.zoo(tsECOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("E. coli"[IN]," [UFC/100 mL]")),ylab=expression(paste("E. coli"[OUT]," [UFC/100 mL]")),cex.lab="1.2",xlim=c(0,55000000),ylim=c(0,80000))
axis(side=2,at=seq(from = 0,to = 80000,by = 10000),las=2,format(seq(from = 0,to = 80000,by = 10000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 55000000,by = 5000000),las=1,format(seq(from = 0,to = 55000000,by = 5000000), big.mark = ".", decimal.mark = ","))
grid(nx=11,ny=8,col="grey")