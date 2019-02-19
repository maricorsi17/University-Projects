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
Caprino[,35] = as.numeric(gsub("\\.","",Caprino[,35]))
Caprino[,36] = as.numeric(gsub("\\.","",Caprino[,36]))

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")

tsSSSINOXtotal<-na.approx(xts(x=Caprino[,37], order.by=datestotal))
tsECINtotal<-na.omit(xts(x=Caprino[,35], order.by=datestotal))
MAtsECINtotal<-na.omit(rollapply(xts(x=Caprino[,35], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsECOUTtotal<-na.omit(xts(x=Caprino[,36], order.by=datestotal))
MAtsECOUTtotal<-na.omit(rollapply(xts(x=Caprino[,36], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsSSSINOX2015<-tsSSSINOXtotal["2015"]
## Creare un oggetto con le date (2016)
tsSSSINOX2016<-tsSSSINOXtotal["2016"]
## Creare un oggetto con le date (2017)
tsSSSINOX2017<-tsSSSINOXtotal["2017"]
## Creare un oggetto con le date (2018)
tsSSSINOX2018<-tsSSSINOXtotal["2018"]


# Plottare la time series ECIN
windows(width = 16,height = 9)
par(mar=c(7,7,4,6),mgp=c(5,1,0)) #margini e distanza etichette-asse

options(scipen=0)
plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[IN]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,20000000), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 20000000, by = 5000000), las=2,format(seq(from = 0, to = 20000000, by = 5000000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsECINtotal),col="darkslategrey")
lines(as.zoo(MAtsECINtotal[index(tsECINtotal)]))


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=18500000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=18500000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=18500000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=18500000,label="2018")
legend(x=index(tsSSSINOX2015[30,]),y=17500000, c(expression("E. coli"[IN]),expression(paste("MA E. coli"[IN]," (mensile)"))),col=c("darkslategrey", "black"),lty=c(1,1),lwd=c(1,1),bg="white")


# Plottare la time series ECOUT
windows(width = 16,height = 9)
par(mar=c(7,7,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[OUT]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,5000), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 5000, by = 1000), las=2,format(seq(from = 0, to = 5000, by = 1000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsECOUTtotal),col="darkslategrey")
lines(as.zoo(MAtsECOUTtotal[index(tsECOUTtotal)]))


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=4750,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=4750,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=4750,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=4750,label="2018")
legend(x=index(tsSSSINOX2015[30,]),y=4450, c(expression(paste("E. coli"[OUT])),expression(paste("MA E. coli"[OUT]," (mensile)"))),col=c("darkslategrey", "black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,7,4,4),mgp=c(5,1,0))

boxplot(coredata(tsECINtotal),yaxt="n",ylab="[UFC/100 mL]",main=expression(paste("E. coli "[IN])), ylim=c(0,45000000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 45000000,by = 5000000),las=2,labels = format(seq(from = 0,to = 45000000,by = 5000000), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsECOUTtotal),yaxt="n",ylab="[UFC/100 mL]",main=expression(paste("E. coli"[OUT])), ylim=c(0,4600),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 4600,by = 200),las=2,labels = format(seq(from = 0,to = 4600,by = 200), big.mark = ".", decimal.mark = ","))
