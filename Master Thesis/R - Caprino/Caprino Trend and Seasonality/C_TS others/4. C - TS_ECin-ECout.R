# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")
library("tsbox")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/NoOutliers_Caprino.CSV", header=TRUE, sep=";")
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
par(mar=c(7,7,4,4),mgp=c(5,1,0)) #margini e distanza etichette-asse

options(scipen=0)
plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[IN]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,20000000), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 20000000, by = 5000000), las=2,format(seq(from = 0, to = 20000000, by = 5000000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsECINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsECINtotal)),lwd=2)
a<-lm(tsECINtotal~index(tsECINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=18500000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=18500000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=18500000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=18500000,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=14000000, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2015[30,]),y=17500000, c(expression("E. coli"[IN]),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# Plottare la time series ECOUT
windows(width = 16,height = 9)
par(mar=c(7,7,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[OUT]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,5000), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 5000, by = 1000), las=2,format(seq(from = 0, to = 5000, by = 1000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsECOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsECOUTtotal)),lwd=2)
a1<-lm(tsECOUTtotal~index(tsECOUTtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=4750,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=4750,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=4750,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=4750,label="2018")
text(x=index(tsSSSINOX2017[181,]),y=1250, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2015[30,]),y=4450, c(expression(paste("E. coli"[OUT])),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsECINtotalDET<-tsECINtotal-(ts_trend(tsECINtotal))
mediaECINDET<-mean(tsECINtotalDET)

tsECOUTtotalDET<-tsECOUTtotal-(ts_trend(tsECOUTtotal))
mediaECOUTDET<-mean(tsECOUTtotalDET)

# STAGIONALITA'
tsECINtotalAGG<-apply.weekly(tsECINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsECINtotalAGG,lag.max = 60, main=expression("E. coli"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsECOUTtotalAGG<-apply.weekly(tsECOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsECOUTtotalAGG,lag.max = 60, main=expression("E. coli"[OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)