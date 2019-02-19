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
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")
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
par(mar=c(7,7,4,4),mgp=c(5,1,0)) #margini e distanza etichette-asse

options(scipen=0)
plot(as.zoo(tsECINtotal), xlab="Mesi",ylab=expression(paste("Escherichia coli "[IN]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,20000000), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 20000000, by = 5000000), las=2,format(seq(from = 0, to = 20000000, by = 5000000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsECINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsECINtotal)),lwd=2)
a<-lm(tsECINtotal~index(tsECINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=19000000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=19000000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=19000000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=19000000,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=14000000, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2015[30,]),y=18000000, c(expression("E. coli"[IN]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# Plottare la time series ECOUT
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOX1total), xlab="Mesi",ylab=expression(paste("Escherichia coli "[OUT]," [UFC/100 mL]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,80000), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 80000, by = 10000), las=2,format(seq(from = 0, to = 80000, by = 10000),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsECOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsECOUTtotal)),lwd=2)
a1<-lm(tsECOUTtotal~index(tsECOUTtotal))
abline(a1,col="red",lwd=2,lty=5)
#ablineclip(h=5000,x1=as.numeric(as.Date("2015-04-01")),x2=as.numeric(as.Date("2015-09-30")),col="red",lty=2)
#ablineclip(h=5000,x1=as.numeric(as.Date("2016-04-01")),x2=as.numeric(as.Date("2016-09-30")),col="red",lty=2)
#ablineclip(h=5000,x1=as.numeric(as.Date("2017-04-01")),x2=as.numeric(as.Date("2017-09-30")),col="red",lty=2)
#ablineclip(h=5000,x1=as.numeric(as.Date("2018-04-01")),x2=as.numeric(as.Date("2018-06-30")),col="red",lty=2)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=75000,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=75000,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=75000,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=75000,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=15000, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2015[30,]),y=69000, c(expression(paste("E. coli"[OUT])),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

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