# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("tsbox")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQINtotal<-xts(x=SAmbrogio[,6], order.by=datestotal)
tsQOUTtotal<-xts(x=SAmbrogio[,8],order.by=datestotal)

## Creare un oggetto con le date (2015)
tsQIN2015<-tsQINtotal["2015"]

## Creare un oggetto con le date (2016)
tsQIN2016<-tsQINtotal["2016"]

## Creare un oggetto con le date (2017)
tsQIN2017<-tsQINtotal["2017"]

## Creare un oggetto con le date (2018)
tsQIN2018<-tsQINtotal["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsQINtotal),type="n",xlab="Mesi",ylab=expression(paste("Q"[IN]," [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,14000), lwd=2,col="blue")
drawTimeAxis(as.zoo(tsQINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 14000,by = 1000),las=2,labels = format(seq(from = 0,to = 14000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=14,col="grey")
lines(as.zoo(tsQINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsQINtotal)),lwd=2)
a<-lm(tsQINtotal[which(.indexyear(tsQINtotal)>(2015-1900))]~index(tsQINtotal[which(.indexyear(tsQINtotal)>(2015-1900))]))
ablineclip(x1=index(tsQIN2016[1,]),a,col="red",lwd=2,lty=5)
b<-lm(tsQINtotal[which(.indexyear(tsQINtotal)==(2015-1900))]~index(tsQINtotal[which(.indexyear(tsQINtotal)==(2015-1900))]))
ablineclip(x2=index(tsQIN2015[365,]),b,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100
perc_b<--(1-coredata(b$fitted.values[length(b$fitted.values)])/coredata(b$fitted.values[1]))*100

abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=13500,label="2015")
text(x=index(tsQIN2016[182,]),y=13500,label="2016")
text(x=index(tsQIN2017[182,]),y=13500,label="2017")
text(x=index(tsQIN2018[90,]),y=13500,label="2018")
text(x=index(tsQIN2017[181,]),y=1000, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
text(x=index(tsQIN2015[181,]),y=1000, label=paste("Variazione = ",format(round(perc_b,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsQIN2015[30,]),y=12100, c(expression("Q"[IN]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsQOUTtotal),type="n",xlab="Mesi",ylab=expression(paste("Q"[OUT]," [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,14000), lwd=2,col="blue")
drawTimeAxis(as.zoo(tsQINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 14000,by = 1000),las=2,labels = format(seq(from = 0,to = 14000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=14,col="grey")
lines(as.zoo(tsQOUTtotal),col="grey")
lines(as.zoo(ts_trend(tsQOUTtotal)),lwd=2)
a1<-lm(tsQOUTtotal[which(.indexyear(tsQOUTtotal)>(2015-1900))]~index(tsQOUTtotal[which(.indexyear(tsQOUTtotal)>(2015-1900))]))
ablineclip(x1=index(tsQIN2016[1,]),a1,col="red",lwd=2,lty=5)
b1<-lm(tsQOUTtotal[which(.indexyear(tsQOUTtotal)==(2015-1900))]~index(tsQOUTtotal[which(.indexyear(tsQOUTtotal)==(2015-1900))]))
ablineclip(x2=index(tsQIN2015[365,]),b1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100
perc_b1<--(1-coredata(b1$fitted.values[length(b1$fitted.values)])/coredata(b1$fitted.values[1]))*100

abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=13500,label="2015")
text(x=index(tsQIN2016[182,]),y=13500,label="2016")
text(x=index(tsQIN2017[182,]),y=13500,label="2017")
text(x=index(tsQIN2018[90,]),y=13500,label="2018")
text(x=index(tsQIN2017[181,]),y=1000, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
text(x=index(tsQIN2015[181,]),y=1000, label=paste("Variazione = ",format(round(perc_b1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsQIN2015[30,]),y=12100, c(expression("Q"[OUT]),"Regressione","LOESS"),col=c("grey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsQINtotalDET<-tsQINtotal-(ts_trend(tsQINtotal))
mediaDET<-mean(tsQINtotalDET)

# STAGIONALITA'
tsQINtotalAGG<-apply.weekly(tsQINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsQINtotalAGG,lag.max = 60, main=expression("Q"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

