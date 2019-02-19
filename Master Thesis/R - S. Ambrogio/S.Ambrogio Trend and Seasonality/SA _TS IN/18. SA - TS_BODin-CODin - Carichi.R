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

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsCaricoBODINtotal_original<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))

tsCaricoBODINtotal<-(na.omit(xts(x=SAmbrogio[,17], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoBODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,17], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



tsCaricoCODINtotal<-(na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))

tsTINtotal<-na.approx(xts(x=SAmbrogio[,4], order.by=datestotal))


## Creare un oggetto con le date (2015)
tsBODIN2015<-tsCaricoBODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsCaricoBODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsCaricoBODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsCaricoBODINtotal_original["2018"]


# Plottare la time series BOD 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoBODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCaricoBODINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCaricoBODINtotal)),lwd=2)
a<-lm(tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)>(2015-1900))]~index(tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)>(2015-1900))]))
ablineclip(x1=index(tsBODIN2016[1,]),a,col="red",lwd=2,lty=5)
b<-lm(tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)==(2015-1900))]~index(tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)==(2015-1900))]))
ablineclip(x2=index(tsBODIN2015[361,]),b,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100
perc_b<--(1-coredata(b$fitted.values[length(b$fitted.values)])/coredata(b$fitted.values[1]))*100

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=3750,label="2015")
text(x=index(tsBODIN2016[182,]),y=3750,label="2016")
text(x=index(tsBODIN2017[182,]),y=3750,label="2017")
text(x=index(tsBODIN2018[91,]),y=3750,label="2018")
text(x=index(tsBODIN2016[181,]),y=2250, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
text(x=index(tsBODIN2015[181,]),y=250, label=paste("Variazione = ",format(round(perc_b,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2017[260,]),y=3450, c(expression(paste("BOD"[5-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# Plottare la time series COD 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoCODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,11000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 11000,by = 1000),las=2,format(seq(from = 0,to = 11000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=11,col="grey")
lines(as.zoo(tsCaricoCODINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCaricoCODINtotal)),lwd=2)
a1<-lm(tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)>(2015-1900))]~index(tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)>(2015-1900))]))
ablineclip(x1=index(tsBODIN2016[1,]),a1,col="red",lwd=2,lty=5)
b1<-lm(tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)==(2015-1900))]~index(tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)==(2015-1900))]))
ablineclip(x2=index(tsBODIN2015[361,]),b1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100
perc_b1<--(1-coredata(b1$fitted.values[length(b1$fitted.values)])/coredata(b1$fitted.values[1]))*100

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=10500,label="2015")
text(x=index(tsBODIN2016[182,]),y=10500,label="2016")
text(x=index(tsBODIN2017[182,]),y=10500,label="2017")
text(x=index(tsBODIN2018[91]),y=10500,label="2018")
text(x=index(tsBODIN2016[181,]),y=6500, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
text(x=index(tsBODIN2015[181,]),y=6500, label=paste("Variazione = ",format(round(perc_b1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2015[30,]),y=9950, c(expression(paste("COD"[IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsCaricoBODINtotalDET<-tsCaricoBODINtotal-(ts_trend(tsCaricoBODINtotal))
mediaCarBODDET<-mean(tsCaricoBODINtotalDET)

tsCaricoCODINtotalDET<-tsCaricoCODINtotal-(ts_trend(tsCaricoCODINtotal))
mediaCarCODDET<-mean(tsCaricoCODINtotalDET)

# STAGIONALITA'
tsCaricoBODINtotalAGG<-apply.weekly(tsCaricoBODINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCaricoBODINtotalAGG,lag.max = 60, main=expression("Carico BOD"[5-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsCaricoCODINtotalAGG<-apply.weekly(tsCaricoCODINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCaricoCODINtotalAGG,lag.max = 60, main=expression("Carico COD"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)