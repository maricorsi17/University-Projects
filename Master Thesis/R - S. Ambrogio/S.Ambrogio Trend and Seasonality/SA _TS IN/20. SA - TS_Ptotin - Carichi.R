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
tsCaricoPtotINtotal_original<-na.approx(xts(x=SAmbrogio[,29], order.by=datestotal))

tsCaricoPtotINtotal<-(na.omit(xts(x=SAmbrogio[,29], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoPtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,29], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsCaricoPtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsCaricoPtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsCaricoPtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsCaricoPtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoPtotINtotal),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,120),col="grey")
drawTimeAxis(as.zoo(tsCaricoPtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 120,by = 20),las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsCaricoPtotINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCaricoPtotINtotal)),lwd=2)
a<-lm(tsCaricoPtotINtotal[which(.indexyear(tsCaricoPtotINtotal)>(2015-1900))]~index(tsCaricoPtotINtotal[which(.indexyear(tsCaricoPtotINtotal)>(2015-1900))]))
ablineclip(x1=index(tsPtotIN2016[1,]),a,col="red",lwd=2,lty=5)
b<-lm(tsCaricoPtotINtotal[which(.indexyear(tsCaricoPtotINtotal)==(2015-1900))]~index(tsCaricoPtotINtotal[which(.indexyear(tsCaricoPtotINtotal)==(2015-1900))]))
ablineclip(x2=index(tsPtotIN2015[361,]),b,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100
perc_b<--(1-coredata(b$fitted.values[length(b$fitted.values)])/coredata(b$fitted.values[1]))*100

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
text(x=index(tsPtotIN2015[182,]),y=110,label="2015")
text(x=index(tsPtotIN2016[182,]),y=110,label="2016")
text(x=index(tsPtotIN2017[182,]),y=110,label="2017")
text(x=index(tsPtotIN2018[91,]),y=110,label="2018")
text(x=index(tsPtotIN2017[181,]),y=50, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
text(x=index(tsPtotIN2015[181,]),y=10, label=paste("Variazione' = ",format(round(perc_b,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsPtotIN2017[250,]),y=99, c(expression(paste("P"[tot-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



# DETRENDING
tsCaricoPtotINtotalDEN<-tsCaricoPtotINtotal-(ts_trend(tsCaricoPtotINtotal))
mediaDET<-mean(tsCaricoPtotINtotalDEN)

# STAGIONALITA'
tsCaricoPtotINtotalAGG<-apply.weekly(tsCaricoPtotINtotalDEN,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCaricoPtotINtotalAGG,lag.max = 60, main=expression("Carico P"[tot-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)