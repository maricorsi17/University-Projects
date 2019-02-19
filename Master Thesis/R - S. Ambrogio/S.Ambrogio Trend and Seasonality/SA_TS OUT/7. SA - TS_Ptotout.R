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
tsPtotINtotal<-na.omit(xts(x=SAmbrogio[,29], order.by=datestotal))
MAtsPtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsPtotOUTtotal_original<-na.approx(xts(x=SAmbrogio[,30], order.by=datestotal))
tsPtotOUTtotal<-na.omit(xts(x=SAmbrogio[,30], order.by=datestotal))
MAtsPtotOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,30], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsPtotOUT2015<-tsPtotOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotOUT2016<-tsPtotOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotOUT2017<-tsPtotOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotOUT2018<-tsPtotOUTtotal_original["2018"]

media2015<-mean(tsPtotOUT2015)
media2016<-mean(tsPtotOUT2016)
media2017<-mean(tsPtotOUT2017)
media2018<-mean(tsPtotOUT2018)


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot-OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,10),col="grey")
drawTimeAxis(as.zoo(tsPtotOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 10,by = 2),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsPtotOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsPtotOUTtotal)),lwd=2)
a<-lm(tsPtotOUTtotal~index(tsPtotOUTtotal))
abline(a,col="red",lwd=2,lty=5)
#ablineclip(h=media2015,x1=index(tsPtotOUTtotal_original[1,]),x2=index(tsPtotOUTtotal_original[365,]),col="darkgreen",lwd=2)
#ablineclip(h=media2016,x1=index(tsPtotOUT2016[1,]),x2=index(tsPtotOUT2016[366,]),col="darkgreen",lwd=2)
#ablineclip(h=media2017,x1=index(tsPtotOUT2017[1,]),x2=index(tsPtotOUT2017[365,]),col="darkgreen",lwd=2)
#ablineclip(h=media2018,x1=index(tsPtotOUT2018[1,]),x2=index(tsPtotOUTtotal_original[1268,]),col="darkgreen",lwd=2)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsPtotOUT2016[1,]),lwd=2)
abline(v=index(tsPtotOUT2017[1,]),lwd=2)
abline(v=index(tsPtotOUT2018[1,]),lwd=2)
#abline(h=10,col="red",lty=2)
#abline(h=2,col="purple",lty=2)
text(x=index(tsPtotOUT2015[182,]),y=9,label="2015")
text(x=index(tsPtotOUT2016[182,]),y=9,label="2016")
text(x=index(tsPtotOUT2017[182,]),y=9,label="2017")
text(x=index(tsPtotOUT2018[90,]),y=9,label="2018")
text(x=index(tsPtotOUT2016[181,]),y=5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsPtotOUT2017[220,]),y=8.5, c(expression(paste("P"[tot-OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsPtotOUTtotalDET<-tsPtotOUTtotal-(ts_trend(tsPtotOUTtotal))
mediaDET<-mean(tsPtotOUTtotalDET)

# STAGIONALITA'
tsPtotOUTtotalAGG<-apply.weekly(tsPtotOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsPtotOUTtotalAGG,lag.max = 60, main=expression("P"[tot-OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)