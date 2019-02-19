library("xts")
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
tsNtotINtotal<-na.omit(xts(x=SAmbrogio[,31], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,31], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNtotOUTtotal_original<-na.approx(xts(x=SAmbrogio[,32], order.by=datestotal))
tsNtotOUTtotal<-na.omit(xts(x=SAmbrogio[,32], order.by=datestotal))
MAtsNtotOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,32], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsNtotOUT2015<-tsNtotOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotOUT2016<-tsNtotOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotOUT2017<-tsNtotOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotOUT2018<-tsNtotOUTtotal_original["2018"]

media2015<-mean(tsNtotOUT2015)
media2016<-mean(tsNtotOUT2016)
media2017<-mean(tsNtotOUT2017)
media2018<-mean(tsNtotOUT2018)

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot-OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,30),col="grey")
drawTimeAxis(as.zoo(tsNtotOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 30,by = 5),las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsNtotOUTtotal),col="darkslategrey")
#abline(h=15,col="blue",lty=2)
#ablineclip(h=media2015,x1=index(tsNtotOUTtotal[1,]),x2=index(tsNtotOUTtotal_original[365,]),col="darkgreen",lwd=2)
#ablineclip(h=media2016,x1=index(tsNtotOUT2016[1,]),x2=index(tsNtotOUT2016[366,]),col="darkgreen",lwd=2)
#ablineclip(h=media2017,x1=index(tsNtotOUT2017[1,]),x2=index(tsNtotOUT2017[365,]),col="darkgreen",lwd=2)
#ablineclip(h=media2018,x1=index(tsNtotOUT2018[1,]),x2=index(tsNtotOUTtotal_original[1268,]),col="darkgreen",lwd=2)
lines(as.zoo(ts_trend(tsNtotOUTtotal)),lwd=2)
a<-lm(tsNtotOUTtotal~index(tsNtotOUTtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsNtotOUT2016[1,]),lwd=2)
abline(v=index(tsNtotOUT2017[1,]),lwd=2)
abline(v=index(tsNtotOUT2018[1,]),lwd=2)
text(x=index(tsNtotOUT2015[182,]),y=28.5,label="2015")
text(x=index(tsNtotOUT2016[182,]),y=28.5,label="2016")
text(x=index(tsNtotOUT2017[182,]),y=28.5,label="2017")
text(x=index(tsNtotOUT2018[90,]),y=28.5,label="2018")
text(x=index(tsNtotOUT2016[181,]),y=2, label=paste("Variazione = ",format(round(perc_a,digits=1),nsmall=1,decimal.mark = ","),"%"),col="red")

legend(x=index(tsNtotOUT2015[30,]),y=26, c(expression(paste("N"[tot-OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsNtotOUTtotalDET<-tsNtotOUTtotal-(ts_trend(tsNtotOUTtotal))
mediaDET<-mean(tsNtotOUTtotalDET)

# STAGIONALITA'
tsNtotOUTtotalAGG<-apply.weekly(tsNtotOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsNtotOUTtotalAGG,lag.max = 60, main=expression("N"[tot-OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)