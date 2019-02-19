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
tsSSTOUTtotal_original<-na.approx(xts(x=SAmbrogio[,16], order.by=datestotal))
tsSSTOUTtotal<-na.omit(xts(x=SAmbrogio[,16], order.by=datestotal))
MAtsSSTOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,16], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



tsQOUTtotal<-na.omit(xts(x=SAmbrogio[,8], order.by=datestotal))
MAtsQOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,8], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsSSTOUT2015<-tsSSTOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsSSTOUT2016<-tsSSTOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsSSTOUT2017<-tsSSTOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsSSTOUT2018<-tsSSTOUTtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTOUTtotal),type="n", xlab="Mesi",ylab=expression(paste("SST"[OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,40),col="grey")
drawTimeAxis(as.zoo(tsSSTOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 40,by = 5),las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsSSTOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTOUTtotal)),lwd=2)
a<-lm(tsSSTOUTtotal~index(tsSSTOUTtotal))
abline(a,col="red",lwd=2,lty=5)
#abline(h=35,col="blue",lty=2)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsSSTOUT2016[1,]),lwd=2)
abline(v=index(tsSSTOUT2017[1,]),lwd=2)
abline(v=index(tsSSTOUT2018[1,]),lwd=2)
text(x=index(tsSSTOUT2015[182,]),y=37.5,label="2015")
text(x=index(tsSSTOUT2016[182,]),y=37.5,label="2016")
text(x=index(tsSSTOUT2017[182,]),y=37.5,label="2017")
text(x=index(tsSSTOUT2018[90,]),y=37.5,label="2018")
text(x=index(tsSSTOUT2016[181,]),y=2.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSTOUT2017[90,]),y=34, c(expression(paste("SST"[OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsSSTOUTtotalDET<-tsSSTOUTtotal-(ts_trend(tsSSTOUTtotal))
mediaDET<-mean(tsSSTOUTtotalDET)

# STAGIONALITA'
tsSSTOUTtotalAGG<-apply.weekly(tsSSTOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTOUTtotalAGG,lag.max = 60, main=expression("SST"[OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)