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

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsBODINtotal_original<-na.approx(xts(x=Caprino[,15], order.by=datestotal))
tsBODINtotal<-na.omit(xts(x=Caprino[,15], order.by=datestotal))
MAtsBODINtotal<-na.omit(rollapply(xts(x=Caprino[,15], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsBODOUTtotal<-na.omit(xts(x=Caprino[,16], order.by=datestotal))
MAtsBODOUTtotal<-na.omit(rollapply(xts(x=Caprino[,16], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsBODIN2015<-tsBODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsBODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsBODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsBODINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,25),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsBODOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsBODOUTtotal)),lwd=2)
a<-lm(tsBODOUTtotal~index(tsBODOUTtotal))
abline(a,col="red",lwd=2,lty=5)
#abline(h=20,col="red",lty=2)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=23,label="2015")
text(x=index(tsBODIN2016[182,]),y=23,label="2016")
text(x=index(tsBODIN2017[182,]),y=23,label="2017")
text(x=index(tsBODIN2018[90,]),y=23,label="2018")
text(x=index(tsBODIN2016[181,]),y=2.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2017[60,]),y=22, c(expression(paste("BOD"[5-OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsBODOUTtotalDET<-tsBODOUTtotal-(ts_trend(tsBODOUTtotal))
mediaDET<-mean(tsBODOUTtotalDET)

# STAGIONALITA'
tsBODOUTtotalAGG<-apply.weekly(tsBODOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsBODOUTtotalAGG,lag.max = 60, main=expression("BOD"[5-OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)