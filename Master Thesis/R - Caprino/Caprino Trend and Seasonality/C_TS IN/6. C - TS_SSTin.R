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
tsSSTINtotal_original<-na.approx(xts(x=Caprino[,13], order.by=datestotal))
tsSSTINtotal<-na.omit(xts(x=Caprino[,13], order.by=datestotal))
MAtsSSTINtotal<-na.omit(rollapply(xts(x=Caprino[,13], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsCODINtotal<-na.omit(xts(x=Caprino[,17], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)
tsSSTIN2015<-tsSSTINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsSSTIN2016<-tsSSTINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsSSTIN2017<-tsSSTINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsSSTIN2018<-tsSSTINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINtotal),type="n", xlab="Mesi",ylab=expression(paste("SST"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,5000),col="grey")
drawTimeAxis(as.zoo(tsSSTINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from =0 ,to = 5000,by = 500),las=2,format(seq(from = 0,to = 5000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=10,col="grey")
par(new=T)
lines(as.zoo(tsSSTINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINtotal)),lwd=2)
a<-lm(tsSSTINtotal~index(tsSSTINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsSSTIN2016[1,]),lwd=2)
abline(v=index(tsSSTIN2017[1,]),lwd=2)
abline(v=index(tsSSTIN2018[1,]),lwd=2)
text(x=index(tsSSTIN2015[182,]),y=4780,label="2015")
text(x=index(tsSSTIN2016[182,]),y=4780,label="2016")
text(x=index(tsSSTIN2017[182,]),y=4780,label="2017")
text(x=index(tsSSTIN2018[90,]),y=4780,label="2018")
text(x=index(tsSSTIN2016[181,]),y=2250, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSTIN2017[120,]),y=4500, c(expression(paste("SST"[IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsSSTINtotalDET<-tsSSTINtotal-(ts_trend(tsSSTINtotal))
mediaDET<-mean(tsSSTINtotalDET)

# STAGIONALITA'
tsSSTINtotalAGG<-apply.weekly(tsSSTINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINtotalAGG,lag.max = 60, main=expression("SST"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)