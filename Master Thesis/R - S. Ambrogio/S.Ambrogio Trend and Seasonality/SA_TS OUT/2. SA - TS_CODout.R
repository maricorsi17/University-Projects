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
tsCODINtotal_original<-na.approx(xts(x=SAmbrogio[,19], order.by=datestotal))
tsCODINtotal<-na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsCODOUTtotal<-na.omit(xts(x=SAmbrogio[,20], order.by=datestotal))
MAtsCODOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,20], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsCODIN2015<-tsCODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsCODIN2016<-tsCODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsCODIN2017<-tsCODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsCODIN2018<-tsCODINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCODINtotal),type="n",xlab="Mesi",ylab=expression(paste("COD"[OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,150),col="grey")
drawTimeAxis(as.zoo(tsCODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 150,by = 25),las=2,format(seq(from = 0,to = 150,by = 25), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsCODOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCODOUTtotal)),lwd=2)
a<-lm(tsCODOUTtotal~index(tsCODOUTtotal))
abline(a,col="red",lwd=2,lty=5)
#abline(h=125,col="blue",lty=2)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsCODIN2016[1,]),lwd=2)
abline(v=index(tsCODIN2017[1,]),lwd=2)
abline(v=index(tsCODIN2018[1,]),lwd=2)
text(x=index(tsCODIN2015[182,]),y=140,label="2015")
text(x=index(tsCODIN2016[182,]),y=140,label="2016")
text(x=index(tsCODIN2017[182,]),y=140,label="2017")
text(x=index(tsCODIN2018[90,]),y=140,label="2018")
text(x=index(tsCODIN2016[181,]),y=112.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsCODIN2015[30,]),y=130, c(expression(paste("COD"[OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsCODOUTtotalDET<-tsCODOUTtotal-(ts_trend(tsCODOUTtotal))
mediaDET<-mean(tsCODOUTtotalDET)

# STAGIONALITA'
tsCODOUTtotalAGG<-apply.weekly(tsCODOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCODOUTtotalAGG,lag.max = 60, main=expression("COD"[OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)
