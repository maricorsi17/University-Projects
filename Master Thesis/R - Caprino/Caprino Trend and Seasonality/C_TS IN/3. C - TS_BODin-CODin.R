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


tsCODINtotal<-na.omit(xts(x=Caprino[,17], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


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

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,2000),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 2000,by = 200),las=2,format(seq(from = 0,to = 2000,by = 200), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=10,col="grey")
lines(as.zoo(tsBODINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsBODINtotal)),lwd=2)
a<-lm(tsBODINtotal~index(tsBODINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=1900,label="2015")
text(x=index(tsBODIN2016[182,]),y=1900,label="2016")
text(x=index(tsBODIN2017[182,]),y=1900,label="2017")
text(x=index(tsBODIN2018[90,]),y=1900,label="2018")
text(x=index(tsBODIN2016[181,]),y=900, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2017[120,]),y=1750, c(expression(paste("BOD"[5-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("COD"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,5000),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 5000,by = 500),las=2,format(seq(from = 0,to = 5000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=10,col="grey")
lines(as.zoo(tsCODINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsCODINtotal)),lwd=2)
a1<-lm(tsCODINtotal~index(tsCODINtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=4750,label="2015")
text(x=index(tsBODIN2016[182,]),y=4750,label="2016")
text(x=index(tsBODIN2017[182,]),y=4750,label="2017")
text(x=index(tsBODIN2018[90,]),y=4750,label="2018")
text(x=index(tsBODIN2016[181,]),y=3250, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2017[120,]),y=4390, c(expression(paste("COD"[IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsBODINtotalDET<-tsBODINtotal-(ts_trend(tsBODINtotal))
mediaBODinDET<-mean(tsBODINtotalDET)

tsCODINtotalDET<-tsCODINtotal-(ts_trend(tsCODINtotal))
mediaCODinDET<-mean(tsCODINtotalDET)

# STAGIONALITA'
tsBODINtotalAGG<-apply.weekly(tsBODINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsBODINtotalAGG,lag.max =60, main=expression("BOD"[5-IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)


tsCODINtotalAGG<-apply.weekly(tsCODINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCODINtotalAGG,lag.max =60, main=expression("COD"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)