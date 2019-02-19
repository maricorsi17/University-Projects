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
ts1_QINtotal<-xts(x=1/SAmbrogio[,6], order.by=datestotal)
tsSSTINOX1total_spazi<-na.approx(xts(x=SAmbrogio[,40], order.by=datestotal))

tsSSTINOX1total<-na.omit(xts(x=SAmbrogio[,40], order.by=datestotal))
tsSSTINOX2total<-na.omit(xts(x=SAmbrogio[,44], order.by=datestotal))
tsSSTINOX3total<-na.omit(xts(x=SAmbrogio[,48], order.by=datestotal))

tsCaricoCODINtotal<-(na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


## Creare un oggetto con le date (2015)
ts1_QIN2015<-ts1_QINtotal["2015"]
## Creare un oggetto con le date (2016)
ts1_QIN2016<-ts1_QINtotal["2016"]
## Creare un oggetto con le date (2017)
ts1_QIN2017<-ts1_QINtotal["2017"]
## Creare un oggetto con le date (2018)
ts1_QIN2018<-ts1_QINtotal["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(tsSSTINOX1total), type = "n", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="Mesi", ylab=expression(paste("SST"[OX1]," [g/L]")), cex.lab=1.2,ylim=c(0,15), col="orange")
drawTimeAxis(as.zoo(tsSSTINOX1total_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from=0, to=15, by=3),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo((tsSSTINOX1total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINOX1total)),lwd=2)
a<-lm(tsSSTINOX1total~index(tsSSTINOX1total))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=14.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=14.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=14.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=14.5,label="2018")
text(x=index(ts1_QIN2016[181,]),y=7.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2015[30,]),y=13.8, c(expression("SST"[OX1]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(tsSSTINOX2total), type = "n", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="Mesi", ylab=expression(paste("SST"[OX2]," [g/L]")),cex.lab=1.2, ylim=c(0,15), col="orange")
drawTimeAxis(as.zoo(tsSSTINOX1total_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from=0, to=15, by=3),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo((tsSSTINOX2total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINOX2total)),lwd=2)
a1<-lm(tsSSTINOX2total~index(tsSSTINOX2total))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100

abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=14.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=14.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=14.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=14.5,label="2018")
text(x=index(ts1_QIN2016[181,]),y=7.5, label=paste("Variazione = ",format(round(perc_a1,digits=1),nsmall=1,decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2015[30,]),y=13.8, c(expression("SST"[OX2]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")





windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(tsSSTINOX3total), type = "n", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="Mesi", ylab=expression(paste("SST"[OX3]," [g/L]")),cex.lab=1.2, ylim=c(0,15), col="orange")
drawTimeAxis(as.zoo(tsSSTINOX1total_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from=0, to=15, by=3),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo((tsSSTINOX3total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINOX3total)),lwd=2)
a2<-lm(tsSSTINOX3total~index(tsSSTINOX3total))
abline(a2,col="red",lwd=2,lty=5)

perc_a2<--(1-coredata(a2$fitted.values[length(a2$fitted.values)])/coredata(a2$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=14.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=14.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=14.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=14.5,label="2018")
text(x=index(ts1_QIN2016[181,]),y=7.5, label=paste("Variazione = ",format(round(perc_a2,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2015[30,]),y=13.8, c(expression("SST"[OX3]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
tsSSTINOX1totalDET<-tsSSTINOX1total-(ts_trend(tsSSTINOX1total))
mediaSSTOX1DET<-mean(tsSSTINOX1totalDET)

tsSSTINOX2totalDET<-tsSSTINOX2total-(ts_trend(tsSSTINOX2total))
mediaSSTOX2DET<-mean(tsSSTINOX2totalDET)

tsSSTINOX3totalDET<-tsSSTINOX3total-(ts_trend(tsSSTINOX3total))
mediaSSTOX3DET<-mean(tsSSTINOX3totalDET)

# STAGIONALITA'
tsSSTINOX1totalAGG<-apply.weekly(tsSSTINOX1totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINOX1totalAGG,lag.max = 60, main=expression("SST"[OX1]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSTINOX2totalAGG<-apply.weekly(tsSSTINOX2totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINOX2totalAGG,lag.max = 60, main=expression("SST"[OX2]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSTINOX3totalAGG<-apply.weekly(tsSSTINOX3totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINOX3totalAGG,lag.max = 60, main=expression("SST"[OX3]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

