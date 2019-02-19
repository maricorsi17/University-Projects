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
ts1_QINtotal<-xts(x=1/Caprino[,6], order.by=datestotal)
tsSSTINOX1total<-na.omit(xts(x=Caprino[,38], order.by=datestotal))

tsCaricoCODINtotal<-(na.omit(xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal)*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


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

plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("SST"[OX]," [g/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,10))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 10, by = 2), las=2,format(seq(from = 0,to = 10,by = 2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo((tsSSTINOX1total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSTINOX1total)),lwd=2)
a<-lm(tsSSTINOX1total~index(tsSSTINOX1total))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=9.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=9.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=9.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=9.5,label="2018")
text(x=index(ts1_QIN2016[181,]),y=1.5, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2017[100,]),y=8.9, c(expression("SST"[OX]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsSSTINOX1totalDET<-tsSSTINOX1total-(ts_trend(tsSSTINOX1total))
mediaDET<-mean(tsSSTINOX1totalDET)

# STAGIONALITA'
tsSSTINOX1totalAGG<-apply.weekly(tsSSTINOX1totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSTINOX1totalAGG,lag.max = 60, main=expression("SST"[OX]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)