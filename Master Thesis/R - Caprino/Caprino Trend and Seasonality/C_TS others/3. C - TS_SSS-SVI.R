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

tsSSSINOXtotal<-na.approx(xts(x=Caprino[,37], order.by=datestotal))
tsSVIINOXtotal<-na.approx(xts(x=Caprino[,39], order.by=datestotal))

## Creare un oggetto con le date (2015)
tsSSSINOX2015<-tsSSSINOXtotal["2015"]
## Creare un oggetto con le date (2016)
tsSSSINOX2016<-tsSSSINOXtotal["2016"]
## Creare un oggetto con le date (2017)
tsSSSINOX2017<-tsSSSINOXtotal["2017"]
## Creare un oggetto con le date (2018)
tsSSSINOX2018<-tsSSSINOXtotal["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(300,1100), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 300, to = 1100, by = 100), las=2,format(seq(from = 300,to = 1100,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo((tsSSSINOXtotal)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSSINOXtotal)),lwd=2)
a<-lm(tsSSSINOXtotal~index(tsSSSINOXtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=1050,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=1050,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=1050,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=1050,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=350, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2016[260,]),y=950, c(expression("Volume del fango"[OX]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")





windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("SVI [mL/g]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,300), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 300, by = 50), las=2,format(seq(from = 0,to = 300,by = 50), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo((tsSVIINOXtotal)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSVIINOXtotal)),lwd=2)
a1<-lm(tsSVIINOXtotal~index(tsSVIINOXtotal))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=280,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=280,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=280,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=280,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=40, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[100,]),y=260, c(expression("SVI"),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsSSSINOXtotalDET<-tsSSSINOXtotal-(ts_trend(tsSSSINOXtotal))
mediaSSSDET<-mean(tsSSSINOXtotalDET)

tsSVIINOXtotalDET<-tsSVIINOXtotal-(ts_trend(tsSVIINOXtotal))
mediaSVIDET<-mean(tsSVIINOXtotalDET)

# STAGIONALITA'
tsSSSINOXtotalAGG<-apply.weekly(tsSSSINOXtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSSINOXtotalAGG,lag.max = 60, main=expression("Volume del fango"[OX]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSVIINOXtotalAGG<-apply.weekly(tsSVIINOXtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSVIINOXtotalAGG,lag.max = 60, main=expression("SVI"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)