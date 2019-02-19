# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("tsbox")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/NoOutliers_Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsTINtotal<-na.approx(xts(x=Caprino[,4], order.by=datestotal))
tsTOUTtotal<-na.approx(xts(x=Caprino[,5], order.by=datestotal))


## Creare un oggetto con le date (2015)
tsTIN2015<-tsTINtotal["2015"]

## Creare un oggetto con le date (2016)
tsTIN2016<-tsTINtotal["2016"]

## Creare un oggetto con le date (2017)
tsTIN2017<-tsTINtotal["2017"]

## Creare un oggetto con le date (2018)
tsTIN2018<-tsTINtotal["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsTINtotal),type="n", xlab="Mesi",ylab=expression(paste("T"[OUT], " [",degree,"C]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(5,30))
drawTimeAxis(as.zoo(tsTINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 5,to = 30,by = 5),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsTOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsTOUTtotal)),lwd=2)
a<-lm(tsTOUTtotal~index(tsTOUTtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsTIN2016[1,]),lwd=2)
abline(v=index(tsTIN2017[1,]),lwd=2)
abline(v=index(tsTIN2018[1,]),lwd=2)
text(x=index(tsTIN2015[140,]),y=28,label="2015")
text(x=index(tsTIN2016[140,]),y=28,label="2016")
text(x=index(tsTIN2017[140,]),y=28,label="2017")
text(x=index(tsTIN2018[48,]),y=28,label="2018")
text(x=index(tsTIN2016[181,]),y=7, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsTIN2017[330,]),y= 27, c(expression(paste("T"[OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsTOUTtotalDET<-tsTOUTtotal-(ts_trend(tsTOUTtotal))
mediaDET<-mean(tsTOUTtotalDET)

# STAGIONALITA'
tsTOUTtotalAGG<-apply.weekly(tsTOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsTOUTtotalAGG,lag.max = 60, main=expression("T"[OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)
