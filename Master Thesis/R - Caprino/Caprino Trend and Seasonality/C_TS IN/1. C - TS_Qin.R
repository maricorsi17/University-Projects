library("xts")
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
tsQINtotal_original<-xts(x=Caprino[,6], order.by=datestotal)


tsQINtotal<-na.approx(tsQINtotal_original)

## Creare un oggetto con le date (2015)
tsQIN2015<-tsQINtotal["2015"]

## Creare un oggetto con le date (2016)
tsQIN2016<-tsQINtotal["2016"]

## Creare un oggetto con le date (2017)
tsQIN2017<-tsQINtotal["2017"]

## Creare un oggetto con le date (2018)
tsQIN2018<-tsQINtotal["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse


plot(as.zoo(tsQINtotal),type="n", xlab="Mesi",ylab=expression(paste("Q"[IN]," [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,5500))
drawTimeAxis(as.zoo(tsQINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 5500,by = 500),las=2,format(seq(from = 0,to = 5500,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=11,col="grey")
lines(as.zoo(tsQINtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tsQINtotal)),lwd=2)
a<-lm(tsQINtotal~index(tsQINtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=5250,label="2015")
text(x=index(tsQIN2016[182,]),y=5250,label="2016")
text(x=index(tsQIN2017[182,]),y=5250,label="2017")
text(x=index(tsQIN2018[90,]),y=5250,label="2018")
text(x=index(tsQIN2016[181,]),y=250, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsQIN2015[30,]),y=4900, c(expression("Q"[IN]),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),pt.bg="lightgrey",pt.cex = 2,bg="white")

# DETRENDING
tsQINtotalDET<-tsQINtotal-(ts_trend(tsQINtotal))
mediaDET<-mean(tsQINtotalDET)

# STAGIONALITA'
tsQINtotalAGG<-apply.weekly(tsQINtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsQINtotalAGG,lag.max = 60, main=expression("Q"[IN]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)