# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQINtotal<-xts(x=SAmbrogio[,6], order.by=datestotal)
tsQOUTtotal<-xts(x=SAmbrogio[,8],order.by=datestotal)

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

plot(as.zoo(tsQINtotal),type="n",xlab="Mesi",ylab=expression(paste("Q [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,14000), lwd=2,col="blue")
drawTimeAxis(as.zoo(tsQINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 14000,by = 1000),las=2,labels = format(seq(from = 0,to = 14000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=14,col="grey")
lines(as.zoo(tsQINtotal),col="blue",lwd=2)
lines(as.zoo(tsQOUTtotal),col="orange")



abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=13500,label="2015")
text(x=index(tsQIN2016[182,]),y=13500,label="2016")
text(x=index(tsQIN2017[182,]),y=13500,label="2017")
text(x=index(tsQIN2018[90,]),y=13500,label="2018")
legend(x=index(tsQIN2018[30,]),y=12100, c(expression("Q"[IN]),expression("Q"[OUT])),col=c("blue","orange"),lty=c(1,1),lwd=c(2,1),bg="white")
