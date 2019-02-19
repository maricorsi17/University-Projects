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
tspHINtotal_original<-na.approx(xts(x=SAmbrogio[,2], order.by=datestotal))
tspHINtotal<-na.omit(xts(x=SAmbrogio[,2], order.by=datestotal))
tspHINtotal[1]=NA

## Creare un oggetto con le date (2015)
tspHIN2015<-tspHINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tspHIN2016<-tspHINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tspHIN2017<-tspHINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tspHIN2018<-tspHINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(OutDec = ",")
plot(as.zoo(tspHINtotal),type="n", xlab="Mesi",ylab=expression(paste("pH"[IN])),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(6,9))
drawTimeAxis(as.zoo(tspHINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 6,to = 9,by = 0.5),las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tspHINtotal))


abline(v=index(tspHIN2016[1,]),lwd=2)
abline(v=index(tspHIN2017[1,]),lwd=2)
abline(v=index(tspHIN2018[1,]),lwd=2)
text(x=index(tspHIN2015[182,]),y=8.75,label="2015")
text(x=index(tspHIN2016[182,]),y=8.75,label="2016")
text(x=index(tspHIN2017[182,]),y=8.75,label="2017")
text(x=index(tspHIN2018[90,]),y=8.75,label="2018")


# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tspHINtotal),yaxt="n",main=expression(paste("pH"[IN])), ylim=c(7,8.5),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 7,to = 8.5,by = 0.5),las=2,labels = format(seq(from = 7,to = 8.5,by = 0.5), big.mark = ".", decimal.mark = ","))