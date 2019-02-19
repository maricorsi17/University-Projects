# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsSSTINtotal_original<-na.approx(xts(x=SAmbrogio[,15], order.by=datestotal))

tsCaricoSSTINtotal<-(na.omit(xts(x=SAmbrogio[,15], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoSSTINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,15], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




## Creare un oggetto con le date (2015)
tsSSTIN2015<-tsSSTINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsSSTIN2016<-tsSSTINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsSSTIN2017<-tsSSTINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsSSTIN2018<-tsSSTINtotal_original["2018"]


# Plottare la time series 
windows(width = 10,height = 8)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoSSTINtotal),type="n",xlab="Mesi",ylab=expression(paste("SST"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,3500),col="grey")
drawTimeAxis(as.zoo(tsSSTINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 3500,by = 500),las=2)
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tsCaricoSSTINtotal),col="grey")
lines(as.zoo(MAtsCaricoSSTINtotal[index(tsCaricoSSTINtotal)]))


abline(v=index(tsSSTIN2016[1,]),lwd=0.3,lty="dotted",col="grey")
abline(v=index(tsSSTIN2017[1,]),lwd=0.3,lty="dotted", col="grey")
abline(v=index(tsSSTIN2018[1,]),lwd=0.3,lty="dotted", col="grey")
text(x=index(tsSSTIN2015[182,]),y=3350,label="2015")
text(x=index(tsSSTIN2016[182,]),y=3350,label="2016")
text(x=index(tsSSTIN2017[182,]),y=3350,label="2017")
text(x=index(tsSSTIN2018[90,]),y=3350,label="2018")
legend(x=index(tsSSTIN2016[90,]),y=3200, c(expression(paste("SST"[IN])),expression(paste("MA SST"[IN]," (mensile)"))),col=c("grey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

