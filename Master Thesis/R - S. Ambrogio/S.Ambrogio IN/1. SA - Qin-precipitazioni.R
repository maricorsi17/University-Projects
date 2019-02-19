# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("scales")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")
Prec_SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/Precipitazioni_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQINtotal<-xts(x=SAmbrogio[,6], order.by=datestotal)
MAtsQINtotal<-rollmean(tsQINtotal,k=90)

tsPrecipitazioni<-xts(x=Prec_SAmbrogio[,1],order.by=datestotal)
prec_mese<-apply.monthly(tsPrecipitazioni, sum)

tsBypass<-xts(x=SAmbrogio[,7],order.by=datestotal)
bypass_mese<-apply.monthly(tsBypass, function(x) length(which(!is.na(x))))

## Creare un oggetto con le date (2015)
tsQIN2015<-tsQINtotal["2015"]

## Creare un oggetto con le date (2016)
tsQIN2016<-tsQINtotal["2016"]

## Creare un oggetto con le date (2017)
tsQIN2017<-tsQINtotal["2017"]

## Creare un oggetto con le date (2018)
tsQIN2018<-tsQINtotal["2018"]


tsQIN2016.2018<-rbind.xts(tsQIN2016,tsQIN2017,tsQIN2018)

# Calcolare la media e la mediana di tutti i campioni

mediatotale<-mean(tsQINtotal)
media2015<-mean(tsQIN2015)
media2016<-mean(tsQIN2016)
media2017<-mean(tsQIN2017)
media2018<-mean(tsQIN2018)
media2016.2018<-mean(tsQIN2016.2018)
mediana2015<-median(tsQIN2015)
mediana2016<-median(tsQIN2016)
mediana2017<-median(tsQIN2017)
mediana2018<-median(tsQIN2018)
mediana2016.2018<-median(tsQIN2016.2018)
rapporto2015<-media2015/mediana2015
rapporto2016<-media2016/mediana2016
rapporto2017<-media2017/mediana2017
rapporto2018<-media2018/mediana2018
rapporto2016.2018<-media2016.2018/mediana2016.2018


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse



barplot(as.zoo(prec_mese),yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,180),col="lightgrey",border="lightgrey")
grid(nx=NA,ny=9,col="grey")
#barplot(as.zoo(prec_mese),yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,180),col="lightgrey",border="lightgrey")

axis(side=4, at=seq(from=0, to=180, by=20),las=2)
mtext("Precipitazioni mensili [mm]", side=4, line=3,cex=1.2)

par(new = TRUE) #aggiungere grafico con secondo asse y
plot(as.zoo(tsQINtotal),type = "n",xlab="Mesi",ylab=expression(paste("Q"[IN]," [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,9000))
axis(side=2,at=seq(from = 0,to = 9000,by = 1000),las=2,labels = format(seq(from = 0,to = 9000,by = 1000), big.mark = ".", decimal.mark = ","))
drawTimeAxis(as.zoo(tsQINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
grid(nx=NA,ny=9,col="grey")
lines(as.zoo(tsQINtotal))
#lines(as.zoo(MAtsQINtotal))

#abline(h=mean(tsQINtotal),col="red",lwd=2)
ablineclip(h=mean(tsQIN2015),x1=index(tsQIN2015[1,]),x2=index(tsQIN2015[365,]),col="blue",lwd=2)
ablineclip(h=mean(tsQIN2016),x1=index(tsQIN2016[1,]),x2=index(tsQIN2016[366,]),col="blue",lwd=2)
ablineclip(h=mean(tsQIN2017),x1=index(tsQIN2017[1,]),x2=index(tsQIN2017[365,]),col="blue",lwd=2)
ablineclip(h=mean(tsQIN2018),x1=index(tsQIN2018[1,]),x2=index(tsQIN2018[181,]),col="blue",lwd=2)
abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=8500,label="2015")
text(x=index(tsQIN2016[182,]),y=8500,label="2016")
text(x=index(tsQIN2017[182,]),y=8500,label="2017")
text(x=index(tsQIN2018[90,]),y=8500,label="2018")
legend(x=index(tsQIN2017[20,]),y=8100, c(expression("Q"[IN]),expression("Media annua Q"[IN]),"Precipitazioni mensili"),col=c("black","blue","lightgrey"),lty=c(1,1,NA),lwd=c(1,2),pch=c(NA,NA,22),pt.bg="lightgrey",pt.cex = 2,bg="white")


# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsQIN2016.2018),yaxt="n",ylab=expression(paste("[m"^"3","/d]")),main=expression(paste("Q"[IN])), ylim=c(0,9000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 9000,by = 1000),las=2,labels = format(seq(from = 0,to = 9000,by = 1000), big.mark = ".", decimal.mark = ","))