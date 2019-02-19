# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("tsbox")


# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tspHINtotal_original<-na.approx(xts(x=SAmbrogio[,2], order.by=datestotal))
tspHINtotal<-na.omit(xts(x=SAmbrogio[,2], order.by=datestotal))
tspHINtotal[1]=NA
tspHOUTtotal<-na.omit(xts(x=SAmbrogio[,3], order.by=datestotal))
tspHOUTtotal[1]=NA


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

plot(as.zoo(tspHINtotal),type="n", xlab="Mesi",ylab=expression("pH"[OUT]),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(6,9.5))
drawTimeAxis(as.zoo(tspHINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 6,to = 9.5,by = 0.5),las=2,format(seq(from = 6,to = 9.5,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tspHOUTtotal),col="darkslategrey")
lines(as.zoo(ts_trend(tspHOUTtotal)),lwd=2)
a<-lm(tspHOUTtotal~index(tspHOUTtotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tspHIN2016[1,]),lwd=2)
abline(v=index(tspHIN2017[1,]),lwd=2)
abline(v=index(tspHIN2018[1,]),lwd=2)
text(x=index(tspHIN2015[182,]),y=9.25,label="2015")
text(x=index(tspHIN2016[182,]),y=9.25,label="2016")
text(x=index(tspHIN2017[182,]),y=9.25,label="2017")
text(x=index(tspHIN2018[90,]),y=9.25,label="2018")
text(x=index(tspHIN2016[181,]),y=6.75, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tspHIN2015[30,]),y= 8.95, c(expression(paste("pH"[OUT])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tspHOUTtotalDET<-tspHOUTtotal-(ts_trend(tspHOUTtotal))
mediaDET<-mean(tspHOUTtotalDET)

# STAGIONALITA'
tspHOUTtotalAGG<-apply.weekly(tspHOUTtotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tspHOUTtotalAGG,lag.max = 60, main=expression("pH"[OUT]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)
