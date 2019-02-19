# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsCODINtotal_original<-na.approx(xts(x=Caprino[,17], order.by=datestotal))
tsCODINtotal<-na.omit(xts(x=Caprino[,17], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCODINtotalNA<-(xts(x=Caprino[,17], order.by=datestotal))


tsCODOUTtotal<-na.omit(xts(x=Caprino[,18], order.by=datestotal))
MAtsCODOUTtotal<-na.omit(rollapply(xts(x=Caprino[,18], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCODOUTtotalNA<-(xts(x=Caprino[,18], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsCODIN2015<-tsCODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsCODIN2016<-tsCODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsCODIN2017<-tsCODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsCODIN2018<-tsCODINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCODINtotal),type="n", xlab="Mesi",ylab="COD [mg/L]",yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,2000),col="grey")
drawTimeAxis(as.zoo(tsCODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 2000,by = 250),las=2,format(seq(from = 0,to = 2000,by = 250), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCODINtotal),col="darkslategrey")
lines(as.zoo(tsCODOUTtotal),col="orange")
lines(as.zoo(MAtsCODINtotal[index(tsCODOUTtotal)]))
lines(as.zoo(MAtsCODOUTtotal[index(tsCODOUTtotal)]),col="red")
abline(h=100,col="blue",lty=2)

abline(v=index(tsCODIN2016[1,]),lwd=2)
abline(v=index(tsCODIN2017[1,]),lwd=2)
abline(v=index(tsCODIN2018[1,]),lwd=2)
text(x=index(tsCODIN2015[182,]),y=1900,label="2015")
text(x=index(tsCODIN2016[182,]),y=1900,label="2016")
text(x=index(tsCODIN2017[182,]),y=1900,label="2017")
text(x=index(tsCODIN2018[90,]),y=1900,label="2018")
legend(x=index(tsCODIN2017[180,]),y=1800, c(expression(paste("COD"[IN])),expression(paste("COD"[OUT])),expression(paste("MA COD"[IN]," (mensile)")),expression(paste("MA COD"[OUT]," (mensile)")),"Limite"),col=c("darkslategrey","orange","black","red","blue"),lty=c(1,1,1,1,2),lwd=c(1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCODOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("COD"[OUT])), ylim=c(0,100),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 100,by = 10),las=2,labels = format(seq(from = 0,to = 100,by = 10), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsCODOUTtotalNA),coredata(tsCODINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCODINtotal),as.zoo(tsCODOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("COD"[IN]," [mg/L]")),ylab=expression(paste("COD"[OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,2000),ylim=c(0,125),pch=16)
axis(side=2,at=seq(from = 0,to = 125,by = 25),las=2,format(seq(from = 0,to = 125,by = 25), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 2000,by = 500),las=1,format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=5,col="grey")
abline(lm(tsCODOUTtotalNA~tsCODINtotalNA),lwd=2)

text(x=250,y=112.5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))