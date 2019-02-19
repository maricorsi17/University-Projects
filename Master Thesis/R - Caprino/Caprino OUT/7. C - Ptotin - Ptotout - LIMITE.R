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
tsPtotINtotal<-(xts(x=Caprino[,27], order.by=datestotal))
tsPtotINtotal[which(tsPtotINtotal>30)]<-NA
tsPtotINtotalNA<-(tsPtotINtotal)
tsPtotINtotal<-na.omit(tsPtotINtotal)
MAtsPtotINtotal<-na.omit(rollapply(xts(x=Caprino[,27], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsPtotOUTtotal_original<-na.approx(xts(x=Caprino[,28], order.by=datestotal))
tsPtotOUTtotal<-na.omit(xts(x=Caprino[,28], order.by=datestotal))
MAtsPtotOUTtotal<-na.omit(rollapply(xts(x=Caprino[,28], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsPtotOUTtotalNA<-(xts(x=Caprino[,28], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsPtotOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsPtotOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsPtotOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsPtotOUTtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,30),col="grey")
drawTimeAxis(as.zoo(tsPtotOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 30,by = 10),las=2)
grid(nx=NA,ny=3,col="grey")
lines(as.zoo(tsPtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsPtotINtotal[index(tsPtotINtotal)]))
lines(as.zoo(tsPtotOUTtotal),col="orange")
lines(as.zoo(MAtsPtotOUTtotal[index(tsPtotOUTtotal)]),col="red")



abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)
abline(h=2,col="blue",lty=2)
text(x=index(tsPtotIN2015[182,]),y=27.5,label="2015")
text(x=index(tsPtotIN2016[182,]),y=27.5,label="2016")
text(x=index(tsPtotIN2017[182,]),y=27.5,label="2017")
text(x=index(tsPtotIN2018[90,]),y=27.5,label="2018")
legend(x=index(tsPtotIN2017[200,]),y=26, c(expression(paste("P"[tot-IN])), expression(paste("P"[tot-OUT])),expression(paste("MA P"[tot-IN]," (mensile)")),expression(paste("MA P"[tot-OUT]," (mensile)")),"Limite"),col=c("darkslategrey","orange","black","red","blue"),lty=c(1,1,1,1,2),lwd=c(1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsPtotOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("P"[tot-OUT])), ylim=c(0,4.5),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 4.5,by = 0.5),las=2,labels = format(seq(from = 0,to = 4.5,by = 0.5), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsPtotINtotalNA),coredata(tsPtotOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsPtotINtotal),as.zoo(tsPtotOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("P"[tot-IN]," [mg/L]")),ylab=expression(paste("P"[tot-OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,20),ylim=c(0,5),pch=16)
axis(side=2,at=seq(from = 0,to = 5,by = 2.5),las=2,format(seq(from = 0,to = 5,by = 2.5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 20,by = 10),las=1,format(seq(from = 0,to = 20,by = 10), big.mark = ".", decimal.mark = ","))
grid(nx=2,ny=2,col="grey")
abline(lm(tsPtotOUTtotalNA~tsPtotINtotalNA),lwd=2)

text(x=5,y=3.75, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
