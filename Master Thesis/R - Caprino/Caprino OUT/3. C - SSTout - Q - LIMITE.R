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
tsSSTOUTtotal_original<-na.approx(xts(x=Caprino[,14], order.by=datestotal))
tsSSTOUTtotal<-na.omit(xts(x=Caprino[,14], order.by=datestotal))
MAtsSSTOUTtotal<-na.omit(rollapply(xts(x=Caprino[,14], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsSSTINtotal<-na.omit(xts(x=Caprino[,13], order.by=datestotal))
tsSSTOUTtotalNA<-(xts(x=Caprino[,14], order.by=datestotal))
tsSSTINtotalNA<-(xts(x=Caprino[,13], order.by=datestotal))


tsQOUTtotal<-na.omit(xts(x=Caprino[,6], order.by=datestotal))
MAtsQOUTtotal<-na.omit(rollapply(xts(x=Caprino[,6], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsQOUTtotalNA<-(xts(x=Caprino[,6], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsSSTOUT2015<-tsSSTOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsSSTOUT2016<-tsSSTOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsSSTOUT2017<-tsSSTOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsSSTOUT2018<-tsSSTOUTtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTOUTtotal),type="n", xlab="Mesi",ylab=expression(paste("SST"[OUT]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,40),col="grey")
drawTimeAxis(as.zoo(tsSSTOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 40,by = 5),las=2)
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsSSTOUTtotal),col="darkslategrey")
lines(as.zoo(MAtsSSTOUTtotal[index(tsSSTOUTtotal)]))
abline(h=35,col="blue",lty=2)

par(new = TRUE) #aggiungere grafico con secondo asse y
plot(as.zoo(tsQOUTtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,5000), col="orange")
lines(as.zoo(MAtsQOUTtotal[index(tsQOUTtotal)]),col="red")
axis(side=4, at=seq(from=0, to=5000, by=625),las=2,format(seq(from = 0,to = 5000,by = 625), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("Q"[OUT]," [m"^{3},"/d]")), side=4, line=4,cex=1.2)

abline(v=index(tsSSTOUT2016[1,]),lwd=2)
abline(v=index(tsSSTOUT2017[1,]),lwd=2)
abline(v=index(tsSSTOUT2018[1,]),lwd=2)
text(x=index(tsSSTOUT2015[182,]),y=4690,label="2015")
text(x=index(tsSSTOUT2016[182,]),y=4690,label="2016")
text(x=index(tsSSTOUT2017[182,]),y=4690,label="2017")
text(x=index(tsSSTOUT2018[90,]),y=4690,label="2018")
legend(x=index(tsSSTOUT2017[180,]),y=4200, c(expression(paste("SST"[OUT])),expression(paste("Q"[OUT])),expression(paste("MA SST"[OUT]," (mensile)")),expression(paste("MA Q"[OUT]," (mensile)")),"Limite"),col=c("darkslategrey","orange","black","red","blue"),lty=c(1,1,1,1,2),lwd=c(1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSSTOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("SST"[OUT])), ylim=c(0,35),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 35,by = 5),las=2,labels = format(seq(from = 0,to = 35,by = 5), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsSSTOUTtotalNA),coredata(tsQOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTOUTtotal),as.zoo(tsQOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[OUT]," [mg/L]")),ylab=expression(paste("Q [m"^"3","/d]")),cex.lab="1.2",xlim=c(0,40),ylim=c(0,5000),pch=16)
axis(side=2,at=seq(from = 0,to = 5000,by = 1000),las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 40,by = 10),las=1,format(seq(from = 0,to = 40,by = 10), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=5,col="grey")
abline(lm(tsQOUTtotalNA~tsSSTOUTtotalNA),lwd=2)

text(x=5,y=4500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsSSTINtotalNA),coredata(tsSSTOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTINtotal),as.zoo(tsSSTOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[IN]," [mg/L]")),ylab=expression(paste("SST"[OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,1500),ylim=c(0,40),pch=16)
axis(side=2,at=seq(from = 0,to = 40,by = 10),las=2,format(seq(from = 0,to = 40,by = 10), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1500,by = 500),las=1,format(seq(from = 0,to = 1500,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=4,col="grey")
abline(lm(tsSSTOUTtotalNA~tsSSTINtotalNA),lwd=2)

text(x=250,y=35, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))