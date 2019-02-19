# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")

# caricare file excel
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsTINtotal<-na.approx(xts(x=Caprino[,4], order.by=datestotal))
tsTOUTtotal<-na.approx(xts(x=Caprino[,5], order.by=datestotal))
tsTINtotalNA<-(xts(x=Caprino[,4], order.by=datestotal))
tsTOUTtotalNA<-(xts(x=Caprino[,5], order.by=datestotal))

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

plot(as.zoo(tsTINtotal),type="n", xlab="Mesi",ylab=expression(paste("T", " [",degree,"C]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(5,30))
drawTimeAxis(as.zoo(tsTINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 5,to = 30,by = 5),las=2)
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsTINtotal))
lines(as.zoo(tsTOUTtotal),col="red")

abline(v=index(tsTIN2016[1,]),lwd=2)
abline(v=index(tsTIN2017[1,]),lwd=2)
abline(v=index(tsTIN2018[1,]),lwd=2)
text(x=index(tsTIN2015[140,]),y=28,label="2015")
text(x=index(tsTIN2016[140,]),y=28,label="2016")
text(x=index(tsTIN2017[140,]),y=28,label="2017")
text(x=index(tsTIN2018[90,]),y=28,label="2018")
legend(x=index(tsTIN2018[20,]),y= 27, c(expression(paste("T"[IN])),expression(paste("T"[OUT]))),col=c("black","red"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsTOUTtotal),yaxt="n",ylab=expression(paste("[",degree,"C]")),main=expression(paste("T"[OUT])), ylim=c(5,30),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 5,to = 30,by = 5),las=2,labels = format(seq(from = 5,to = 30,by = 5), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsTINtotalNA),coredata(tsTOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsTINtotal),as.zoo(tsTOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("T"[IN]," [",degree,"C]")),ylab=expression(paste("T"[OUT]," [",degree,"C]")),cex.lab="1.2",xlim=c(5,30),ylim=c(5,30),pch=16)
axis(side=2,at=seq(from = 5,to = 30,by = 5),las=2,format(seq(from = 5,to = 30,by = 5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 5,to = 30,by = 5),las=1,format(seq(from = 5,to = 30,by = 5), big.mark = ".", decimal.mark = ","))
grid(nx=5,ny=5,col="grey")
abline(lm(tsTOUTtotalNA~tsTINtotalNA),lwd=2)

text(x=7.5,y=27.5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
