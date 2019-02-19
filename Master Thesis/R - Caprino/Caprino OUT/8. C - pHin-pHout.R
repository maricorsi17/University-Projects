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
tspHINtotal<-na.approx(xts(x=Caprino[,2], order.by=datestotal))
tspHOUTtotal<-na.approx(xts(x=Caprino[,3], order.by=datestotal))
tspHINtotalNA<-(xts(x=Caprino[,2], order.by=datestotal))
tspHOUTtotalNA<-(xts(x=Caprino[,3], order.by=datestotal))

## Creare un oggetto con le date (2015)
tspHIN2015<-tspHINtotal["2015"]

## Creare un oggetto con le date (2016)
tspHIN2016<-tspHINtotal["2016"]

## Creare un oggetto con le date (2017)
tspHIN2017<-tspHINtotal["2017"]

## Creare un oggetto con le date (2018)
tspHIN2018<-tspHINtotal["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tspHINtotal),type="n", xlab="Mesi",ylab="pH",yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(6,9.5))
drawTimeAxis(as.zoo(tspHINtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 6,to = 9.5,by = 0.5),las=2,format(seq(from = 6,to = 9.5,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tspHINtotal))
lines(as.zoo(tspHOUTtotal),col="red")


abline(v=index(tspHIN2016[1,]),lwd=2)
abline(v=index(tspHIN2017[1,]),lwd=2)
abline(v=index(tspHIN2018[1,]),lwd=2)
text(x=index(tspHIN2015[182,]),y=9.25,label="2015")
text(x=index(tspHIN2016[182,]),y=9.25,label="2016")
text(x=index(tspHIN2017[182,]),y=9.25,label="2017")
text(x=index(tspHIN2018[90,]),y=9.25,label="2018")
legend(x=index(tspHIN2018[20,]),y= 8.95, c(expression(paste("pH"[IN])),expression(paste("pH"[OUT]))),col=c("black","red"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tspHOUTtotal),yaxt="n",main=expression(paste("pH"[OUT])), ylim=c(6.5,9.5),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 6.5,to = 9.5,by = 0.5),las=2,labels = format(seq(from = 6.5,to = 9.5,by = 0.5), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tspHINtotalNA),coredata(tspHOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tspHINtotal),as.zoo(tspHOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("pH"[IN])),ylab=expression(paste("pH"[OUT])),cex.lab="1.2",xlim=c(6.5,9.5),ylim=c(7,9),pch=16)
axis(side=2,at=seq(from = 7,to = 9,by = 0.5),las=2,format(seq(from = 7,to = 9,by = 0.5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 6.5,to = 9.5,by = 0.5),las=1,format(seq(from = 6.5,to = 9.5,by = 0.5), big.mark = ".", decimal.mark = ","))
grid(nx=6,ny=4,col="grey")
abline(lm(tspHOUTtotalNA~tspHINtotalNA),lwd=2)

text(x=6.75,y=8.75, label=paste("r = ",format(round(corr$estimate,digits=2),nsmall=2,decimal.mark=",")))
