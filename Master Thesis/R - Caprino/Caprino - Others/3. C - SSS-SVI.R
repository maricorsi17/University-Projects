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

tsSSSINOXtotal<-na.approx(xts(x=Caprino[,37], order.by=datestotal))
tsSVIINOXtotal<-na.approx(xts(x=Caprino[,39], order.by=datestotal))
tsSSSINOXtotalNA<-(xts(x=Caprino[,37], order.by=datestotal))
tsSVIINOXtotalNA<-(xts(x=Caprino[,39], order.by=datestotal))

## Creare un oggetto con le date (2015)
tsSSSINOX2015<-tsSSSINOXtotal["2015"]
## Creare un oggetto con le date (2016)
tsSSSINOX2016<-tsSSSINOXtotal["2016"]
## Creare un oggetto con le date (2017)
tsSSSINOX2017<-tsSSSINOXtotal["2017"]
## Creare un oggetto con le date (2018)
tsSSSINOX2018<-tsSSSINOXtotal["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOXtotal), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(300,1100), type="n")
drawTimeAxis(as.zoo(tsSSSINOXtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 300, to = 1100, by = 100), las=2,format(seq(from = 300,to = 1100,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo((tsSSSINOXtotal)),col="black")

par(new = T)
plot(as.zoo((tsSVIINOXtotal)), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,400), col="orange")
axis(side=4, at=seq(from=0, to=400, by=50),las=2)
mtext(expression(paste("SVI"," [mL/g]")), side=4, line=3, cex = 1.2)

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
legend(x=index(tsSSSINOX2016[260,]),y=360, c(expression("Volume del fango"[OX]),expression("SVI")),col=c("black", "orange"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSSSINOXtotal),yaxt="n",ylab="[mL/L]",main=expression(paste("SSS"[OX])), ylim=c(0,1100),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 1100,by = 100),las=2,labels = format(seq(from = 0,to = 1100,by = 100), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSVIINOXtotal),yaxt="n",ylab="[mL/g]",main=expression(paste("SVI")), ylim=c(0,240),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 240,by = 20),las=2,labels = format(seq(from = 0,to = 240,by = 20), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsSVIINOXtotalNA),coredata(tsSSSINOXtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSSINOXtotal),as.zoo(tsSVIINOXtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("Volume del fango"[OX]," [mL/L]")),ylab=expression(paste("SVI [mL/g]")),cex.lab="1.2",xlim=c(0,1200),ylim=c(0,400),pch=16)
axis(side=2,at=seq(from = 0,to = 400,by = 100),las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1200,by = 300),las=1,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsSVIINOXtotalNA~tsSSSINOXtotalNA),lwd=2)

text(x=150,y=350, label=paste("r = ",format(round(corr$estimate,digits=2),nsmall=2,decimal.mark=",")))
