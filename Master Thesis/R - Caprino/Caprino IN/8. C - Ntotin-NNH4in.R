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
tsNtotINtotal_original<-na.approx(xts(x=Caprino[,29], order.by=datestotal))
tsNtotINtotal<-na.omit(xts(x=Caprino[,29], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=Caprino[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNtotINtotalNA<-(xts(x=Caprino[,29], order.by=datestotal))


tsNH4INtotal_original<-xts(x=Caprino[,41], order.by=datestotal)
tsNH4INtotal_original[which(tsNH4INtotal_original==0)]=NA
tsNH4INtotal<-na.omit(tsNH4INtotal_original)
MAtsNH4INtotal<-na.omit(rollapply(tsNH4INtotal_original, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNH4INtotalNA<-(tsNH4INtotal_original)


## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsNtotINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsNtotINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsNtotINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsNtotINtotal_original["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse  

plot(as.zoo(tsNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot-IN],", N-NH"[4-IN]^"+"," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,140),lwd=1,col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 140,by = 20),las=2)
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(tsNtotINtotal),col="darkslategrey")
lines(as.zoo(tsNH4INtotal),col="orange")
lines(as.zoo(MAtsNtotINtotal[index(tsNtotINtotal)]))
lines(as.zoo(MAtsNH4INtotal[index(tsNH4INtotal)]),col="red")

abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)
text(x=index(tsNtotIN2015[182,]),y=130,label="2015")
text(x=index(tsNtotIN2016[182,]),y=130,label="2016")
text(x=index(tsNtotIN2017[182,]),y=130,label="2017")
text(x=index(tsNtotIN2018[90,]),y=130,label="2018")
legend(x=index(tsNtotIN2017[220,]),y=125, c(expression(paste("N"[tot-IN])), expression(paste("N-NH"[4-IN]^"+")),expression(paste("MA N"[tot-IN]," (mensile)")), expression(paste("MA N-NH"[4-IN]^"+"," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNtotINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N"[tot-IN])), ylim=c(0,140),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 140,by = 20),las=2,labels = format(seq(from = 0,to = 140,by = 20), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNH4INtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N-NH"[4-IN]^"+")), ylim=c(0,90),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 90,by = 10),las=2,labels = format(seq(from = 0,to = 90,by = 10), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsNtotINtotalNA),coredata(tsNH4INtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNtotINtotal),as.zoo(tsNH4INtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N"[tot-IN]," [mg/L]")),ylab=expression(paste("N-NH"[4-IN]^"+"," [mg/L]")),cex.lab="1.2",xlim=c(0,150),ylim=c(0,100),pch=16)
axis(side=2,at=seq(from = 0,to = 100,by = 50),las=2,format(seq(from = 0,to = 100,by = 50), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 150,by = 50),las=1,format(seq(from = 0,to = 150,by = 50), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=2,col="grey")
abline(lm(tsNH4INtotalNA~tsNtotINtotalNA),lwd=2)

text(x=25,y=75, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))