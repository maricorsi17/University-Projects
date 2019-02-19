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
ts1_QINtotal<-xts(x=1/Caprino[,6], order.by=datestotal)
tsSSTINOX1total<-na.omit(xts(x=Caprino[,38], order.by=datestotal))
tsSSTINOX1totalNA<-(xts(x=Caprino[,38], order.by=datestotal))

tsCaricoCODINtotal<-(na.omit(xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal)*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCaricoCODINtotalNA<-((xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)


## Creare un oggetto con le date (2015)
ts1_QIN2015<-ts1_QINtotal["2015"]
## Creare un oggetto con le date (2016)
ts1_QIN2016<-ts1_QINtotal["2016"]
## Creare un oggetto con le date (2017)
ts1_QIN2017<-ts1_QINtotal["2017"]
## Creare un oggetto con le date (2018)
ts1_QIN2018<-ts1_QINtotal["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,5000))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 5000, by = 1000), las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(MAtsCaricoCODINtotal[index(tsCaricoCODINtotal)]),col="black")

par(new = T)
plot(as.zoo(tsSSTINOX1total), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,10), col="orange")
axis(side=4, at=seq(from=0, to=12, by=2),las=2)
mtext(expression(paste("SST"[OX]," [g/L]")), side=4, line=3, cex = 1.2)
lines(as.zoo((tsSSTINOX1total)),col="orange")

abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=9.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=9.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=9.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=9.5,label="2018")
legend(x=index(ts1_QIN2017[220,]),y=9, c(expression(paste("MA COD"[IN]," (mensile)")),expression("SST"[OX])),col=c("black","orange"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSSTINOX1total),yaxt="n",ylab="[g/L]",main=expression(paste("SST"[OX])), ylim=c(0,650),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 650,by = 50),las=2,labels = format(seq(from = 0,to = 650,by = 50), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsSSTINOX1totalNA),coredata(tsCaricoCODINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTINOX1total),as.zoo(tsCaricoCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[OX]," [g/L]")),ylab=expression(paste("COD"[IN]," [kg/d]")),cex.lab="1.2",xlim=c(0,12),ylim=c(0,5000),pch=16)
axis(side=2,at=seq(from = 0,to = 5000,by = 1000),las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 12,by = 3),las=1,format(seq(from = 0,to = 12,by = 3), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=5,col="grey")
abline(lm(tsCaricoCODINtotalNA~tsSSTINOX1totalNA),lwd=2)

text(x=1.5,y=4500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
