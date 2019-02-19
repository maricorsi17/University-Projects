# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
ts1_QINtotal<-xts(x=1/SAmbrogio[,6], order.by=datestotal)

tsSSTINOX1total<-na.omit(xts(x=SAmbrogio[,40], order.by=datestotal))
tsSSTINOX2total<-na.omit(xts(x=SAmbrogio[,44], order.by=datestotal))
tsSSTINOX3total<-na.omit(xts(x=SAmbrogio[,48], order.by=datestotal))

tsSSTINOX1totalNA<-(xts(x=SAmbrogio[,40], order.by=datestotal))
tsSSTINOX2totalNA<-(xts(x=SAmbrogio[,44], order.by=datestotal))
tsSSTINOX3totalNA<-(xts(x=SAmbrogio[,48], order.by=datestotal))

tsCaricoCODINtotal<-(na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCaricoCODINtotalNA<-((xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)


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

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,5000))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 5000, by = 1000), las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(MAtsCaricoCODINtotal[index(tsCaricoCODINtotal)]),col="black")
par(new = T)
plot(as.zoo(tsSSTINOX1total), type = "n", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,15), col="orange")
axis(side=4, at=seq(from=0, to=15, by=3),las=2)
mtext(expression(paste("SST"[OX]," [g/L]")), side=4, line=3, cex= 1.2)
lines(as.zoo((tsSSTINOX1total)),col="orange")
lines(as.zoo((tsSSTINOX2total)),col="magenta")
lines(as.zoo((tsSSTINOX3total)),col="red")



abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=14.5,label="2015")
text(x=index(ts1_QIN2016[182,]),y=14.5,label="2016")
text(x=index(ts1_QIN2017[182,]),y=14.5,label="2017")
text(x=index(ts1_QIN2018[90,]),y=14.5,label="2018")
legend(x=index(ts1_QIN2015[30,]),y=13.8, c(expression(paste("MA COD"[IN]," (mensile)")),expression("SST"[OX1]),expression("SST"[OX2]),expression("SST"[OX3])),col=c("black","orange","magenta","red"),lty=c(1,1,1),lwd=c(1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(3,6,4,4),mgp=c(4,1,0))

boxplot(cbind(coredata(xts(x=SAmbrogio[,40], order.by=datestotal)),coredata(xts(x=SAmbrogio[,44], order.by=datestotal)),coredata(xts(x=SAmbrogio[,48], order.by=datestotal))),yaxt="n",ylab="[g/L]",names=c(expression("SST"[OX1]),expression("SST"[OX2]),expression("SST"[OX3])),main=expression(paste("SST"[OX])), ylim=c(0,10),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 10,by = 1),las=2,labels = format(seq(from = 0,to = 10,by = 1), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsCaricoCODINtotalNA),coredata(tsSSTINOX1totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTINOX1total),as.zoo(tsCaricoCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[OX1]," [g/L]")),ylab=expression(paste("COD"[IN]," [kg/d]")),cex.lab="1.2",xlim=c(0,10),ylim=c(0,4000),pch=16)
axis(side=2,at=seq(from = 0,to = 4000,by = 1000),las=2,format(seq(from = 0,to = 4000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 10,by = 2.5),las=1,format(seq(from = 0,to = 10,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsCaricoCODINtotalNA~tsSSTINOX1totalNA),lwd=2)

text(x=1.25,y=3500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsCaricoCODINtotalNA),coredata(tsSSTINOX2totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTINOX2total),as.zoo(tsCaricoCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[OX2]," [g/L]")),ylab=expression(paste("COD"[IN]," [kg/d]")),cex.lab="1.2",xlim=c(0,10),ylim=c(0,4000),pch=16)
axis(side=2,at=seq(from = 0,to = 4000,by = 1000),las=2,format(seq(from = 0,to = 4000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 10,by = 2.5),las=1,format(seq(from = 0,to = 10,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsCaricoCODINtotalNA~tsSSTINOX2totalNA),lwd=2)

text(x=1.25,y=3500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsCaricoCODINtotalNA),coredata(tsSSTINOX3totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSTINOX3total),as.zoo(tsCaricoCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("SST"[OX3]," [g/L]")),ylab=expression(paste("COD"[IN]," [kg/d]")),cex.lab="1.2",xlim=c(0,10),ylim=c(0,4000),pch=16)
axis(side=2,at=seq(from = 0,to = 4000,by = 1000),las=2,format(seq(from = 0,to = 4000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 10,by = 2.5),las=1,format(seq(from = 0,to = 10,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsCaricoCODINtotalNA~tsSSTINOX3totalNA),lwd=2)

text(x=1.25,y=3500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))

