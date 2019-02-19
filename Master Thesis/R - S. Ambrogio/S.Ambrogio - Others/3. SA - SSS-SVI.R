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

tsSSSINOX1total<-na.approx(xts(x=SAmbrogio[,39], order.by=datestotal))
tsSVIINOX1total<-na.approx(xts(x=SAmbrogio[,41], order.by=datestotal))
tsSSSINOX2total<-na.approx(xts(x=SAmbrogio[,43], order.by=datestotal))
tsSVIINOX2total<-na.approx(xts(x=SAmbrogio[,45], order.by=datestotal))
tsSSSINOX3total<-na.approx(xts(x=SAmbrogio[,47], order.by=datestotal))
tsSVIINOX3total<-na.approx(xts(x=SAmbrogio[,49], order.by=datestotal))

tsSSSINOX1totalNA<-(xts(x=SAmbrogio[,39], order.by=datestotal))
tsSVIINOX1totalNA<-(xts(x=SAmbrogio[,41], order.by=datestotal))
tsSSSINOX2totalNA<-(xts(x=SAmbrogio[,43], order.by=datestotal))
tsSVIINOX2totalNA<-(xts(x=SAmbrogio[,45], order.by=datestotal))
tsSSSINOX3totalNA<-(xts(x=SAmbrogio[,47], order.by=datestotal))
tsSVIINOX3totalNA<-(xts(x=SAmbrogio[,49], order.by=datestotal))

## Creare un oggetto con le date (2015)
tsSSSINOX2015<-tsSSSINOX1total["2015"]
## Creare un oggetto con le date (2016)
tsSSSINOX2016<-tsSSSINOX1total["2016"]
## Creare un oggetto con le date (2017)
tsSSSINOX2017<-tsSSSINOX1total["2017"]
## Creare un oggetto con le date (2018)
tsSSSINOX2018<-tsSSSINOX1total["2018"]


# Plottare la time series
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOX1total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX1], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1200), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1200, by = 300), las=2,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSSSINOX1total)),col="black")

par(new = T)
plot(as.zoo((tsSVIINOX1total)), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,400), col="orange")
axis(side=4, at=seq(from=0, to=400, by=100),las=2)
mtext(expression(paste("SVI"[1]," [mL/g]")), side=4, line=3, cex = 1.2)

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
legend(x=index(tsSSSINOX2017[200,]),y=360, c(expression("Volume del fango"[OX]),expression("SVI")),col=c("black", "orange"),lty=c(1,1),lwd=c(1,1),bg="white")

windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0))

plot(as.zoo(tsSSSINOX2total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX2], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1200), type="n")
drawTimeAxis(as.zoo(tsSSSINOX2total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1200, by = 300), las=2,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSSSINOX2total)),col="black")

par(new = T)
plot(as.zoo((tsSVIINOX2total)), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,400), col="orange")
axis(side=4, at=seq(from=0, to=400, by=100),las=2)
mtext(expression(paste("SVI"[2]," [mL/g]")), side=4, line=3, cex = 1.2)

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
legend(x=index(tsSSSINOX2017[200,]),y=360, c(expression("Volume del fango"[OX]),expression("SVI")),col=c("black", "orange"),lty=c(1,1),lwd=c(1,1),bg="white")


windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0))

plot(as.zoo(tsSSSINOX3total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX3], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1200), type="n")
drawTimeAxis(as.zoo(tsSSSINOX3total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1200, by = 300), las=2,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSSSINOX3total)),col="black")

par(new = T)
plot(as.zoo((tsSVIINOX3total)), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,400), col="orange")
axis(side=4, at=seq(from=0, to=400, by=100),las=2)
mtext(expression(paste("SVI"[3]," [mL/g]")), side=4, line=3, cex = 1.2)

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
legend(x=index(tsSSSINOX2017[120,]),y=360, c(expression("Volume del fango"[OX]),expression("SVI")),col=c("black", "orange"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(3,6,4,4),mgp=c(4,1,0))

boxplot(cbind(coredata(xts(x=SAmbrogio[,39], order.by=datestotal)),coredata(xts(x=SAmbrogio[,43], order.by=datestotal)),coredata(xts(x=SAmbrogio[,47], order.by=datestotal))),yaxt="n",ylab="[mL/L]",names=c(expression("SSS"[OX1]),expression("SSS"[OX2]),expression("SSS"[OX3])),main=expression(paste("SSS"[OX])), ylim=c(0,1300),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 1300,by = 100),las=2,labels = format(seq(from = 0,to = 1300,by = 100), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(3,6,4,4),mgp=c(4,1,0))

boxplot(cbind(coredata(xts(x=SAmbrogio[,41], order.by=datestotal)),coredata(xts(x=SAmbrogio[,45], order.by=datestotal)),coredata(xts(x=SAmbrogio[,49], order.by=datestotal))),yaxt="n",ylab="[mL/g]",names=c(expression("SVI"[1]),expression("SVI"[2]),expression("SVI"[3])),main=expression(paste("SVI")), ylim=c(0,3200),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 3200,by = 200),las=2,labels = format(seq(from = 0,to = 3200,by = 200), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsSSSINOX1totalNA),coredata(tsSVIINOX1totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSSINOX1total),as.zoo(tsSVIINOX1total),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("Volume del fango"[OX1]," [mL/L]")),ylab=expression(paste("SVI"[1]," [mL/g]")),cex.lab="1.2",xlim=c(0,1200),ylim=c(0,400),pch=16)
axis(side=2,at=seq(from = 0,to = 400,by = 100),las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1200,by = 300),las=1,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsSVIINOX1totalNA~tsSSSINOX1totalNA),lwd=2)

text(x=150,y=350, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsSSSINOX2totalNA),coredata(tsSVIINOX2totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSSINOX2total),as.zoo(tsSVIINOX2total),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("Volume del fango"[OX2]," [mL/L]")),ylab=expression(paste("SVI"[2]," [mL/g]")),cex.lab="1.2",xlim=c(0,1200),ylim=c(0,400),pch=16)
axis(side=2,at=seq(from = 0,to = 400,by = 100),las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1200,by = 300),las=1,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsSVIINOX2totalNA~tsSSSINOX2totalNA),lwd=2)

text(x=150,y=350, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsSSSINOX3totalNA),coredata(tsSVIINOX3totalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsSSSINOX3total),as.zoo(tsSVIINOX3total),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("Volume del fango"[OX3]," [mL/L]")),ylab=expression(paste("SVI"[3]," [mL/g]")),cex.lab="1.2",xlim=c(0,1200),ylim=c(0,400),pch=16)
axis(side=2,at=seq(from = 0,to = 400,by = 100),las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1200,by = 300),las=1,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")
abline(lm(tsSVIINOX3totalNA~tsSSSINOX3totalNA),lwd=2)

text(x=150,y=350, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
