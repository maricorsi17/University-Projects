# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")
library("tsbox")


# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")

tsSSSINOX1total<-na.approx(xts(x=SAmbrogio[,39], order.by=datestotal))
tsSVIINOX1total<-na.approx(xts(x=SAmbrogio[,41], order.by=datestotal))
tsSSSINOX2total<-na.approx(xts(x=SAmbrogio[,43], order.by=datestotal))
tsSVIINOX2total<-na.approx(xts(x=SAmbrogio[,45], order.by=datestotal))
tsSSSINOX3total<-na.approx(xts(x=SAmbrogio[,47], order.by=datestotal))
tsSVIINOX3total<-na.approx(xts(x=SAmbrogio[,49], order.by=datestotal))

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
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSSINOX1total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX1], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1400, by = 200), las=2,format(seq(from = 0,to = 1400,by = 200), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo((tsSSSINOX1total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSSINOX1total)),lwd=2)
a<-lm(tsSSSINOX1total~index(tsSSSINOX1total))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=1300,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=1300,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=1300,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=1300,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=1200, c(expression("Volume del fango"[OX1]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))

plot(as.zoo(tsSSSINOX2total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX2], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX2total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1400, by = 200), las=2,format(seq(from = 0,to = 1400,by = 200), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo((tsSSSINOX2total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSSINOX2total)),lwd=2)
a1<-lm(tsSSSINOX2total~index(tsSSSINOX2total))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=1300,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=1300,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=1300,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=1300,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=1200, c(expression("Volume del fango"[OX2]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))

plot(as.zoo(tsSSSINOX3total), xlab="Mesi",ylab=expression(paste("Volume del fango"[OX3], " [mL/L]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,1400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX3total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 1400, by = 200), las=2,format(seq(from = 0,to = 1400,by = 200), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo((tsSSSINOX3total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSSSINOX3total)),lwd=2)
a2<-lm(tsSSSINOX3total~index(tsSSSINOX3total))
abline(a2,col="red",lwd=2,lty=5)

perc_a2<--(1-coredata(a2$fitted.values[length(a2$fitted.values)])/coredata(a2$fitted.values[1]))*100

abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=1300,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=1300,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=1300,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=1300,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a2,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=1200, c(expression("Volume del fango"[OX3]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSVIINOX1total), xlab="Mesi",ylab=expression(paste("SVI"[1]," [mL/g]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX1total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 400, by = 100), las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSVIINOX1total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSVIINOX1total)),lwd=2)
a3<-lm(tsSVIINOX1total~index(tsSVIINOX1total))
abline(a3,col="red",lwd=2,lty=5)

perc_a3<--(1-coredata(a3$fitted.values[length(a3$fitted.values)])/coredata(a3$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a3,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=360, c(expression("SVI"[1]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))

plot(as.zoo(tsSVIINOX2total), xlab="Mesi",ylab=expression(paste("SVI"[2]," [mL/g]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX2total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 400, by = 100), las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSVIINOX2total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSVIINOX2total)),lwd=2)
a4<-lm(tsSVIINOX2total~index(tsSVIINOX2total))
abline(a4,col="red",lwd=2,lty=5)

perc_a4<--(1-coredata(a4$fitted.values[length(a4$fitted.values)])/coredata(a4$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a4,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=360, c(expression("SVI"[2]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))

plot(as.zoo(tsSVIINOX3total), xlab="Mesi",ylab=expression(paste("SVI"[3]," [mL/g]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,400), type="n")
drawTimeAxis(as.zoo(tsSSSINOX3total), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 400, by = 100), las=2,format(seq(from = 0,to = 400,by = 100), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo((tsSVIINOX3total)),col="darkslategrey")
lines(as.zoo(ts_trend(tsSVIINOX3total)),lwd=2)
a5<-lm(tsSVIINOX3total~index(tsSVIINOX3total))
abline(a5,col="red",lwd=2,lty=5)

perc_a5<--(1-coredata(a5$fitted.values[length(a5$fitted.values)])/coredata(a5$fitted.values[1]))*100


abline(v=index(tsSSSINOX2016[1,]),lwd=2)
abline(v=index(tsSSSINOX2017[1,]),lwd=2)
abline(v=index(tsSSSINOX2018[1,]),lwd=2)
text(x=index(tsSSSINOX2015[182,]),y=380,label="2015")
text(x=index(tsSSSINOX2016[182,]),y=380,label="2016")
text(x=index(tsSSSINOX2017[182,]),y=380,label="2017")
text(x=index(tsSSSINOX2018[90,]),y=380,label="2018")
text(x=index(tsSSSINOX2016[181,]),y=50, label=paste("Variazione = ",format(round(perc_a5,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsSSSINOX2017[160,]),y=360, c(expression("SVI"[3]),"Regressione","LOESS"),col=c("darkslategrey", "red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")



# DETRENDING
tsSSSINOX1totalDET<-tsSSSINOX1total-(ts_trend(tsSSSINOX1total))
mediaSSSOX1DET<-mean(tsSSSINOX1totalDET)

tsSSSINOX2totalDET<-tsSSSINOX2total-(ts_trend(tsSSSINOX2total))
mediaSSSOX2DET<-mean(tsSSSINOX2totalDET)

tsSSSINOX3totalDET<-tsSSSINOX3total-(ts_trend(tsSSSINOX3total))
mediaSSSOX3DET<-mean(tsSSSINOX3totalDET)

tsSVIINOX1totalDET<-tsSVIINOX1total-(ts_trend(tsSVIINOX1total))
mediaSVI1DET<-mean(tsSVIINOX1totalDET)

tsSVIINOX2totalDET<-tsSVIINOX2total-(ts_trend(tsSVIINOX2total))
mediaSVI2DET<-mean(tsSVIINOX2totalDET)

tsSVIINOX3totalDET<-tsSVIINOX3total-(ts_trend(tsSVIINOX3total))
mediaSVI3DET<-mean(tsSVIINOX3totalDET)

# STAGIONALITA'
tsSSSINOX1totalAGG<-apply.weekly(tsSSSINOX1totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSSINOX1totalAGG,lag.max = 60, main=expression("Volume del fango"[OX1]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSSINOX2totalAGG<-apply.weekly(tsSSSINOX2totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSSINOX2totalAGG,lag.max = 60, main=expression("Volume del fango"[OX2]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSSSINOX3totalAGG<-apply.weekly(tsSSSINOX3totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSSSINOX3totalAGG,lag.max = 60, main=expression("Volume del fango"[OX3]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSVIINOX1totalAGG<-apply.weekly(tsSVIINOX1totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSVIINOX1totalAGG,lag.max = 60, main=expression("SVI"[1]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSVIINOX2totalAGG<-apply.weekly(tsSVIINOX2totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSVIINOX2totalAGG,lag.max = 60, main=expression("SVI"[2]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSVIINOX3totalAGG<-apply.weekly(tsSVIINOX3totalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSVIINOX3totalAGG,lag.max = 60, main=expression("SVI"[3]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)