library("xts")
library("TTR")
library("plotrix")
library("hydroTSM")
library("scales")

# caricare file excel
SAmbrogio_OD<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/OD_S.Ambrogio_3.CSV", header=TRUE, sep=";")

allyears<-(c(na.trim(SAmbrogio_OD[,3]),na.trim(SAmbrogio_OD[,4]),na.trim(SAmbrogio_OD[,5]),na.trim(SAmbrogio_OD[,6]),na.trim(SAmbrogio_OD[,7]),na.trim(SAmbrogio_OD[,8]),na.trim(SAmbrogio_OD[,9]),na.trim(SAmbrogio_OD[,10]),na.trim(SAmbrogio_OD[,11]),na.trim(SAmbrogio_OD[,12]),na.trim(SAmbrogio_OD[,13]),na.trim(SAmbrogio_OD[,14]),na.trim(SAmbrogio_OD[,15]),na.trim(SAmbrogio_OD[,16]),na.trim(SAmbrogio_OD[,17]),na.trim(SAmbrogio_OD[,18]),na.trim(SAmbrogio_OD[,19]),na.trim(SAmbrogio_OD[,20])))

minutestotal<-seq(ISOdate(2017,1,1,0,0),ISOdate(2018,6,30,23,54),by="6 min")
tsOD<-xts(allyears,order.by=minutestotal)
tsOD[which(tsOD<0)]<-NA

#TOTALE

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsOD),type="n", xlab="Mesi",ylab=expression(paste("OD [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,12.5))
drawTimeAxis(as.zoo(tsOD), tick.tstep ="months", lab.tstep ="months",las=1,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 12.5,by = 2.5),las=2,format(seq(from = 0,to = 12.5,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsOD),col="orange")

abline(v=index(tsOD[87601,]),lwd=2)
text(x=index(tsOD[43800,]),y=11.75,label="2017")
text(x=index(tsOD[108906.5,]),y=11.75,label="2018")

#2017

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsOD[which(.indexyear(tsOD)<(2018-1900))]),type="n", xlab="Mesi",ylab=expression(paste("OD [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,12.5))
drawTimeAxis(as.zoo(tsOD), tick.tstep ="months", lab.tstep ="months",las=1,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 12.5,by = 2.5),las=2,format(seq(from = 0,to = 12.5,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsOD),col="orange")

text(x=index(tsOD[43800,]),y=11.75,label="2017")

#2018

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsOD[which(.indexyear(tsOD)>(2017-1900))]),type="n", xlab="Mesi",ylab=expression(paste("OD [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,12.5))
drawTimeAxis(as.zoo(tsOD), tick.tstep ="months", lab.tstep ="months",las=1,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 12.5,by = 2.5),las=2,format(seq(from = 0,to = 12.5,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsOD),col="orange")

text(x=index(tsOD[108906.5,]),y=11.75,label="2018")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsOD),yaxt="n",ylab=expression(paste("[mg/L]")),main=expression(paste("OD linea 3")), ylim=c(0,12),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 12,by = 2),las=2,labels = format(seq(from = 0,to = 12,by = 2), big.mark = ".", decimal.mark = ","))



media_marzoaprile2017<-mean(na.omit(tsOD["2017-03::2017-04"]))
media_15giugnoagosto2017<-mean(na.omit(tsOD["2017-06-15::2017-08-15"]))
mediagennaiofebbraio2018<-mean(na.omit(tsOD["2018-01::2018-02"]))
