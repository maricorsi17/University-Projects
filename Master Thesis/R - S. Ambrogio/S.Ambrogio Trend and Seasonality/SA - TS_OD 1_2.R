library("xts")
library("TTR")
library("plotrix")
library("hydroTSM")
library("scales")
library("tsbox")

# caricare file excel
SAmbrogio_OD<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/OD_S.Ambrogio_1_2.CSV", header=TRUE, sep=";")

allyears<-(c(na.trim(SAmbrogio_OD[,3]),na.trim(SAmbrogio_OD[,4]),na.trim(SAmbrogio_OD[,5]),na.trim(SAmbrogio_OD[,6]),na.trim(SAmbrogio_OD[,7]),na.trim(SAmbrogio_OD[,8]),na.trim(SAmbrogio_OD[,9]),na.trim(SAmbrogio_OD[,10]),na.trim(SAmbrogio_OD[,11]),na.trim(SAmbrogio_OD[,12]),na.trim(SAmbrogio_OD[,13]),na.trim(SAmbrogio_OD[,14]),na.trim(SAmbrogio_OD[,15]),na.trim(SAmbrogio_OD[,16]),na.trim(SAmbrogio_OD[,17]),na.trim(SAmbrogio_OD[,18]),as.numeric(sub(",",".",as.character(na.trim(SAmbrogio_OD[,19])))),na.trim(SAmbrogio_OD[,20])))

minutestotal<-seq(ISOdate(2017,1,1,0,0),ISOdate(2018,6,30,23,54),by="6 min")
tsOD<-xts(allyears,order.by=minutestotal)
tsOD[which(tsOD<0)]<-NA

#TOTALE

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsOD),type="n", xlab="Mesi",ylab=expression(paste("OD linee 1 e 2 [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,12.5))
drawTimeAxis(as.zoo(tsOD), tick.tstep ="months", lab.tstep ="months",las=1,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 12.5,by = 2.5),las=2,format(seq(from = 0,to = 12.5,by = 2.5), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsOD),col="darkslategrey")
lines(as.zoo(ts_trend(tsOD)),lwd=2)
a<-lm(tsOD~index(tsOD))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsOD[87601,]),lwd=2)
text(x=index(tsOD[43800,]),y=11.75,label="2017")
text(x=index(tsOD[108906.5,]),y=11.75,label="2018")
text(x=index(tsOD[43800,]),y=7, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ",",nsmall=1),"%"),col="red")
legend(x=index(tsOD[108906.5,]),y=10.5, c(expression(paste("OD")),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsODDET<-tsOD-(ts_trend(tsOD))
mediaDET<-mean(tsODDET)

# STAGIONALITA'

tsODAGG<-apply.weekly(tsODDET,median)


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(coredata(tsODAGG),lag.max = 60, main=expression("OD linee 1 e 2"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)
