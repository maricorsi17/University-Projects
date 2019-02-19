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
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQINtotal<-xts(x=SAmbrogio[,6], order.by=datestotal)
tsQOUTtotal<-xts(x=SAmbrogio[,8], order.by=datestotal)


ts1_QINtotal<-xts(x=1/SAmbrogio[,6], order.by=datestotal)

tsSSTINOX1total<-(xts(x=SAmbrogio[,40], order.by=datestotal))
tsSSTINOX2total<-(xts(x=SAmbrogio[,44], order.by=datestotal))
tsSSTINOX3total<-(xts(x=SAmbrogio[,48], order.by=datestotal))

SSTcolonne<-cbind(tsSSTINOX1total,tsSSTINOX2total,tsSSTINOX3total)
countDati<-apply(SSTcolonne,1,function(x) sum(!is.na(x)))
SSTcolonne[which(is.na(SSTcolonne[,1]))]<-0
SSTcolonne[which(is.na(SSTcolonne[,2])),2]<-0
SSTcolonne[which(is.na(SSTcolonne[,3])),3]<-0

tsSSTOUTtotal<-na.omit(xts(x=SAmbrogio[,16], order.by=datestotal))


tsCaricoBODINtotal<-(na.omit(xts(x=SAmbrogio[,17], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
tsmediaSSTOX<-(SSTcolonne[,1]+SSTcolonne[,2]+SSTcolonne[,3])/as.numeric(countDati)
tsmediaSSTOX[which(tsmediaSSTOX==0)]<-NA
toSubstitute<-datestotal[intersect(which(is.na(tsmediaSSTOX)),which(!is.na((xts(x=SAmbrogio[,17], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)))]
tsTemp<-na.trim(tsmediaSSTOX)
tsmediaSSTOX[toSubstitute]<-((na.locf(tsTemp) + as.xts(rev(na.locf(rev(tsTemp)))))/2)[toSubstitute]


tsQftotal<-(xts(x=SAmbrogio[,10], order.by=datestotal))
tsSSTINRIC1total<-na.omit(xts(x=SAmbrogio[,42], order.by=datestotal))
tsSSTINRIC2total<-na.omit(xts(x=SAmbrogio[,46], order.by=datestotal))
tsSSTINRIC3total<-na.omit(xts(x=SAmbrogio[,50], order.by=datestotal))


datesmonthly<-seq(as.Date("2015-01-01"),as.Date("2018-06-30"),by="months")


mensileSSTOX1<-apply.monthly(na.omit(tsSSTINOX1total),mean)
mensileSSTOX2<-apply.monthly(na.omit(tsSSTINOX2total),mean)
mensileSSTOX3<-apply.monthly(na.omit(tsSSTINOX3total),mean)

index(mensileSSTOX1)<-datesmonthly
index(mensileSSTOX2)<-datesmonthly
index(mensileSSTOX3)<-datesmonthly


tsmediaSSTOXmensile<-(mensileSSTOX1+mensileSSTOX2+mensileSSTOX3)/3


mensileSSTRIC1<-apply.monthly(tsSSTINRIC1total,mean)
mensileSSTRIC2<-apply.monthly(tsSSTINRIC2total,mean)
mensileSSTRIC3<-apply.monthly(tsSSTINRIC3total,mean)

index(mensileSSTRIC1)<-datesmonthly
index(mensileSSTRIC2)<-datesmonthly
index(mensileSSTRIC3)<-datesmonthly


tsmediaSSTric<-(mensileSSTRIC1+mensileSSTRIC2+mensileSSTRIC3)/3

mensileSSTOUT<-apply.monthly(na.omit(tsQOUTtotal*tsSSTOUTtotal),mean)

index(mensileSSTOUT)<-datesmonthly


tsQfnoNA<-tsQftotal
tsQfnoNA[which(is.na(tsQfnoNA))]<-0
tsQfmensile<-apply.monthly(tsQfnoNA,function(x) sum(x)/length(x))

index(tsQfmensile)<-datesmonthly


tsCf<-(tsCaricoBODINtotal)/(tsmediaSSTOX*2932.5)
tsSRT<-(2932.5*tsmediaSSTOXmensile)/(tsQfmensile*tsmediaSSTric+mensileSSTOUT/1000)

## Creare un oggetto con le date (2015)
ts1_QIN2015<-ts1_QINtotal["2015"]
## Creare un oggetto con le date (2016)
ts1_QIN2016<-ts1_QINtotal["2016"]
## Creare un oggetto con le date (2017)
ts1_QIN2017<-ts1_QINtotal["2017"]
## Creare un oggetto con le date (2018)
ts1_QIN2018<-ts1_QINtotal["2018"]


# GRAFICO Cf
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("C"[f]," [kgBOD"[5],"/(kgSST"[OX]," d)]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,0.4))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 0.4, by = 0.05), las=2,format(seq(from = 0,to = 0.4,by = 0.05), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCf),col="darkslategrey")
lines(as.zoo(ts_trend(tsCf)),lwd=2)
a<-lm(tsCf~index(tsCf))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=0.375,label="2015")
text(x=index(ts1_QIN2016[182,]),y=0.375,label="2016")
text(x=index(ts1_QIN2017[182,]),y=0.375,label="2017")
text(x=index(ts1_QIN2018[90,]),y=0.375,label="2018")
text(x=index(ts1_QIN2016[181,]),y=0.225, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2017[160,]),y=0.33, c(expression("C"[f]),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# GRAFICO SRT
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("SRT [d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,50))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 50, by = 10), las=2,format(seq(from = 0,to = 50,by = 10), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsSRT),col="darkslategrey")
lines(as.zoo(ts_trend(tsSRT)),lwd=2)
a1<-lm(tsSRT~index(tsSRT))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=48,label="2015")
text(x=index(ts1_QIN2016[182,]),y=48,label="2016")
text(x=index(ts1_QIN2017[182,]),y=48,label="2017")
text(x=index(ts1_QIN2018[90,]),y=48,label="2018")
text(x=index(ts1_QIN2016[181,]),y=25, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2015[30,]),y=45, c(expression("SRT"),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")





# DETRENDING
tsCfDET<-tsCf-(ts_trend(tsCf))
mediaCfDET<-mean(tsCfDET)

tsSRTDET<-tsSRT-(ts_trend(tsSRT))
mediaSRTDET<-mean(tsSRTDET)

# STAGIONALITA'
tsCfAGG<-apply.weekly(tsCfDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsCfAGG,lag.max = 60, main=expression("C"[f]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)

tsSRTAGG<-apply.weekly(tsSRTDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsSRTAGG,lag.max = 12, main=expression("SRT"),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)