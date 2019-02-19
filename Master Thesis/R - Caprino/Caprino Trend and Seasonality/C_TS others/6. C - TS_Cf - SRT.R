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
Caprino<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - Caprino/Caprino.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQINtotal<-xts(x=Caprino[,6], order.by=datestotal)
tsQOUTtotal<-xts(x=Caprino[,6], order.by=datestotal)

ts1_QINtotal<-xts(x=1/Caprino[,6], order.by=datestotal)


tsSSTINOX1total<-(xts(x=Caprino[,38], order.by=datestotal))
tsSSTINOX1total[which(tsSSTINOX1total==600)]<-NA

tsSSTOUTtotal<-na.omit(xts(x=Caprino[,14], order.by=datestotal))



tsCaricoBODINtotal<-(na.omit(xts(x=Caprino[,15], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
# tsmediaSSTOX<-tsSSTINOX1total
# toSubstitute<-datestotal[intersect(which(is.na(tsmediaSSTOX)),which(!is.na((xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)))]
# tsTemp<-na.trim(tsmediaSSTOX)
# tsmediaSSTOX[toSubstitute]<-((na.locf(tsTemp) + as.xts(rev(na.locf(rev(tsTemp)))))/2)[toSubstitute]

tsmediaSSTOX<-na.approx(tsSSTINOX1total)

tsCf<-(tsCaricoBODINtotal)/(tsmediaSSTOX*986)

tsQftotal<-(xts(x=Caprino[,8], order.by=datestotal))
tsSSTINRIC1total<-na.omit(xts(x=Caprino[,40], order.by=datestotal))

datesmonthly<-seq(as.Date("2015-01-01"),as.Date("2018-06-30"),by="months")

mensileSSTOX1<-apply.monthly(na.omit(tsSSTINOX1total),mean)
index(mensileSSTOX1)<-datesmonthly

mensileSSTRIC1<-apply.monthly(tsSSTINRIC1total,mean)
index(mensileSSTRIC1)<-datesmonthly

mensileSSTOUT<-apply.monthly(na.omit(tsQOUTtotal*tsSSTOUTtotal),mean)
index(mensileSSTOUT)<-datesmonthly

tsQfnoNA<-tsQftotal
tsQfnoNA[which(is.na(tsQfnoNA))]<-0
tsQfmensile<-apply.monthly(tsQfnoNA,function(x) sum(x)/length(x))

index(tsQfmensile)<-datesmonthly

tsSRT<-(986*mensileSSTOX1)/(tsQfmensile*mensileSSTRIC1+mensileSSTOUT/1000)

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
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("C"[f]," [kgBOD"[5],"/(kgSST"[OX]," d)]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,0.8))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 0.8, by = 0.1), las=2,format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCf),col="darkslategrey")
lines(as.zoo(ts_trend(tsCf)),lwd=2)
a<-lm(tsCf~index(tsCf))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=0.75,label="2015")
text(x=index(ts1_QIN2016[182,]),y=0.75,label="2016")
text(x=index(ts1_QIN2017[182,]),y=0.75,label="2017")
text(x=index(ts1_QIN2018[90,]),y=0.75,label="2018")
text(x=index(ts1_QIN2016[181,]),y=0.35, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2017[100,]),y=0.65, c(expression("C"[f]),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# GRAFICO SRT
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("SRT [d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,18))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 18, by = 2), las=2,format(seq(from = 0,to = 18,by = 2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=9,col="grey")
lines(as.zoo(tsSRT),col="darkslategrey")
lines(as.zoo(ts_trend(tsSRT)),lwd=2)
a1<-lm(tsSRT~index(tsSRT))
abline(a1,col="red",lwd=2,lty=5)

perc_a1<--(1-coredata(a1$fitted.values[length(a1$fitted.values)])/coredata(a1$fitted.values[1]))*100



abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=17,label="2015")
text(x=index(ts1_QIN2016[182,]),y=17,label="2016")
text(x=index(ts1_QIN2017[182,]),y=17,label="2017")
text(x=index(ts1_QIN2018[90,]),y=17,label="2018")
text(x=index(ts1_QIN2016[181,]),y=15, label=paste("Variazione = ",format(round(perc_a1,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(ts1_QIN2017[100,]),y=5, c(expression("SRT"),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")




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