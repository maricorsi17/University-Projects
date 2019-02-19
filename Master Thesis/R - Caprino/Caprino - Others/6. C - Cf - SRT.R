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
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("C"[f]," [kgBOD"[5],"/(kgSST"[OX]," d)]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,0.8))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 0.8, by = 0.1), las=2,format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCf),col="black")

abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=0.75,label="2015")
text(x=index(ts1_QIN2016[182,]),y=0.75,label="2016")
text(x=index(ts1_QIN2017[182,]),y=0.75,label="2017")
text(x=index(ts1_QIN2018[90,]),y=0.75,label="2018")


# GRAFICO SRT
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("SRT [d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,18))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 18, by = 2), las=2,format(seq(from = 0,to = 18,by = 2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=9,col="grey")
lines(as.zoo(tsSRT),col="black")


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=17,label="2015")
text(x=index(ts1_QIN2016[182,]),y=17,label="2016")
text(x=index(ts1_QIN2017[182,]),y=17,label="2017")
text(x=index(ts1_QIN2018[90,]),y=17,label="2018")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCf),yaxt="n",ylab=expression(paste("[d"^"-1","]")),main=expression(paste("C"[f])), ylim=c(0,0.8),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.1),las=2,labels = format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSRT),yaxt="n",ylab="[d]",main=expression(paste("SRT")), ylim=c(0,20),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 20,by = 5),las=2,labels = format(seq(from = 0,to = 20,by = 5), big.mark = ".", decimal.mark = ","))
