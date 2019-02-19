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
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("C"[f]," [kgBOD"[5],"/(kgSST"[OX]," d)]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,0.4))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 0.4, by = 0.05), las=2,format(seq(from = 0,to = 0.4,by = 0.05), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCf),col="black")


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=0.375,label="2015")
text(x=index(ts1_QIN2016[182,]),y=0.375,label="2016")
text(x=index(ts1_QIN2017[182,]),y=0.375,label="2017")
text(x=index(ts1_QIN2018[90,]),y=0.375,label="2018")


# GRAFICO SRT
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=999)
plot(as.zoo(ts1_QINtotal),type="n", xlab="Mesi",ylab=expression(paste("SRT [d]")), yaxt = "n", xaxt = "n", yaxs="i", xaxs="i", cex.lab=1.2, ylim=c(0,50))
drawTimeAxis(as.zoo(ts1_QINtotal), tick.tstep ="months", lab.tstep ="months", las=2, lab.fmt="%m")
axis(side=2,at=seq(from = 0, to = 50, by = 10), las=2,format(seq(from = 0,to = 50,by = 10), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsSRT),col="black")


abline(v=index(ts1_QIN2016[1,]),lwd=2)
abline(v=index(ts1_QIN2017[1,]),lwd=2)
abline(v=index(ts1_QIN2018[1,]),lwd=2)
text(x=index(ts1_QIN2015[182,]),y=48,label="2015")
text(x=index(ts1_QIN2016[182,]),y=48,label="2016")
text(x=index(ts1_QIN2017[182,]),y=48,label="2017")
text(x=index(ts1_QIN2018[90,]),y=48,label="2018")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCf),yaxt="n",ylab=expression(paste("[d"^"-1","]")),main=expression(paste("C"[f])), ylim=c(0,0.4),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.1),las=2,labels = format(seq(from = 0,to = 0.4,by = 0.1), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSRT),yaxt="n",ylab="[d]",main=expression(paste("SRT")), ylim=c(0,40),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 40,by = 10),las=2,labels = format(seq(from = 0,to = 40,by = 10), big.mark = ".", decimal.mark = ","))
