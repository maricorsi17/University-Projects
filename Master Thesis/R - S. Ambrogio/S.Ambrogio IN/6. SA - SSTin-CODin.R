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
tsSSTINtotal_original<-na.approx(xts(x=SAmbrogio[,15], order.by=datestotal))
tsSSTINtotal<-na.omit(xts(x=SAmbrogio[,15], order.by=datestotal))
MAtsSSTINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,15], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsSSTINtotalNA<-(xts(x=SAmbrogio[,15], order.by=datestotal))


tsCODINtotal<-na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCODINtotalNA<-(xts(x=SAmbrogio[,19], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsSSTIN2015<-tsSSTINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsSSTIN2016<-tsSSTINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsSSTIN2017<-tsSSTINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsSSTIN2018<-tsSSTINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINtotal),type="n",xlab="Mesi",ylab=expression(paste("SST"[IN],", COD"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsSSTINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsSSTINtotal),col="darkslategrey")
lines(as.zoo(tsCODINtotal),col="orange")
lines(as.zoo(MAtsSSTINtotal[index(tsSSTINtotal)]))
lines(as.zoo(MAtsCODINtotal[index(tsCODINtotal)]),col="red")

abline(v=index(tsSSTIN2016[1,]),lwd=2)
abline(v=index(tsSSTIN2017[1,]),lwd=2)
abline(v=index(tsSSTIN2018[1,]),lwd=2)
text(x=index(tsSSTIN2015[182,]),y=3750,label="2015")
text(x=index(tsSSTIN2016[182,]),y=3750,label="2016")
text(x=index(tsSSTIN2017[182,]),y=3750,label="2017")
text(x=index(tsSSTIN2018[90,]),y=3750,label="2018")
legend(x=index(tsSSTIN2015[30,]),y=3500, c(expression(paste("SST"[IN])),expression(paste("COD"[IN])),expression(paste("MA SST"[IN]," (mensile)")),expression(paste("MA COD"[IN]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")


# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsSSTINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("SST"[IN])), ylim=c(0,1250),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 1250,by = 250),las=2,labels = format(seq(from = 0,to = 1250,by = 250), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsCODINtotalNA),coredata(tsSSTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCODINtotal),as.zoo(tsSSTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("COD"[IN]," [mg/L]")),ylab=expression(paste("SST"[IN]," [mg/L]")),cex.lab="1.2",xlim=c(0,2000),ylim=c(0,2000),pch=16)
axis(side=2,at=seq(from = 0,to = 2000,by = 500),las=2,format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 2000,by = 500),las=1,format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=4,col="grey")

abline(lm(tsSSTINtotalNA~tsCODINtotalNA),lwd=2)

text(x=250,y=1750, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))

