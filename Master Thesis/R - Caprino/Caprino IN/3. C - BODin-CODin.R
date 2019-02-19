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
tsBODINtotal_original<-na.approx(xts(x=Caprino[,15], order.by=datestotal))

tsBODINtotal<-na.omit(xts(x=Caprino[,15], order.by=datestotal))
MAtsBODINtotal<-na.omit(rollapply(xts(x=Caprino[,15], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsBODINtotalNA<-(xts(x=Caprino[,15], order.by=datestotal))


tsCODINtotal<-na.omit(xts(x=Caprino[,17], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCODINtotalNA<-(xts(x=Caprino[,17], order.by=datestotal))


## Creare un oggetto con le date (2015)
tsBODIN2015<-tsBODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsBODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsBODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsBODINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-IN],", COD"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,5000),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 5000,by = 500),las=2,format(seq(from = 0,to = 5000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=10,col="grey")
lines(as.zoo(tsBODINtotal),col="darkslategrey")
lines(as.zoo(tsCODINtotal),col="orange")
lines(as.zoo(MAtsBODINtotal[index(tsBODINtotal)]))
lines(as.zoo(MAtsCODINtotal[index(tsCODINtotal)]),col="red")

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=4750,label="2015")
text(x=index(tsBODIN2016[182,]),y=4750,label="2016")
text(x=index(tsBODIN2017[182,]),y=4750,label="2017")
text(x=index(tsBODIN2018[90,]),y=4750,label="2018")
legend(x=index(tsBODIN2017[220,]),y=4390, c(expression(paste("BOD"[5-IN])),expression(paste("COD"[IN])),expression(paste("MA BOD"[5-IN]," (mensile)")), expression(paste("MA COD"[IN]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsBODINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("BOD"[5-IN])), ylim=c(0,2000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 2000,by = 500),las=2,labels = format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCODINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("COD"[IN])), ylim=c(0,5000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 5000,by = 500),las=2,labels = format(seq(from = 0,to = 5000,by = 500), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsBODINtotalNA),coredata(tsCODINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsBODINtotal),as.zoo(tsCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [mg/L]")),ylab=expression(paste("COD"[IN]," [mg/L]")),cex.lab="1.2",xlim=c(0,1500),ylim=c(0,3000),pch=16)
axis(side=2,at=seq(from = 0,to = 3000,by = 1000),las=2,format(seq(from = 0,to = 3000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to =1500,by = 500),las=1,format(seq(from = 0,to = 1500,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=3,col="grey")
abline(lm(tsCODINtotalNA~tsBODINtotalNA),lwd=2)

text(x=250,y=2500, label=paste("r = ",format(round(corr$estimate,digits=2),nsmall=2,decimal.mark=",")))
