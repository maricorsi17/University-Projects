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
tsSSTINtotal_original<-na.approx(xts(x=Caprino[,13], order.by=datestotal))
tsSSTINtotal<-na.omit(xts(x=Caprino[,13], order.by=datestotal))
MAtsSSTINtotal<-na.omit(rollapply(xts(x=Caprino[,13], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsSSTINtotalNA<-(xts(x=Caprino[,13], order.by=datestotal))

tsBODINtotal<-na.omit(xts(x=Caprino[,15], order.by=datestotal))
MAtsBODINtotal<-na.omit(rollapply(xts(x=Caprino[,15], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsBODINtotalNA<-(xts(x=Caprino[,15], order.by=datestotal))



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
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsSSTINtotal),type="n", xlab="Mesi",ylab=expression(paste("SST"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,5000),col="grey")
drawTimeAxis(as.zoo(tsSSTINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from =0 ,to = 5000,by = 1000),las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsSSTINtotal),col="darkslategrey")
lines(as.zoo(MAtsSSTINtotal[index(tsSSTINtotal)]))

par(new = TRUE) #aggiungere grafico con secondo asse y
plot(as.zoo(tsBODINtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,2000), col="orange")
lines(as.zoo(MAtsBODINtotal[index(tsBODINtotal)]),col="red")
axis(side=4, at=seq(from=0, to=2000, by=400),las=2,format(seq(from = 0,to = 2000,by = 400), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("BOD"[5-IN]," [mg/L]")), side=4, line=4,cex=1.2)

abline(v=index(tsSSTIN2016[1,]),lwd=2)
abline(v=index(tsSSTIN2017[1,]),lwd=2)
abline(v=index(tsSSTIN2018[1,]),lwd=2)
text(x=index(tsSSTIN2015[182,]),y=1900,label="2015")
text(x=index(tsSSTIN2016[182,]),y=1900,label="2016")
text(x=index(tsSSTIN2017[182,]),y=1900,label="2017")
text(x=index(tsSSTIN2018[90,]),y=1900,label="2018")
legend(x=index(tsSSTIN2017[220,]),y=1800, c(expression(paste("SST"[IN])),expression(paste("BOD"[5-IN])),expression(paste("MA SST"[IN]," (mensile)")),expression(paste("MA BOD"[5-IN]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")

# SCATTERPLOT
corr<-cor.test(coredata(tsBODINtotalNA),coredata(tsSSTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsBODINtotal),as.zoo(tsSSTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [mg/L]")),ylab=expression(paste("SST"[IN]," [mg/L]")),cex.lab="1.2",xlim=c(0,2000),ylim=c(0,5000),pch=16)
axis(side=2,at=seq(from = 0,to = 5000,by = 1000),las=2,format(seq(from = 0,to = 5000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 2000,by = 500),las=1,format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=5,col="grey")
abline(lm(tsSSTINtotalNA~tsBODINtotalNA),lwd=2)

text(x=250,y=4500, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))