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


tsBODOUTtotal<-na.omit(xts(x=Caprino[,16], order.by=datestotal))
MAtsBODOUTtotal<-na.omit(rollapply(xts(x=Caprino[,16], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsBODOUTtotalNA<-(xts(x=Caprino[,16], order.by=datestotal))



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

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,900),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 900,by = 50),las=2)
grid(nx=NA,ny=18,col="grey")
lines(as.zoo(tsBODINtotal),col="darkslategrey")
lines(as.zoo(tsBODOUTtotal),col="orange")
lines(as.zoo(MAtsBODINtotal[index(tsBODINtotal)]))
lines(as.zoo(MAtsBODOUTtotal[index(tsBODOUTtotal)]),col="red")
abline(h=20,col="blue",lty=2)

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=875,label="2015")
text(x=index(tsBODIN2016[182,]),y=875,label="2016")
text(x=index(tsBODIN2017[182,]),y=875,label="2017")
text(x=index(tsBODIN2018[90,]),y=875,label="2018")
legend(x=index(tsBODIN2017[180,]),y=835, c(expression(paste("BOD"[5-IN])),expression(paste("BOD"[5-OUT])),expression(paste("MA BOD"[5-IN]," (mensile)")), expression(paste("MA BOD"[5-OUT]," (mensile)")), "Limite"),col=c("darkslategrey","orange","black","red","blue"),lty=c(1,1,1,1,2),lwd=c(1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsBODOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("BOD"[5-OUT])), ylim=c(0,22),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 22,by = 2),las=2,labels = format(seq(from = 0,to = 22,by = 2), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsBODINtotalNA),coredata(tsBODOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsBODINtotal),as.zoo(tsBODOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [mg/L]")),ylab=expression(paste("BOD"[5-OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,900),ylim=c(0,25),pch=16)
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2,format(seq(from = 0,to = 25,by = 5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 900,by = 300),las=1,format(seq(from = 0,to = 900,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=5,col="grey")
abline(lm(tsBODOUTtotalNA~tsBODINtotalNA),lwd=2)

text(x=150,y=22.5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))

