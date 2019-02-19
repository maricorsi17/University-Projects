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
tsPtotINtotal<-na.omit(xts(x=SAmbrogio[,29], order.by=datestotal))
MAtsPtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,29], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsPtotINtotalNA<-(xts(x=SAmbrogio[,29], order.by=datestotal))


tsPtotOUTtotal_original<-na.approx(xts(x=SAmbrogio[,30], order.by=datestotal))
tsPtotOUTtotal<-na.omit(xts(x=SAmbrogio[,30], order.by=datestotal))
MAtsPtotOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,30], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsPtotOUTtotalNA<-(xts(x=SAmbrogio[,30], order.by=datestotal))




## Creare un oggetto con le date (2015)
tsPtotOUT2015<-tsPtotOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsPtotOUT2016<-tsPtotOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsPtotOUT2017<-tsPtotOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsPtotOUT2018<-tsPtotOUTtotal_original["2018"]

media2015<-mean(tsPtotOUT2015)
media2016<-mean(tsPtotOUT2016)
media2017<-mean(tsPtotOUT2017)
media2018<-mean(tsPtotOUT2018)


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsPtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("P"[tot]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,40),col="grey")
drawTimeAxis(as.zoo(tsPtotOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 40,by = 10),las=2)
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(tsPtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsPtotINtotal[index(tsPtotINtotal)]))
lines(as.zoo(tsPtotOUTtotal),col="orange")
lines(as.zoo(MAtsPtotOUTtotal[index(tsPtotOUTtotal)]),col="red")
# ablineclip(h=media2015,x1=index(tsPtotOUTtotal_original[1,]),x2=index(tsPtotOUTtotal_original[365,]),col="darkgreen",lwd=2)
# ablineclip(h=media2016,x1=index(tsPtotOUT2016[1,]),x2=index(tsPtotOUT2016[366,]),col="darkgreen",lwd=2)
# ablineclip(h=media2017,x1=index(tsPtotOUT2017[1,]),x2=index(tsPtotOUT2017[365,]),col="darkgreen",lwd=2)
# ablineclip(h=media2018,x1=index(tsPtotOUT2018[1,]),x2=index(tsPtotOUTtotal_original[1268,]),col="darkgreen",lwd=2)



abline(v=index(tsPtotOUT2016[1,]),lwd=2)
abline(v=index(tsPtotOUT2017[1,]),lwd=2)
abline(v=index(tsPtotOUT2018[1,]),lwd=2)
abline(h=10,col="blue",lty=2)
#abline(h="2",col="purple",lty=2)
text(x=index(tsPtotOUT2015[182,]),y=37.5,label="2015")
text(x=index(tsPtotOUT2016[182,]),y=37.5,label="2016")
text(x=index(tsPtotOUT2017[182,]),y=37.5,label="2017")
text(x=index(tsPtotOUT2018[90,]),y=37.5,label="2018")
legend(x=index(tsPtotOUT2015[30,]),y=35, c(expression(paste("P"[tot-IN])), expression(paste("P"[tot-OUT])),expression(paste("MA P"[tot-IN]," (mensile)")),expression(paste("MA P"[tot-OUT]," (mensile)")),"Limite"),col=c("darkslategrey","orange","black","red","blue"),lty=c(1,1,1,1,2),lwd=c(1,1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsPtotOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("P"[tot-OUT])), ylim=c(0,5),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 5,by = 1),las=2,labels = format(seq(from = 0,to = 5,by = 1), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsPtotOUTtotalNA),coredata(tsPtotINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsPtotINtotal),as.zoo(tsPtotOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("P"[tot-IN]," [mg/L]")),ylab=expression(paste("P"[tot-OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,30),ylim=c(0,5),pch=16)
axis(side=2,at=seq(from = 0,to = 5,by = 2.5),las=2,format(seq(from = 0,to = 5,by = 2.5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 30,by = 10),las=1,format(seq(from = 0,to = 30,by = 10), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=2,col="grey")

abline(lm(tsPtotOUTtotalNA~tsPtotINtotalNA),lwd=2)

text(x=25,y=3.75, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
