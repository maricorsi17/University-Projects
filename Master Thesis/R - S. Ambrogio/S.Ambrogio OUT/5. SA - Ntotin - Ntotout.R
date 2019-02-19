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
tsNtotINtotal<-na.omit(xts(x=SAmbrogio[,31], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,31], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNtotINtotalNA<-(xts(x=SAmbrogio[,31], order.by=datestotal))


tsNtotOUTtotal_original<-na.approx(xts(x=SAmbrogio[,32], order.by=datestotal))
tsNtotOUTtotal<-na.omit(xts(x=SAmbrogio[,32], order.by=datestotal))
MAtsNtotOUTtotal<-na.omit(rollapply(xts(x=SAmbrogio[,32], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNtotOUTtotalNA<-(xts(x=SAmbrogio[,32], order.by=datestotal))




## Creare un oggetto con le date (2015)
tsNtotOUT2015<-tsNtotOUTtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsNtotOUT2016<-tsNtotOUTtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsNtotOUT2017<-tsNtotOUTtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsNtotOUT2018<-tsNtotOUTtotal_original["2018"]

media2015<-mean(tsNtotOUT2015)
media2016<-mean(tsNtotOUT2016)
media2017<-mean(tsNtotOUT2017)
media2018<-mean(tsNtotOUT2018)

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsNtotINtotal),type="n", xlab="Mesi",ylab=expression(paste("N"[tot]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,150),col="grey")
drawTimeAxis(as.zoo(tsNtotOUTtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 150,by = 25),las=2)
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(tsNtotINtotal),col="darkslategrey")
lines(as.zoo(MAtsNtotINtotal[index(tsNtotINtotal)]))
lines(as.zoo(tsNtotOUTtotal),col="orange")
lines(as.zoo(MAtsNtotOUTtotal[index(tsNtotOUTtotal)]),col="red")
# abline(h=15,col="blue",lty=2)
# ablineclip(h=media2015,x1=index(tsNtotOUTtotal[1,]),x2=index(tsNtotOUTtotal_original[365,]),col="darkgreen",lwd=2)
# ablineclip(h=media2016,x1=index(tsNtotOUT2016[1,]),x2=index(tsNtotOUT2016[366,]),col="darkgreen",lwd=2)
# ablineclip(h=media2017,x1=index(tsNtotOUT2017[1,]),x2=index(tsNtotOUT2017[365,]),col="darkgreen",lwd=2)
# ablineclip(h=media2018,x1=index(tsNtotOUT2018[1,]),x2=index(tsNtotOUTtotal_original[1268,]),col="darkgreen",lwd=2)


abline(v=index(tsNtotOUT2016[1,]),lwd=2)
abline(v=index(tsNtotOUT2017[1,]),lwd=2)
abline(v=index(tsNtotOUT2018[1,]),lwd=2)
text(x=index(tsNtotOUT2015[182,]),y=140,label="2015")
text(x=index(tsNtotOUT2016[182,]),y=140,label="2016")
text(x=index(tsNtotOUT2017[182,]),y=140,label="2017")
text(x=index(tsNtotOUT2018[90,]),y=140,label="2018")
legend(x=index(tsNtotOUT2015[30,]),y=135, c(expression(paste("N"[tot-IN])), expression(paste("N"[tot-OUT])),expression(paste("MA N"[tot-IN]," (mensile)")),expression(paste("MA N"[tot-OUT]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsNtotOUTtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("N"[tot-OUT])), ylim=c(0,30),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 30,by = 5),las=2,labels = format(seq(from = 0,to = 30,by = 5), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsNtotINtotalNA),coredata(tsNtotOUTtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNtotINtotal),as.zoo(tsNtotOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N"[tot-IN]," [mg/L]")),ylab=expression(paste("N"[tot-OUT]," [mg/L]")),cex.lab="1.2",xlim=c(0,150),ylim=c(0,25),pch=16)
axis(side=2,at=seq(from = 0,to = 25,by = 5),las=2,format(seq(from = 0,to = 25,by = 5), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 150,by = 50),las=1,format(seq(from = 0,to = 150,by = 50), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=5,col="grey")

abline(lm(tsNtotOUTtotalNA~tsNtotINtotalNA),lwd=2)

text(x=25,y=22.5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
