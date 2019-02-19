# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")

# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQftotal_original<-na.approx(xts(x=SAmbrogio[,10], order.by=datestotal))
tsQftotal<-(xts(x=SAmbrogio[,10], order.by=datestotal))
tsQftotal[which(is.na(tsQftotal))]<-0
#tsQftotal<-na.omit(xts(x=SAmbrogio[,10], order.by=datestotal))
MAtsQftotal<-na.omit(rollapply(tsQftotal, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



## Creare un oggetto con le date (2015)

tsQIN2015<-tsQftotal_original["2015"]

## Creare un oggetto con le date (2016)
tsQIN2016<-tsQftotal_original["2016"]

## Creare un oggetto con le date (2017)
tsQIN2017<-tsQftotal_original["2017"]

## Creare un oggetto con le date (2018)
tsQIN2018<-tsQftotal_original["2018"]





# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsQftotal),type = "n", xlab="Mesi",ylab=expression(paste("Q"[f]," [m"^"3","/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,550))
drawTimeAxis(as.zoo(tsQftotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 550,by = 50),las=2)
grid(nx=NA,ny=11,col="grey")
lines(as.zoo(tsQftotal),col="darkslategrey")
lines(as.zoo(MAtsQftotal[index(tsQftotal)]))


abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=525,label="2015")
text(x=index(tsQIN2016[182,]),y=525,label="2016")
text(x=index(tsQIN2017[182,]),y=525,label="2017")
text(x=index(tsQIN2018[90,]),y=525,label="2018")
legend(x=index(tsQIN2017[80,]),y=490, c(expression("Q"[f]),expression(paste("MA Q"[f]," (mensile)"))),col=c("darkslategrey", "black"),lty=c(1,1),lwd=c(1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsQftotal),yaxt="n",ylab=expression(paste("[m"^"3","/d]")),main=expression(paste("Q"[f])), ylim=c(0,550),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 550,by = 50),las=2,labels = format(seq(from = 0,to = 550,by = 50), big.mark = ".", decimal.mark = ","))
