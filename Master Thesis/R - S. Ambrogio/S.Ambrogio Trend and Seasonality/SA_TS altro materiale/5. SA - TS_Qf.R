# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("tsbox")


# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsQftotal_original<-na.approx(xts(x=SAmbrogio[,10], order.by=datestotal))
tsQftotal<-(xts(x=SAmbrogio[,10], order.by=datestotal))
tsQftotal[which(is.na(tsQftotal))]<-0
MAtsQftotal<-na.omit(rollapply(xts(x=SAmbrogio[,10], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



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
lines(as.zoo(ts_trend(tsQftotal)),lwd=2)
a<-lm(tsQftotal~index(tsQftotal))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100


abline(v=index(tsQIN2016[1,]),lwd=2)
abline(v=index(tsQIN2017[1,]),lwd=2)
abline(v=index(tsQIN2018[1,]),lwd=2)
text(x=index(tsQIN2015[182,]),y=525,label="2015")
text(x=index(tsQIN2016[182,]),y=525,label="2016")
text(x=index(tsQIN2017[182,]),y=525,label="2017")
text(x=index(tsQIN2018[90,]),y=525,label="2018")
text(x=index(tsQIN2016[181,]),y=375, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsQIN2017[80,]),y=490, c(expression("Q"[f]),"Regressione","LOESS"),col=c("darkslategrey","red", "black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
tsQftotalDET<-tsQftotal-(ts_trend(tsQftotal))
mediaDET<-mean(tsQftotalDET)

# STAGIONALITA'
tsQftotalAGG<-apply.weekly(tsQftotalDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(tsQftotalAGG,lag.max = 60, main=expression("Q"[f]),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)