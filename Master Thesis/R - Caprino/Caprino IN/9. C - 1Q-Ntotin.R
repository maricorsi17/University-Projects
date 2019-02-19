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
tsQINtotal_original<-xts(x=Caprino[,6], order.by=datestotal)
tsQINtotal_original["2016-10-01"]<-NA #rimuovere il dato
tsQINtotal_original["2016-10-02"]<-NA # rimuovere il dato
tsQINtotal_original["2015-11-24"]<-NA # rimuovere il dato
tsQINtotal<-na.approx(tsQINtotal_original)

ts1QOUTtotal<-1/tsQINtotal
MAts1QOUTtotal<-na.omit(rollapply(ts1QOUTtotal, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


tsNtotINtotal<-na.omit(xts(x=Caprino[,29], order.by=datestotal))
MAtsNtotINtotal<-na.omit(rollapply(xts(x=Caprino[,29], order.by=datestotal), width=15, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsNtotINtotalNA<-(xts(x=Caprino[,29], order.by=datestotal))


## Creare un oggetto con le date (2015)
tsQOUT2015<-ts1QOUTtotal["2015"]

## Creare un oggetto con le date (2016)
tsQOUT2016<-ts1QOUTtotal["2016"]

## Creare un oggetto con le date (2017)
tsQOUT2017<-ts1QOUTtotal["2017"]

## Creare un oggetto con le date (2018)
tsQOUT2018<-ts1QOUTtotal["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

options(scipen=5) #togliere notazione scientifica (se non va via, aumentare il numero)
plot(as.zoo(ts1QOUTtotal),type="n", xlab="Mesi",ylab=expression(paste("1/Q [d/m"^"3","]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.003), col="grey")
drawTimeAxis(as.zoo(ts1QOUTtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0 ,to = 0.003,by = 0.0005),las=2,format(seq(from = 0,to = 0.003,by = 0.0005), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(ts1QOUTtotal),col="darkslategrey")
lines(as.zoo(MAts1QOUTtotal))

par(new = TRUE) #aggiungere grafico con secondo asse y
plot(as.zoo(tsNtotINtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,150), col="orange")
lines(as.zoo(MAtsNtotINtotal[index(tsNtotINtotal)]),col="red")
axis(side=4, at=seq(from=0, to=150, by=25),las=2)
mtext(expression(paste("N"[tot-IN]," [mg/L]")), side=4, line=3,cex=1.2)


abline(v=index(tsQOUT2016[1,]),lwd=2)
abline(v=index(tsQOUT2017[1,]),lwd=2)
abline(v=index(tsQOUT2018[1,]),lwd=2)
text(x=index(tsQOUT2015[182,]),y=137.5,label="2015")
text(x=index(tsQOUT2016[182,]),y=137.5,label="2016")
text(x=index(tsQOUT2017[182,]),y=137.5,label="2017")
text(x=index(tsQOUT2018[90,]),y=137.5,label="2018")
legend(x=index(tsQOUT2017[220,]),y=130, c("1/Q",expression(paste("N"[tot-IN])),"MA 1/Q (mensile)",expression(paste("MA N"[tot-IN]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")

# SCATTERPLOT
corr<-cor.test(coredata(tsNtotINtotalNA),coredata(ts1QOUTtotal),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsNtotINtotal),as.zoo(ts1QOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("N"[tot-IN]," [mg/L]")),ylab=expression(paste("1/Q [d/m"^"3","]")),cex.lab="1.2",xlim=c(0,150),ylim=c(0,0.0025),pch=16)
axis(side=2,at=seq(from = 0,to = 0.0025,by = 0.0005),las=2,format(seq(from = 0,to = 0.0025,by = 0.0005), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 150,by = 50),las=1,format(seq(from = 0,to = 150,by = 50), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=5,col="grey")
abline(lm(ts1QOUTtotal~tsNtotINtotalNA),lwd=2)

text(x=25,y=0.00225, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
