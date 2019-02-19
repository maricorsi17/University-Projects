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
tsQOUTtotal<-xts(x=SAmbrogio[,8], order.by=datestotal)

ts1QOUTtotal<-1/tsQOUTtotal

MAts1QOUTtotal<-na.omit(rollapply(ts1QOUTtotal, width=15, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))

tsBODINtotal<-na.omit(xts(x=SAmbrogio[,15], order.by=datestotal))
MAtsBODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,15], order.by=datestotal), width=15, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsBODINtotalNA<-(xts(x=SAmbrogio[,15], order.by=datestotal))



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
plot(as.zoo(ts1QOUTtotal),type="n", xlab="Mesi",ylab=expression(paste("1/Q [d/m"^"3","]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.0015), col="grey")
drawTimeAxis(as.zoo(ts1QOUTtotal), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.0015,by = 0.00025),las=2,format(seq(from = 0,to = 0.0015,by = 0.00025), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(ts1QOUTtotal),col="darkslategrey")
lines(as.zoo(MAts1QOUTtotal))

par(new = TRUE) #aggiungere grafico con secondo asse y
plot(as.zoo(tsBODINtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,1200), col="orange")
lines(as.zoo(MAtsBODINtotal[index(tsBODINtotal)]),col="red")
axis(side=4, at=seq(from=0, to=1200, by=200),las=2,format(seq(from = 0,to = 1200,by = 200), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("BOD"[5-IN]," [mg/L]")), side=4, line=3,cex=1.2)

abline(v=index(tsQOUT2016[1,]),lwd=2,lty=1)
abline(v=index(tsQOUT2017[1,]),lwd=2,lty=1)
abline(v=index(tsQOUT2018[1,]),lwd=2,lty=1)
text(x=index(tsQOUT2015[182,]),y=1100,label="2015")
text(x=index(tsQOUT2016[182,]),y=1100,label="2016")
text(x=index(tsQOUT2017[182,]),y=1100,label="2017")
text(x=index(tsQOUT2018[90,]),y=1100,label="2018")
legend(x=index(tsQOUT2015[20,]),y=1050, c("1/Q",expression(paste("BOD"[5-IN])),"MA 1/Q (bisettimanale)",expression(paste("MA BOD"[5-IN]," (bisettimanale)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")


# SCATTERPLOT
corr<-cor.test(coredata(tsBODINtotalNA),coredata(ts1QOUTtotal),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsBODINtotal),as.zoo(ts1QOUTtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [mg/L]")),ylab=expression(paste("1/Q [d/m"^"3","]")),cex.lab="1.2",xlim=c(0,1200),ylim=c(0,0.00075),pch=16)
axis(side=2,at=seq(from = 0,to = 0.00075,by = 0.00025),las=2,format(seq(from = 0,to = 0.00075,by = 0.00025), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1200,by = 300),las=1,format(seq(from = 0,to = 1200,by = 300), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=3,col="grey")

abline(lm(ts1QOUTtotal~tsBODINtotalNA),lwd=2)

text(x=1050,y=0.000625, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
