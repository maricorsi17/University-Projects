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
tsNtotINtotal_original<-xts(x=Caprino[,29], order.by=datestotal)
tsPtotINtotal_original<-xts(x=Caprino[,27], order.by=datestotal)

tsNtotINtotal_spazi<-na.approx(xts(x=Caprino[,29], order.by=datestotal))


tsNtotINtotal<-tsNtotINtotal_original[intersect(which(!is.na(tsPtotINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsPtotINtotal<-tsPtotINtotal_original[intersect(which(!is.na(tsPtotINtotal_original)),which(!is.na(tsNtotINtotal_original)))]

rapportoP_N<-tsPtotINtotal/tsNtotINtotal
rapportoP_N[which(rapportoP_N>0.4)]<-NA
rapportoP_N<-na.omit(rapportoP_N)
rapportoP_N_NA<-tsPtotINtotal_original/tsNtotINtotal_original
MArapportoP_N<-na.omit(rollapply(rapportoP_N_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaP_N<-mean(rapportoP_N)


## Creare un oggetto con le date (2015)
tsNtotIN2015<-tsNtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsNtotIN2016<-tsNtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsNtotIN2017<-tsNtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsNtotIN2018<-tsNtotINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_N), type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/N"[tot-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.4),col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.05),las=2,format(seq(from = 0,to = 0.4,by = 0.05), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(rapportoP_N),col="darkslategrey")
lines(as.zoo(MArapportoP_N[index(rapportoP_N)]))
abline(h=mediaP_N,col="blue",lwd=2) #media
abline(h=(1.2/15),lwd=2,col="red") #min
abline(h=(1.5/12),lwd=2,col="darkgreen") #max
abline(h=(0.1),col="orange",lwd=2) #tipico

abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)

text(x=index(tsNtotIN2015[182,]),y=0.375,label="2015")
text(x=index(tsNtotIN2016[182,]),y=0.375,label="2016")
text(x=index(tsNtotIN2017[182,]),y=0.375,label="2017")
text(x=index(tsNtotIN2018[90,]),y=0.375,label="2018")
legend(x=index(tsNtotIN2016[250,]),y=0.35, c(expression(paste("P"[tot-IN],"/N"[tot-IN])),expression(paste("MA P"[tot-IN],"/N"[tot-IN]," (mensile)")),expression(paste("Media P"[tot-IN],"/N"[tot-IN])),expression(paste("Min P"[tot-IN],"/N"[tot-IN])),expression(paste("Max P"[tot-IN],"/N"[tot-IN])),expression(paste("Tipico P"[tot-IN],"/N"[tot-IN]))),col=c("darkslategrey","black","blue","red","darkgreen","orange"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,2,2),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapportoP_N),yaxt="n",ylab="[ - ]",main=expression(paste("P"[tot-IN],"/N"[tot-IN])), ylim=c(0,0.4),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.1),las=2,labels = format(seq(from = 0,to = 0.4,by = 0.1), big.mark = ".", decimal.mark = ","))
