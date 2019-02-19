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
tsPtotINtotal_original<-xts(x=Caprino[,27], order.by=datestotal)
tsPtotINtotal_original["2018-01-15"]<-NA #la riga sopra serve per rimuovere il dato
tsBODINtotal_original<-xts(x=Caprino[,15], order.by=datestotal)
tsBODINtotal_original["2015-06-08"]<-NA #la riga sopra serve per rimuovere il dato
tsCODINtotal_original<-xts(x=Caprino[,17], order.by=datestotal)
tsCODINtotal_original["2015-08-03"]<-NA #la riga sopra serve per rimuovere il dato

tsPtotINtotal_spazi<-na.approx(xts(x=Caprino[,27], order.by=datestotal))


tsBODINtotal<-tsBODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsPtotINtotal_BOD<-tsPtotINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsCODINtotal<-tsCODINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]
tsPtotINtotal_COD<-tsPtotINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsPtotINtotal_original)))]

rapportoP_BOD<-tsPtotINtotal_BOD/tsBODINtotal
rapportoP_BOD_NA<-tsPtotINtotal_original/tsBODINtotal_original
MArapportoP_BOD<-na.omit(rollapply(rapportoP_BOD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaP_BOD<-mean(rapportoP_BOD)

rapportoP_COD<-tsPtotINtotal_COD/tsCODINtotal
rapportoP_COD_NA<-tsPtotINtotal_original/tsCODINtotal_original
MArapportoP_COD<-na.omit(rollapply(rapportoP_COD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaP_COD<-mean(rapportoP_COD)

## Creare un oggetto con le date (2015)
tsPtotIN2015<-tsPtotINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsPtotIN2016<-tsPtotINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsPtotIN2017<-tsPtotINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsPtotIN2018<-tsPtotINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_BOD),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/BOD"[5-IN],", P"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.14),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.14,by = 0.02),las=2,format(seq(from = 0,to = 0.14,by = 0.02),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=7,col="grey")
lines(as.zoo(rapportoP_BOD),col="darkslategrey")
lines(as.zoo(MArapportoP_BOD[index(rapportoP_BOD)]))
lines(as.zoo(rapportoP_COD),col="orange")
lines(as.zoo(MArapportoP_COD[index(rapportoP_COD)]),col="red")


abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)

text(x=index(tsPtotIN2015[182,]),y=0.13,label="2015")
text(x=index(tsPtotIN2016[182,]),y=0.13,label="2016")
text(x=index(tsPtotIN2017[182,]),y=0.13,label="2017")
text(x=index(tsPtotIN2018[90,]),y=0.13,label="2018")
legend(x=index(tsPtotIN2017[190,]),y=0.125, c(expression(paste("P"[tot-IN],"/BOD"[5-IN])),expression(paste("MA P"[tot-IN],"/BOD"[5-IN]," (mensile)")),expression(paste("P"[tot-IN],"/COD"[IN])), expression(paste("MA P"[tot-IN],"/COD"[IN]," (mensile)"))),col=c("darkslategrey","black","orange","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")




# RAPPORTO P_BOD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_BOD),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/BOD"[5-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.08),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.08,by = 0.01),las=2,format(seq(from = 0,to = 0.08,by = 0.01),big.mark = ".",decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(rapportoP_BOD),col="darkslategrey")
lines(as.zoo(MArapportoP_BOD[index(rapportoP_BOD)]))
abline(h=mediaP_BOD,col="blue",lwd=2) #media
abline(h=(1.2/70),lwd=2,col="red") #min
abline(h=(1.5/54),lwd=2,col="darkgreen") #max
abline(h=(0.02),col="orange",lwd=2) #tipico

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)

text(x=index(tsPtotIN2015[182,]),y=0.075,label="2015")
text(x=index(tsPtotIN2016[182,]),y=0.075,label="2016")
text(x=index(tsPtotIN2017[182,]),y=0.075,label="2017")
text(x=index(tsPtotIN2018[90,]),y=0.075,label="2018")
legend(x=index(tsPtotIN2017[190,]),y=0.072, c(expression(paste("P"[tot-IN],"/BOD"[5-IN])),expression(paste("MA P"[tot-IN],"/BOD"[5-IN]," (mensile)")),expression(paste("Media P"[tot-IN],"/BOD"[5-IN])),expression(paste("Min P"[tot-IN],"/BOD"[5-IN])),expression(paste("Max P"[tot-IN],"/BOD"[5-IN])),expression(paste("Tipico P"[tot-IN],"/BOD"[5-IN]))),col=c("darkslategrey","black","blue","red","darkgreen","orange"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,2,2),bg="white")




# RAPPORTO P_COD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoP_COD),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.04),col="grey")
drawTimeAxis(as.zoo(tsPtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.04,by = 0.01),las=2,format(seq(from = 0,to = 0.04,by = 0.01), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
lines(as.zoo(rapportoP_COD),col="darkslategrey")
lines(as.zoo(MArapportoP_COD[index(rapportoP_COD)]))
abline(h=mediaP_COD,col="blue",lwd=2) #media
abline(h=(1.2/130),lwd=2,col="red") #min
abline(h=(1.5/110),lwd=2,col="darkgreen") #max
abline(h=(0.01),col="orange",lwd=2) #tipico

abline(v=index(tsPtotIN2016[1,]),lwd=2)
abline(v=index(tsPtotIN2017[1,]),lwd=2)
abline(v=index(tsPtotIN2018[1,]),lwd=2)

text(x=index(tsPtotIN2015[182,]),y=0.0375,label="2015")
text(x=index(tsPtotIN2016[182,]),y=0.0375,label="2016")
text(x=index(tsPtotIN2017[182,]),y=0.0375,label="2017")
text(x=index(tsPtotIN2018[90,]),y=0.0375,label="2018")
legend(x=index(tsPtotIN2017[190,]),y=0.035, c(expression(paste("P"[tot-IN],"/COD"[IN])),expression(paste("MA P"[tot-IN],"/COD"[IN]," (mensile)")),expression(paste("Media P"[tot-IN],"/COD"[IN])),expression(paste("Min P"[tot-IN],"/COD"[IN])),expression(paste("Max P"[tot-IN],"/COD"[IN])),expression(paste("Tipico P"[tot-IN],"/COD"[IN]))),col=c("darkslategrey","black","blue","red","darkgreen","orange"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,2,2),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapportoP_BOD),yaxt="n",ylab="[ - ]",main=expression(paste("P"[tot-IN],"/BOD"[5-IN])), ylim=c(0,0.15),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.15,by = 0.01),las=2,labels = format(seq(from = 0,to = 0.15,by = 0.01), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapportoP_COD),yaxt="n",ylab="[ - ]",main=expression(paste("P"[tot-IN],"/COD"[IN])), ylim=c(0,0.08),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.08,by = 0.01),las=2,labels = format(seq(from = 0,to = 0.08,by = 0.01), big.mark = ".", decimal.mark = ","))
