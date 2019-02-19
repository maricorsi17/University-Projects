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
tsNtotINtotal_original<-xts(x=SAmbrogio[,31], order.by=datestotal)
tsBODINtotal_original<-xts(x=SAmbrogio[,17], order.by=datestotal)
tsCODINtotal_original<-xts(x=SAmbrogio[,19], order.by=datestotal)

tsBODINtotal_spazi<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))


tsBODINtotal<-tsBODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsNtotINtotal_BOD<-tsNtotINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsCODINtotal<-tsCODINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsNtotINtotal_COD<-tsNtotINtotal_original[intersect(which(!is.na(tsCODINtotal_original)),which(!is.na(tsNtotINtotal_original)))]

rapportoN_BOD<-tsNtotINtotal_BOD/tsBODINtotal
rapportoN_BOD_NA<-tsNtotINtotal_original/tsBODINtotal_original
MArapportoN_BOD<-na.omit(rollapply(rapportoN_BOD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaN_BOD<-mean(rapportoN_BOD)

rapportoN_COD<-tsNtotINtotal_COD/tsCODINtotal
rapportoN_COD_NA<-tsNtotINtotal_original/tsCODINtotal_original
MArapportoN_COD<-na.omit(rollapply(rapportoN_COD_NA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))


mediaN_COD<-mean(rapportoN_COD)


## Creare un oggetto con le date (2015)
tsBODIN2015<-tsBODINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsBODINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsBODINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsBODINtotal_spazi["2018"]

# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoN_BOD),type="n",xlab="Mesi",ylab=expression(paste("N"[tot-IN],"/BOD"[5-IN],", N"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.8),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.1),las=2,format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
par(new=T)
lines(as.zoo(rapportoN_BOD),col="darkslategrey")
lines(as.zoo(MArapportoN_BOD[index(rapportoN_BOD)]))
lines(as.zoo(rapportoN_COD),col="orange")
lines(as.zoo(MArapportoN_COD[index(rapportoN_COD)]),col="red")


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=0.75,label="2015")
text(x=index(tsBODIN2016[182,]),y=0.75,label="2016")
text(x=index(tsBODIN2017[182,]),y=0.75,label="2017")
text(x=index(tsBODIN2018[90,]),y=0.75,label="2018")
legend(x=index(tsBODIN2017[150,]),y=0.68, c(expression(paste("N"[tot-IN],"/BOD"[5-IN])),expression(paste("MA N"[tot-IN],"/BOD"[5-IN]," (mensile)")),expression(paste("N"[tot-IN],"/COD"[IN])), expression(paste("MA N"[tot-IN],"/COD"[IN]," (mensile)"))),col=c("darkslategrey","black","orange","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")



#RAPPORTO N_BOD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoN_BOD),type="n",xlab="Mesi",ylab=expression(paste("N"[tot-IN],"/BOD"[5-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.8),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.1),las=2,format(seq(from = 0,to = 0.8,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
par(new=T)
lines(as.zoo(rapportoN_BOD),col="darkslategrey")
lines(as.zoo(MArapportoN_BOD[index(rapportoN_BOD)]))
abline(h=mediaN_BOD,col="blue",lwd=2) #media
abline(h=(12/70),lwd=2,col="red") #min
abline(h=(15/54),lwd=2,col="darkgreen") #max
abline(h=(12/60),col="orange",lwd=2) #tipico


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=0.75,label="2015")
text(x=index(tsBODIN2016[182,]),y=0.75,label="2016")
text(x=index(tsBODIN2017[182,]),y=0.75,label="2017")
text(x=index(tsBODIN2018[90,]),y=0.75,label="2018")
legend(x=index(tsBODIN2015[30,]),y=0.68, c(expression(paste("N"[tot-IN],"/BOD"[5-IN])),expression(paste("MA N"[tot-IN],"/BOD"[5-IN]," (mensile)")),expression(paste("Media N"[tot-IN],"/BOD"[5-IN])),expression(paste("Min N"[tot-IN],"/BOD"[5-IN]," liquame urbano")),expression(paste("Max N"[tot-IN],"/BOD"[5-IN]," liquame urbano")),expression(paste("Tipico N"[tot-IN],"/BOD"[5-IN]," liquame urbano"))), col=c("darkslategrey","black","blue","red","darkgreen","orange"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,2,2),bg="white")




#RAPPORTO N_COD

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapportoN_COD),type="n",xlab="Mesi",ylab=expression(paste("N"[tot-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.4),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.1),las=2,format(seq(from = 0,to = 0.4,by = 0.1), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=4,col="grey")
par(new=T)
lines(as.zoo(rapportoN_COD),col="darkslategrey")
lines(as.zoo(MArapportoN_COD[index(rapportoN_COD)]))
abline(h=mediaN_COD,col="blue",lwd=2) #media
abline(h=(12/130),lwd=2,col="red") #min
abline(h=(15/110),lwd=2,col="darkgreen") #max
abline(h=(12/120),col="orange",lwd=2) #tipico


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=0.375,label="2015")
text(x=index(tsBODIN2016[182,]),y=0.375,label="2016")
text(x=index(tsBODIN2017[182,]),y=0.375,label="2017")
text(x=index(tsBODIN2018[90,]),y=0.375,label="2018")
legend(x=index(tsBODIN2017[120,]),y=0.35, c(expression(paste("N"[tot-IN],"/COD"[IN])),expression(paste("MA N"[tot-IN],"/COD"[IN]," (mensile)")),expression(paste("Media N"[tot-IN],"/COD"[IN])),expression(paste("Min N"[tot-IN],"/COD"[IN]," liquame urbano")),expression(paste("Max N"[tot-IN],"/COD"[IN]," liquame urbano")),expression(paste("Tipico N"[tot-IN],"/COD"[IN]," liquame urbano"))), col=c("darkslategrey","black","blue","red","darkgreen","orange"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,2,2),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapportoN_BOD),yaxt="n",ylab="[ - ]",main=expression(paste("N"[tot-IN],"/BOD"[5-IN])), ylim=c(0,0.8),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.8,by = 0.2),las=2,labels = format(seq(from = 0,to = 0.8,by = 0.2), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapportoN_COD),yaxt="n",ylab="[ - ]",main=expression(paste("N"[tot-IN],"/COD"[IN])), ylim=c(0,0.4),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.1),las=2,labels = format(seq(from = 0,to = 0.4,by = 0.1), big.mark = ".", decimal.mark = ","))
