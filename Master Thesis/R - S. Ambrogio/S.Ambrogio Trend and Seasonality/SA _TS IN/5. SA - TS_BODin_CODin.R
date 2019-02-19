# Installare pacchetti
#install.packages("xts")
library("xts")
#install.packages("TTR")
library("TTR")
library("plotrix")
library("hydroTSM")
library("zoo")
library("tsbox")


# caricare file excel
SAmbrogio<-read.csv2(file="c:/Users/Marianna/Documents/Universita/Tesi/R - S. Ambrogio/NoOutliers_S.Ambrogio.CSV", header=TRUE, sep=";")

# Creare timeseries
## Creare un oggetto con le date (tutti gli anni)
datestotal<-seq(as.Date("2015-01-01"), length=1277, by="days")
## Creare la time series (tutti gli anni)
tsBODINtotal_original<-xts(x=SAmbrogio[,17], order.by=datestotal)
#tsBODINtotal_original["2016-01-25"]<-NA
#la riga sopra serve per rimuovere il dato

tsBODINtotal_spazi<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))


tsCODINtotal_original<-xts(x=SAmbrogio[,19], order.by=datestotal)
MAtsCODINtotal_original<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))




tsBODINtotal<-tsBODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsCODINtotal_original)))]
tsCODINtotal<-tsCODINtotal_original[intersect(which(!is.na(tsBODINtotal_original)),which(!is.na(tsCODINtotal_original)))]


rapporto<-tsBODINtotal/tsCODINtotal
rapportoNA<-tsBODINtotal_original/tsCODINtotal_original
MArapporto<-na.omit(rollapply(rapportoNA, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))



mediaRapporto<-mean(rapporto)


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

plot(as.zoo(rapporto),type="n",xlab="Mesi",ylab=expression(paste("BOD"[5-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,1.2),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 1.2,by = 0.2),las=2,format(seq(from = 0,to = 1.2,by = 0.2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(rapporto),col="darkslategrey")
lines(as.zoo(ts_trend(rapporto)),lwd=2)
a<-lm(rapporto~index(rapporto))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=1.1,label="2015")
text(x=index(tsBODIN2016[182,]),y=1.1,label="2016")
text(x=index(tsBODIN2017[182,]),y=1.1,label="2017")
text(x=index(tsBODIN2018[90,]),y=1.1,label="2018")
text(x=index(tsBODIN2016[181,]),y=0.1, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsBODIN2015[30,]),y=1.05, c(expression(paste("BOD"[5-IN],"/COD"[IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")

# DETRENDING
rapportoDET<-rapporto-(ts_trend(rapporto))
mediaDET<-mean(rapportoDET)

# STAGIONALITA'
rapportoAGG<-apply.weekly(rapportoDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoAGG,lag.max = 60, main=expression(paste("BOD"[5-IN],"/COD"[IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)