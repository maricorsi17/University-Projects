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
tsNtotINtotal_original<-xts(x=SAmbrogio[,31], order.by=datestotal)
tsPtotINtotal_original<-xts(x=SAmbrogio[,29], order.by=datestotal)

tsNtotINtotal_spazi<-na.approx(xts(x=SAmbrogio[,31], order.by=datestotal))


tsNtotINtotal<-tsNtotINtotal_original[intersect(which(!is.na(tsPtotINtotal_original)),which(!is.na(tsNtotINtotal_original)))]
tsPtotINtotal<-tsPtotINtotal_original[intersect(which(!is.na(tsPtotINtotal_original)),which(!is.na(tsNtotINtotal_original)))]

rapportoP_N<-tsPtotINtotal/tsNtotINtotal
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

plot(as.zoo(rapportoP_N),type="n",xlab="Mesi",ylab=expression(paste("P"[tot-IN],"/N"[tot-IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,0.4),col="grey")
drawTimeAxis(as.zoo(tsNtotINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 0.4,by = 0.05),las=2,format(seq(from = 0,to = 0.4,by = 0.05), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(rapportoP_N),col="darkslategrey")
lines(as.zoo(ts_trend(rapportoP_N)),lwd=2)
a<-lm(rapportoP_N~index(rapportoP_N))
abline(a,col="red",lwd=2,lty=5)

perc_a<--(1-coredata(a$fitted.values[length(a$fitted.values)])/coredata(a$fitted.values[1]))*100



abline(v=index(tsNtotIN2016[1,]),lwd=2)
abline(v=index(tsNtotIN2017[1,]),lwd=2)
abline(v=index(tsNtotIN2018[1,]),lwd=2)

text(x=index(tsNtotIN2015[182,]),y=0.375,label="2015")
text(x=index(tsNtotIN2016[182,]),y=0.375,label="2016")
text(x=index(tsNtotIN2017[182,]),y=0.375,label="2017")
text(x=index(tsNtotIN2018[90,]),y=0.375,label="2018")
text(x=index(tsNtotIN2016[181,]),y=0.225, label=paste("Variazione = ",format(round(perc_a,digits=1),decimal.mark = ","),"%"),col="red")
legend(x=index(tsNtotIN2015[45,]),y=0.35, c(expression(paste("P"[tot-IN],"/N"[tot-IN])),"Regressione","LOESS"),col=c("darkslategrey","red","black"),lty=c(1,5,1),lwd=c(1,2,2),bg="white")


# DETRENDING
rapportoP_NDET<-rapportoP_N-(ts_trend(rapportoP_N))
mediaDET<-mean(rapportoP_NDET)

# STAGIONALITA'
rapportoP_NAGG<-apply.weekly(rapportoP_NDET,median)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(3,1,0),cex.main=2)
options(OutDec= ",")
acf(rapportoP_NAGG,lag.max = 60, main=expression(paste("P"[tot-IN],"/N"[tot-IN])),yaxt="n", ci.col="black",cex.lab=1.2)
axis(side=2,las=2)