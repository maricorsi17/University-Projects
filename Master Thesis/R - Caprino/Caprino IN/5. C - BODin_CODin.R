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
tsBODINtotal_original<-xts(x=Caprino[,15], order.by=datestotal)
# tsBODINtotal_original["2015-06-08"]<-NA #la riga sopra serve per rimuovere il dato

tsBODINtotal_spazi<-na.approx(xts(x=Caprino[,15], order.by=datestotal))


tsCODINtotal_original<-xts(x=Caprino[,17], order.by=datestotal)
tsCODINtotal_original["2015-08-03"]<-NA 
MAtsCODINtotal_original<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))

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
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(rapporto),type="n",xlab="Mesi",ylab=expression(paste("BOD"[5-IN],"/COD"[IN]," [ - ]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,1.2),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 1.2,by = 0.2),las=2,format(seq(from = 0,to = 1.2,by = 0.2), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=6,col="grey")
lines(as.zoo(rapporto),col="darkslategrey")
lines(as.zoo(MArapporto[index(rapporto)]))
abline(h=mediaRapporto,col="blue",lwd=2) #media
abline(h=(54/130),lwd=2,col="red") #min
abline(h=(70/110),lwd=2,col="darkgreen") #max
abline(h=(60/120),col="orange",lwd=2) #tipico

par(new = TRUE) #aggiungere grafico con secondo asse y
# le linne qua sotto (commentate) servono per rappresentare COD e sua media mobile
# plot(as.zoo(na.approx(tsCODINtotal_original)), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,5000), col="orange")
# lines(as.zoo(MAtsCODINtotal_original),col="red")
plot(as.zoo(MAtsCODINtotal_original[index(na.omit(tsCODINtotal_original))]), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(0,6000), col="purple")
axis(side=4, at=seq(from=0, to=6000, by=1000),las=2,format(seq(from = 0,to = 6000,by = 1000), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("COD"[IN]," [mg/L]")), side=4, line=4,cex=1.2)


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)

text(x=index(tsBODIN2015[182,]),y=5750,label="2015")
text(x=index(tsBODIN2016[182,]),y=5750,label="2016")
text(x=index(tsBODIN2017[182,]),y=5750,label="2017")
text(x=index(tsBODIN2018[90,]),y=5750,label="2018")

#legenda se rappresento COD e sua media mobile
#legend(x=index(tsBODIN2016[90,]),y=4600, c(expression(paste("BOD"[5],"/COD")),expression(paste("MA BOD"[5],"/COD (mensile)")),expression(paste("Max BOD"[5],"/COD liquame urbano")), expression(paste("Min BOD"[5],"/COD liquame urbano")),"COD","MA COD (15 valori)"),col=c("grey","black","red","blue","orange","red"),lty=c(1,1,1,1,1,1),lwd=c(1,1,2,2,1,1),bg="white")

legend(x=index(tsBODIN2017[80,]),y=5450, c(expression(paste("BOD"[5-IN],"/COD"[IN])),expression(paste("MA BOD"[5-IN],"/COD"[IN]," (mensile)")),expression(paste("Media BOD"[5-IN],"/COD"[IN])),expression(paste("Min BOD"[5-IN],"/COD"[IN]," liquame urbano")), expression(paste("Max BOD"[5-IN],"/COD"[IN]," liquame urbano")),expression(paste("Tipico BOD"[5-IN],"/COD"[IN]," liquame urbano")),expression(paste("MA COD"[IN]," (mensile)"))),col=c("darkslategrey","black","blue","red","darkgreen","orange","purple"),lty=c(1,1,1,1,1,1,1),lwd=c(1,1,2,2,2,2,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(rapporto),yaxt="n",ylab="[ - ]",main=expression(paste("BOD"[5-IN],"/COD"[IN])), ylim=c(0,1.1),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 1.1,by = 0.1),las=2,labels = format(seq(from = 0,to = 1.1,by = 0.1), big.mark = ".", decimal.mark = ","))
