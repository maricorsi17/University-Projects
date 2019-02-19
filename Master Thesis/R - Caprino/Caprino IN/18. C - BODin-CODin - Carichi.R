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
tsCaricoBODINtotal_spazi<-na.approx(xts(x=Caprino[,15], order.by=datestotal))


tsCaricoBODINtotal<-(na.omit(xts(x=Caprino[,15], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoBODINtotal<-na.omit(rollapply(xts(x=Caprino[,15], order.by=datestotal)*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCaricoBODINtotalNA<-((xts(x=Caprino[,15], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)


tsCaricoCODINtotal<-(na.omit(xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=Caprino[,17], order.by=datestotal)*xts(x=Caprino[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsTINtotal<-na.approx(xts(x=Caprino[,4], order.by=datestotal))
tsCaricoCODINtotalNA<-((xts(x=Caprino[,17], order.by=datestotal))*xts(x=Caprino[,6],order.by=datestotal)/1000)
tsTINtotalNA<-(xts(x=Caprino[,4], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsBODIN2015<-tsCaricoBODINtotal_spazi["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsCaricoBODINtotal_spazi["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsCaricoBODINtotal_spazi["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsCaricoBODINtotal_spazi["2018"]


# Plottare la time series BOD
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCaricoBODINtotal),col="grey")
lines(as.zoo(MAtsCaricoBODINtotal[index(tsCaricoBODINtotal)]))

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=3750,label="2015")
text(x=index(tsBODIN2016[182,,]),y=3750,label="2016")
text(x=index(tsBODIN2017[182,,]),y=3750,label="2017")
text(x=index(tsBODIN2018[90]),y=3750,label="2018")
legend(x=index(tsBODIN2017[90,]),y=3450, c(expression(paste("BOD"[5-IN])),expression(paste("MA BOD"[5-IN]," (mensile)"))),col=c("grey","black"),lty=c(1,1),lwd=c(1,1),bg="white")



# Plottare la time series COD
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoCODINtotal),type="n", xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,8500),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 8500,by = 500),las=2,format(seq(from = 0,to = 8500,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=17,col="grey")
lines(as.zoo(tsCaricoCODINtotal),col="grey")
lines(as.zoo(MAtsCaricoCODINtotal[index(tsCaricoCODINtotal)]))

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,,]),y=8250,label="2015")
text(x=index(tsBODIN2016[182,,]),y=8250,label="2016")
text(x=index(tsBODIN2017[182,]),y=8250,label="2017")
text(x=index(tsBODIN2018[90]),y=8250,label="2018")
legend(x=index(tsBODIN2017[90,]),y=7750, c(expression(paste("COD"[IN])),expression(paste("MA COD"[IN]," (mensile)"))),col=c("grey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# Distribuzione di frequenza percentuale COD 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

x <- (unlist(lapply(index(sort(coredata(tsCaricoCODINtotal),decreasing = T)),function(x){x/length(tsCaricoCODINtotal)})))*100
y <- sort(coredata(tsCaricoCODINtotal),decreasing = T)
z <- (unlist(lapply(index(sort(coredata(tsCaricoBODINtotal),decreasing = T)),function(x){x/length(tsCaricoBODINtotal)})))*100
q <- sort(coredata(tsCaricoBODINtotal),decreasing = T)
percentiliCOD<-quantile(y, c(.90, .75, .50))
percentiliBOD<-quantile(q, c(.90, .75, .50))
plot(x, y, type = "l", lwd = 2, xlab="% superamento",ylab=expression(paste("COD"[IN],", BOD"[5-IN]," [kg/d]")),yaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,xlim=c(0,100),ylim=c(0,9000))
axis(side=2,at=seq(from = 0,to = 9000,by = 1000),las=2,format(seq(from = 0,to =9400,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 100,by = 10))
grid(nx=10,ny=9,col="grey")
lines(z,q,type="l",lwd=2,col="orange")
lines(10, percentiliCOD[1], type = "p", pch=19, cex = 2)
lines(25, percentiliCOD[2], type = "p", pch=19, cex = 2)
lines(50, percentiliCOD[3], type = "p", pch=19, cex = 2)
lines(10, percentiliBOD[1], type = "p", pch=19, cex = 2,col="orange")
lines(25, percentiliBOD[2], type = "p", pch=19, cex = 2,col="orange")
lines(50, percentiliBOD[3], type = "p", pch=19, cex = 2,col="orange")
text(x=21,y=percentiliCOD[1]+215,label="90° percentile = 2.656 kg/d")
text(x=36,y=percentiliCOD[2]+215,label="75° percentile = 1.871 kg/d")
text(x=61,y=percentiliCOD[3]+215,label="50° percentile = 1.203 kg/d")
text(x=21,y=percentiliBOD[1]+215,label="90° percentile = 1.034 kg/d")
text(x=36,y=percentiliBOD[2]+215,label="75° percentile = 732 kg/d")
text(x=61,y=percentiliBOD[3]+215,label="50° percentile = 517 kg/d")
legend(x=75,y=8500, c(expression(paste("COD"[IN])),expression(paste("BOD"[5-IN]))),col=c("black","orange"),lty=c(1,1),lwd=c(2,2),bg="white")

# GRAFICO CARICO COD E T
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoCODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,10000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 10000,by = 2000),las=2,format(seq(from = 0,to = 10000,by = 2000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsCaricoCODINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoCODINtotal[index(tsCaricoCODINtotal)]))
par(new=T)
plot(as.zoo(tsTINtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(5,30), col="orange")
axis(side=4, at=seq(from=5, to=30, by=5),las=2,format(seq(from = 5,to = 30,by = 5), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("T"[IN]," [",degree,"C]")), side=4, line=3,cex=1.2)

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=29,label="2015")
text(x=index(tsBODIN2016[182,]),y=29,label="2016")
text(x=index(tsBODIN2017[182,]),y=29,label="2017")
text(x=index(tsBODIN2018[91]),y=29,label="2018")
legend(x=index(tsBODIN2017[250,]),y=27.5, c(expression(paste("COD"[IN])),expression(paste("MA COD"[5-IN]," (mensile)")),expression(paste("T"[IN]))),col=c("darkslategrey","black","orange"),lty=c(1,1,1),lwd=c(1,1,1),bg="white")

# GRAFICO CARICO BOD E T
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_spazi), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 800),las=2,format(seq(from = 0,to = 4000,by = 800), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=5,col="grey")
lines(as.zoo(tsCaricoBODINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoBODINtotal[index(tsCaricoBODINtotal)]))
par(new=T)
plot(as.zoo(tsTINtotal), type = "l", yaxt="n",xaxt="n",xaxs="i",yaxs="i", bty="n", xlab="", ylab="", ylim=c(5,30), col="orange")
axis(side=4, at=seq(from=5, to=30, by=5),las=2,format(seq(from = 5,to = 30,by = 5), big.mark = ".", decimal.mark = ","))
mtext(expression(paste("T"[IN]," [",degree,"C]")), side=4, line=3,cex=1.2)


abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=29,label="2015")
text(x=index(tsBODIN2016[182,,]),y=29,label="2016")
text(x=index(tsBODIN2017[182,,]),y=29,label="2017")
text(x=index(tsBODIN2018[90]),y=29,label="2018")
legend(x=index(tsBODIN2017[240,]),y=28, c(expression(paste("BOD"[5-IN])),expression(paste("MA BOD"[5-IN]," (mensile)")),expression(paste("T"[IN]))),col=c("darkslategrey","black","orange"),lty=c(1,1,1),lwd=c(1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoBODINtotal),yaxt="n",ylab="[kg/d]",main=expression(paste("BOD"[5-IN])), ylim=c(0,4000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,labels = format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoCODINtotal),yaxt="n",ylab="[kg/d]",main=expression(paste("COD"[IN])), ylim=c(0,8500),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 8500,by = 500),las=2,labels = format(seq(from = 0,to = 8500,by = 500), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsCaricoBODINtotalNA),coredata(tsTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCaricoBODINtotal),as.zoo(tsTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [kg/d]")),ylab=expression(paste("T"[IN]," [",degree,"C]")),cex.lab="1.2",xlim=c(0,4000),ylim=c(0,30),pch=16)
axis(side=2,at=seq(from = 0,to = 30,by = 10),las=2,format(seq(from = 0,to = 30,by = 10), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 4000,by = 1000),las=1,format(seq(from = 0,to = 4000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=4,ny=3,col="grey")
abline(lm(tsTINtotalNA~tsCaricoBODINtotalNA),lwd=2)

text(x=3500,y=5, label=paste("r = ",format(round(corr$estimate,digits=2),nsmall=2,decimal.mark=",")))


corr<-cor.test(coredata(tsCaricoCODINtotalNA),coredata(tsTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCaricoCODINtotal),as.zoo(tsTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("COD"[IN]," [kg/d]")),ylab=expression(paste("T"[IN]," [",degree,"C]")),cex.lab="1.2",xlim=c(0,9000),ylim=c(0,30),pch=16)
axis(side=2,at=seq(from = 0,to = 30,by = 10),las=2,format(seq(from = 0,to = 30,by = 10), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 9000,by = 3000),las=1,format(seq(from = 0,to = 9000,by = 3000), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=3,col="grey")
abline(lm(tsTINtotalNA~tsCaricoCODINtotalNA),lwd=2)

text(x=7500,y=5, label=paste("r = ",format(round(corr$estimate,digits=2),nsmall=2,decimal.mark=",")))

