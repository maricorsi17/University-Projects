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
tsBODINtotal_original<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))

tsBODINtotal<-na.omit(xts(x=SAmbrogio[,17], order.by=datestotal))
MAtsBODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,17], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsBODINtotalNA<-(xts(x=SAmbrogio[,17], order.by=datestotal))

tsCODINtotal<-na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))
MAtsCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal), width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCODINtotalNA<-(xts(x=SAmbrogio[,19], order.by=datestotal))



## Creare un oggetto con le date (2015)
tsBODIN2015<-tsBODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsBODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsBODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsBODINtotal_original["2018"]


# Plottare la time series 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsBODINtotal),type="n", xlab="Mesi",ylab=expression(paste("BOD"[5-IN],", COD"[IN]," [mg/L]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsBODINtotal),col="darkslategrey")
lines(as.zoo(tsCODINtotal),col="orange")
lines(as.zoo(MAtsBODINtotal[index(tsBODINtotal)]))
lines(as.zoo(MAtsCODINtotal[index(tsCODINtotal)]),col="red")

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=3750,label="2015")
text(x=index(tsBODIN2016[182,]),y=3750,label="2016")
text(x=index(tsBODIN2017[182,]),y=3750,label="2017")
text(x=index(tsBODIN2018[90,]),y=3750,label="2018")
legend(x=index(tsBODIN2015[40,]),y=3450, c(expression(paste("BOD"[5-IN])),expression(paste("COD"[IN])),expression(paste("MA BOD"[5-IN]," (mensile)")), expression(paste("MA COD"[IN]," (mensile)"))),col=c("darkslategrey","orange","black","red"),lty=c(1,1,1,1),lwd=c(1,1,1,1),bg="white")


# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsBODINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("BOD"[5-IN])), ylim=c(0,800),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 800,by = 100),las=2,labels = format(seq(from = 0,to = 800,by = 100), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCODINtotal),yaxt="n",ylab="[mg/L]",main=expression(paste("COD"[IN])), ylim=c(0,3600),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,labels = format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))

# SCATTERPLOT
corr<-cor.test(coredata(tsBODINtotalNA),coredata(tsCODINtotalNA),method = "spearman", use="complete.obs",exact = F)


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsBODINtotal),as.zoo(tsCODINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [mg/L]")),ylab=expression(paste("COD"[IN]," [mg/L]")),cex.lab="1.2",xlim=c(0,1000),ylim=c(0,2000),pch=16)
axis(side=2,at=seq(from = 0,to = 2000,by = 500),las=2,format(seq(from = 0,to = 2000,by = 500), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 1000,by = 500),las=1,format(seq(from = 0,to = 1000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=2,ny=4,col="grey")
abline(lm(tsCODINtotalNA~tsBODINtotalNA),lwd=2)

text(x=150,y=1750, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


# df<-data.frame(coredata(tsBODINtotalNA),coredata(tsCODINtotalNA))
# colnames(df)<-c("BOD","COD")
# 
# windows(width = 16,height = 9)
# par(mar=c(6,6,4,6),mgp=c(4,1,0))
# ggscatter(df, x = "BOD", y = "COD",
#           add = "reg.line", conf.int = F,
#           cor.coef = TRUE, cor.method = "spearman",
#           xlab = "BOD", ylab = "COD")
