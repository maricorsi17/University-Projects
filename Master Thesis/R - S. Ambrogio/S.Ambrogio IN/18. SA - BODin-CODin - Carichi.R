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
tsCaricoBODINtotal_original<-na.approx(xts(x=SAmbrogio[,17], order.by=datestotal))

tsCaricoBODINtotal<-(na.omit(xts(x=SAmbrogio[,17], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoBODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,17], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCaricoBODINtotalNA<-((xts(x=SAmbrogio[,17], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)



tsCaricoCODINtotal<-(na.omit(xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)
MAtsCaricoCODINtotal<-na.omit(rollapply(xts(x=SAmbrogio[,19], order.by=datestotal)*xts(x=SAmbrogio[,6],order.by=datestotal)/1000, width=31, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, fill=NA, align="center"))
tsCaricoCODINtotalNA<-((xts(x=SAmbrogio[,19], order.by=datestotal))*xts(x=SAmbrogio[,6],order.by=datestotal)/1000)

tsTINtotal<-na.approx(xts(x=SAmbrogio[,4], order.by=datestotal))
tsTINtotalNA<-(xts(x=SAmbrogio[,4], order.by=datestotal))


## Creare un oggetto con le date (2015)
tsBODIN2015<-tsCaricoBODINtotal_original["2015"]

## Creare un oggetto con le date (2016)
tsBODIN2016<-tsCaricoBODINtotal_original["2016"]

## Creare un oggetto con le date (2017)
tsBODIN2017<-tsCaricoBODINtotal_original["2017"]

## Creare un oggetto con le date (2018)
tsBODIN2018<-tsCaricoBODINtotal_original["2018"]


# Plottare la time series BOD 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoBODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 4000,by = 500),las=2,format(seq(from = 0,to = 4000,by = 500), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=8,col="grey")
lines(as.zoo(tsCaricoBODINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoBODINtotal[index(tsCaricoBODINtotal)]))

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=3750,label="2015")
text(x=index(tsBODIN2016[182,]),y=3750,label="2016")
text(x=index(tsBODIN2017[182,]),y=3750,label="2017")
text(x=index(tsBODIN2018[91,]),y=3750,label="2018")
legend(x=index(tsBODIN2017[60,]),y=3450, c(expression(paste("BOD"[5-IN])),expression(paste("MA BOD"[5-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")


# Plottare la time series COD 
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoCODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,11000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
axis(side=2,at=seq(from = 0,to = 11000,by = 1000),las=2,format(seq(from = 0,to = 11000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=NA,ny=11,col="grey")
lines(as.zoo(tsCaricoCODINtotal),col="darkslategrey")
lines(as.zoo(MAtsCaricoCODINtotal[index(tsCaricoCODINtotal)]))

abline(v=index(tsBODIN2016[1,]),lwd=2)
abline(v=index(tsBODIN2017[1,]),lwd=2)
abline(v=index(tsBODIN2018[1,]),lwd=2)
text(x=index(tsBODIN2015[182,]),y=10500,label="2015")
text(x=index(tsBODIN2016[182,]),y=10500,label="2016")
text(x=index(tsBODIN2017[182,]),y=10500,label="2017")
text(x=index(tsBODIN2018[91]),y=10500,label="2018")
legend(x=index(tsBODIN2015[60,]),y=9950, c(expression(paste("COD"[IN])),expression(paste("MA COD"[5-IN]," (mensile)"))),col=c("darkslategrey","black"),lty=c(1,1),lwd=c(1,1),bg="white")

# Distribuzione di frequenza percentuale COD (tutti gli anni)


windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse

x <- (unlist(lapply(index(sort(coredata(tsCaricoCODINtotal),decreasing = T)),function(x){x/length(tsCaricoCODINtotal)})))*100
y <- sort(coredata(tsCaricoCODINtotal),decreasing = T)
z <- (unlist(lapply(index(sort(coredata(tsCaricoBODINtotal),decreasing = T)),function(x){x/length(tsCaricoBODINtotal)})))*100
q <- sort(coredata(tsCaricoBODINtotal),decreasing = T)
percentiliCOD<-quantile(y, c(.90, .75, .50))
percentiliBOD<-quantile(q, c(.90, .75, .50))
plot(x, y, type = "l", lwd = 2, xlab="% superamento",ylab=expression(paste("COD"[IN],", BOD"[5-IN]," [kg/d]")),yaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,xlim=c(0,100),ylim=c(0,11000))
axis(side=2,at=seq(from = 0,to = 11000,by = 1000),las=2,format(seq(from = 0,to = 11000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 100,by = 10))
grid(nx=10,ny=11,col="grey")
lines(z,q,type="l",lwd=2,col="orange")
lines(10, percentiliCOD[1], type = "p", pch=19, cex = 2)
lines(25, percentiliCOD[2], type = "p", pch=19, cex = 2)
lines(50, percentiliCOD[3], type = "p", pch=19, cex = 2)
lines(10, percentiliBOD[1], type = "p", pch=19, cex = 2,col="orange")
lines(25, percentiliBOD[2], type = "p", pch=19, cex = 2,col="orange")
lines(50, percentiliBOD[3], type = "p", pch=19, cex = 2,col="orange")
text(x=21,y=percentiliCOD[1]+215,label="90° percentile = 3.643 kg/d")
text(x=36,y=percentiliCOD[2]+215,label="75° percentile = 2.690 kg/d")
text(x=61,y=percentiliCOD[3]+215,label="50° percentile = 1.896 kg/d")
text(x=21,y=percentiliBOD[1]+215,label="90° percentile = 1.678 kg/d")
text(x=36,y=percentiliBOD[2]+215,label="75° percentile = 1.181 kg/d")
text(x=61,y=percentiliBOD[3]+215,label="50° percentile = 834 kg/d")
legend(x=75,y=9500, c(expression(paste("COD"[IN])),expression(paste("BOD"[5-IN]))),col=c("black","orange"),lty=c(1,1),lwd=c(2,2),bg="white")



# Distribuzione di frequenza percentuale COD (senza il 2015)
windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0)) #margini e distanza etichette-asse
tsCODno2015<-tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)>(2015-1900))]
tsBODno2015<-tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)>(2015-1900))]
x <- (unlist(lapply(index(sort(coredata(tsCODno2015),decreasing = T)),function(x){x/length(tsCODno2015)})))*100
y <- sort(coredata(tsCODno2015),decreasing = T)
z <- (unlist(lapply(index(sort(coredata(tsBODno2015),decreasing = T)),function(x){x/length(tsBODno2015)})))*100
q <- sort(coredata(tsBODno2015),decreasing = T)
percentiliCODno2015<-quantile(y, c(.90, .75, .50))
percentiliBODno2015<-quantile(q, c(.90, .75, .50))
plot(x, y, type = "l", lwd = 2, xlab="% superamento",ylab=expression(paste("COD"[IN],", BOD"[5-IN]," [kg/d]")),yaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,xlim=c(0,100),ylim=c(0,11000))
axis(side=2,at=seq(from = 0,to = 11000,by = 1000),las=2,format(seq(from = 0,to = 11000,by = 1000), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 100,by = 10))
grid(nx=10,ny=11,col="grey")
lines(z,q,type="l",lwd=2,col="orange")
lines(10, percentiliCODno2015[1], type = "p", pch=19, cex = 2)
lines(25, percentiliCODno2015[2], type = "p", pch=19, cex = 2)
lines(50, percentiliCODno2015[3], type = "p", pch=19, cex = 2)
lines(10, percentiliBODno2015[1], type = "p", pch=19, cex = 2,col="orange")
lines(25, percentiliBODno2015[2], type = "p", pch=19, cex = 2,col="orange")
lines(50, percentiliBODno2015[3], type = "p", pch=19, cex = 2,col="orange")
text(x=21,y=percentiliCODno2015[1]+215,label="90° percentile = 3.020 kg/d")
text(x=36,y=percentiliCODno2015[2]+215,label="75° percentile = 2.212 kg/d")
text(x=61,y=percentiliCODno2015[3]+215,label="50° percentile = 1.424 kg/d")
text(x=21,y=percentiliBODno2015[1]+315,label="90° percentile = 1.247 kg/d")
text(x=36,y=percentiliBODno2015[2]+215,label="75° percentile = 985 kg/d")
text(x=61,y=percentiliBODno2015[3]+215,label="50° percentile = 724 kg/d")
legend(x=75,y=9500, c(expression(paste("COD"[IN])),expression(paste("BOD"[5-IN]))),col=c("black","orange"),lty=c(1,1),lwd=c(2,2),bg="white")


# GRAFICO CARICO COD E T
windows(width = 16,height = 9)
par(mar=c(6,6,4,6),mgp=c(4,1,0)) #margini e distanza etichette-asse

plot(as.zoo(tsCaricoCODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("COD"[IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,10000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
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

plot(as.zoo(tsCaricoBODINtotal),type="n",  xlab="Mesi",ylab=expression(paste("BOD"[5-IN]," [kg/d]")),yaxt="n",xaxt="n",yaxs="i",xaxs="i",cex.lab=1.2,ylim=c(0,4000),col="grey")
drawTimeAxis(as.zoo(tsCaricoBODINtotal_original), tick.tstep ="months", lab.tstep ="months",las=2,lab.fmt="%m")
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
text(x=index(tsBODIN2016[182,]),y=29,label="2016")
text(x=index(tsBODIN2017[182,]),y=29,label="2017")
text(x=index(tsBODIN2018[91,]),y=29,label="2018")
legend(x=index(tsBODIN2017[240,]),y=28, c(expression(paste("BOD"[5-IN])),expression(paste("MA BOD"[5-IN]," (mensile)")),expression(paste("T"[IN]))),col=c("darkslategrey","black","orange"),lty=c(1,1,1),lwd=c(1,1,1),bg="white")

# BOXPLOT
windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoBODINtotal[which(.indexyear(tsCaricoBODINtotal)>(2015-1900))]),yaxt="n",ylab="[kg/d]",main=expression(paste("BOD"[5-IN])), ylim=c(0,2600),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 2600,by = 200),las=2,labels = format(seq(from = 0,to = 2600,by = 200), big.mark = ".", decimal.mark = ","))

windows(width = 6,height = 6)
par(mar=c(1,6,4,4),mgp=c(4,1,0))

boxplot(coredata(tsCaricoCODINtotal[which(.indexyear(tsCaricoCODINtotal)>(2015-1900))]),yaxt="n",ylab="[kg/d]",main=expression(paste("COD"[IN])), ylim=c(0,11000),cex.lab=1.5,cex.main=2)
axis(side=2,at=seq(from = 0,to = 11000,by = 1000),las=2,labels = format(seq(from = 0,to = 11000,by = 1000), big.mark = ".", decimal.mark = ","))


# SCATTERPLOT
corr<-cor.test(coredata(tsCaricoBODINtotalNA),coredata(tsTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCaricoBODINtotal),as.zoo(tsTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("BOD"[5-IN]," [kg/d]")),ylab=expression(paste("T"[IN]," [",degree,"C]")),cex.lab="1.2",xlim=c(0,3000),ylim=c(0,30),pch=16)
axis(side=2,at=seq(from = 0,to = 30,by = 10),las=2,format(seq(from = 0,to = 30,by = 10), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 3000,by = 1000),las=1,format(seq(from = 0,to = 3000,by = 1000), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=3,col="grey")

abline(lm(tsTINtotalNA~tsCaricoBODINtotalNA),lwd=2)

text(x=2500,y=5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))


corr<-cor.test(coredata(tsCaricoCODINtotalNA),coredata(tsTINtotalNA),method = "spearman", use="complete.obs",exact = F)

windows(width = 16,height = 9)
par(mar=c(6,6,4,4),mgp=c(4,1,0))
plot(as.zoo(tsCaricoCODINtotal),as.zoo(tsTINtotal),xaxs="i",yaxt="n",xaxt="n",yaxs="i",xlab=expression(paste("COD"[IN]," [kg/d]")),ylab=expression(paste("T"[IN]," [",degree,"C]")),cex.lab="1.2",xlim=c(0,12000),ylim=c(0,30),pch=16)
axis(side=2,at=seq(from = 0,to = 30,by = 10),las=2,format(seq(from = 0,to = 30,by = 10), big.mark = ".", decimal.mark = ","))
axis(side=1,at=seq(from = 0,to = 12000,by = 4000),las=1,format(seq(from = 0,to = 12000,by = 4000), big.mark = ".", decimal.mark = ","))
grid(nx=3,ny=3,col="grey")

abline(lm(tsTINtotalNA~tsCaricoCODINtotalNA),lwd=2)

text(x=10000,y=5, label=paste("r = ",format(round(corr$estimate,digits=2),decimal.mark=",")))
