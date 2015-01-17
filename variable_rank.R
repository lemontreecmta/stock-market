library(DMwR)
library(zoo)
library(xts)
library(quantmod)
library(randomForest)

#read the provided dataset
GSPC<-as.xts(read.zoo("sp500.csv", header = T))
#reformat existing variables (technical indicators) from GSPC dataset
myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]

#import dataset of new variables from website Quandl.com and reformat them
HousePrice<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/USSTHPI.csv?&trim_start=1975-01-01&trim_end=2013-10-01&sort_order=desc', colClasses=c('Date'='Date'))
GoldPrice<-read.csv('http://www.quandl.com/api/v1/datasets/LBMA/GOLD.csv?&trim_start=1968-01-02&trim_end=2014-06-11&sort_order=desc', colClasses=c('Date'='Date'))
PersonalComs<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/PCE.csv?&trim_start=1959-01-01&trim_end=2014-03-01&sort_order=desc', colClasses=c('Date'='Date'))
CrudeOil<-read.csv('http://www.quandl.com/api/v1/datasets/CHRIS/CME_CL1.csv?&trim_start=1983-03-30&trim_end=2014-06-11&sort_order=desc', colClasses=c('Date'='Date'))
EffectiveFed<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/DFF.csv?&trim_start=1954-07-01&trim_end=2014-06-10&sort_order=desc', colClasses=c('Date'='Date'))

PCE<-PersonalComs[PersonalComs$Date>'1970-01-01',]
PCE<-xts(PCE[,2], as.POSIXct(PCE[,1]))
GP<-GoldPrice[GoldPrice$Date>'1970-01-01',]
GP<-xts(GP[,2], as.POSIXct(GP[,1]))
CO<-xts(CrudeOil[,2], as.POSIXct(CrudeOil[,1]))
EFR<-xts(EffectiveFed[,2], as.POSIXct(EffectiveFed[,1]))
HousePrice<-xts(HousePriceIndex[,2], as.POSIXct(HousePriceIndex[,1]))
IR<-xts(InflationRate[,2], as.POSIXct(InflationRate[,1]))
CPI<-xts(CPI[,2], as.POSIXct(CPI[,1]))
PGE<-xts(Price_Gas_Export[,2], as.POSIXct(Price_Gas_Export[,1]))

#After getting all variables, rank them in the order of importance based upon different measures 
#Using correlation: 
comC<-cbind(Cl(GSPC)[,1], T.ind(GSPC)[,1], myATR(GSPC)[,1], 
            mySMI(GSPC)[,1], myADX(GSPC)[,1], myAroon(GSPC)[,1], 
            myBB(GSPC)[,1], myChaikinVol(GSPC)[,1],myCLV(GSPC)[,1], 
            CMO(Cl(GSPC))[,1], myEMV(GSPC)[,1], myVolat(GSPC)[,1], 
            myMACD(GSPC)[,1], myMFI(GSPC)[,1], RSI(Cl(GSPC))[,1], 
            mySAR(GSPC)[,1], runMean(Cl(GSPC))[,1], runSD(Cl(GSPC))[,1],
            HousePrice[,1], GP[,1], PCE[,1], CO[,1], EFR[,1])
colnames(comC)<-c("Close", "T.ind", "ATR", 
                  "SMI", "ADX", "Aroon", 
                  "BB", "ChaikinVol", "CLV", 
                  "CMO", "EMV", "Volat", 
                  "MACD", "MFI", "RSI", 
                  "SAR", "runMean", "runSD",
                  "House Price", "Gold Price", "Personal Consumption Exp", 
                  "Crude Oil", "Effective Federal Funds Rate")
symnum(cor(comC[,1:23], use = 'complete.obs'))

##Use random forest to rank variables 

data.model <- specifyModel(T.ind(GSPC) ~ 
                             myATR(GSPC) + myADX(GSPC) + 
                             myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myMACD(GSPC) + myMFI(GSPC) +
                             runMean(Cl(GSPC)) + 
                               GP + HousePrice + PCE + IR + CPI)
set.seed(2000)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c(index(GSPC["1980-01-02"]),index(GSPC["1999-12-31"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type = 1)
imp<-importance(rf@fitted.model, type = 1)
rownames(imp)[which(imp>8)]

#Using information gain algorithm to rank variables
#use partition clustering to discretize data. The number of clusters is determined by from plots of sum of squares for all possible number of clusters. 
#atr
atr.data<-na.omit(myATR(GSPC)$atr)
fit<-kmeans(atr.data, 6)
atr.data<-data.frame(atr.data, fit$cluster)
smi.data<-na.omit(mySMI(GSPC)$SMI)
fit<-kmeans(smi.data, 8)
smi.data<-data.frame(smi.data, fit$cluster)
adx.data<-na.omit(myADX(GSPC)$ADX)
fit<-kmeans(adx.data, 10)
adx.data<-data.frame(adx.data, fit$cluster)
aroon.data<-na.omit(myAroon(GSPC)$oscillator)
fit<-kmeans(aroon.data, 5)
aroon.data<-data.frame(aroon.data, fit$cluster)
bb.data<-na.omit(myBB(GSPC)$pctB)
fit<-kmeans(bb.data, 12)
bb.data<-data.frame(bb.data, fit$cluster)
chaikinvol.data<-na.omit(myChaikinVol(GSPC)$Delt.1.arithmetic)
fit<-kmeans(chaikinvol.data, 2)
chaikinvol.data<-data.frame(chaikinvol.data, fit$cluster)
clv.data<-na.omit(myCLV(GSPC)$clv.EMA.10)
fit<-kmeans(clv.data, 13)
clv.data<-data.frame(clv.data, fit$cluster)
emv.data<-na.omit(myEMV(GSPC)$maEMV)
fit<-kmeans(emv.data, 15)
emv.data<-data.frame(emv.data, fit$cluster)
macd.data<-na.omit(myMACD(Cl(GSPC))$signal)
fit<-kmeans(macd.data, 15)
macd.data<-data.frame(macd.data, fit$cluster)
mfi.data<-na.omit(myMFI(GSPC)$mfi)
fit<-kmeans(mfi.data, 13)
mfi.data<-data.frame(mfi.data, fit$cluster)
sar.data<-na.omit(mySAR(GSPC)[,1])
fit<-kmeans(sar.data, 11)
sar.data<-data.frame(sar.data, fit$cluster)
volat.data<-na.omit(myVolat(GSPC)[,1])
fit<-kmeans(volat.data, 13)
volat.data<-data.frame(volat.data, fit$cluster)
atr.data<-na.omit(myATR(GSPC)$atr)
fit<-kmeans(atr.data, 6)
atr.data<-data.frame(atr.data, fit$cluster)
atr.f<-atr.data["1970-02-01"]

## discretize T indicator to sell/hold/buy order

T.ind <- function(quotes,tgt.margin=0.001,n.days=5) {
  v <- apply(HLC(quotes),1,mean)
  
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
    for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)
  
  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

t.data<-na.omit(T.ind(GSPC)[,1])
wss <- (nrow(t.data)-1)*sum(apply(t.data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(t.data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
fit<-kmeans(t.data, 15)
t.data<-data.frame(t.data, fit$cluster)
#all items = 9995 values before and at 2009-09-08
#get all together
#reformat data
atr.final<-factor(first(last(atr.data$fitcluster, 10000), 9995))
aroon.final<-factor(first(last(aroon.data$fitcluster, 10000), 9995))
bb.final<-factor(first(last(bb.data$fitcluster, 10000), 9995))
chaikinvol.final<-factor(first(last(chaikinvol.data$fitcluster, 10000), 9995))
clv.final<-factor(first(last(clv.data$fitcluster, 10000), 9995))
emv.final<-factor(first(last(emv.data$fitcluster, 10000), 9995))
mfi.final<-factor(first(last(mfi.data$fitcluster, 10000), 9995))
sar.final<-factor(first(last(sar.data$fitcluster, 10000), 9995))
volat.final<-factor(first(last(volat.data$fitcluster, 10000), 9995))
t.final<-factor(last(t.data, 9995))

InfoGainAttributeEval(t.final~atr.final + aroon.final + bb.final + chaikinvol.final 
                      + clv.final + emv.final + mfi.final + sar.final + volat.final)

