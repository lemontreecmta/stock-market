library(DMwR)
library(zoo)
library(xts)
library(quantmod)
library(randomForest)

#create 9 model formats
model_A<- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1) + myATR(GSPC) 
                           + myADX(GSPC) +    myEMV(GSPC) + myVolat(GSPC)  + myMACD(GSPC) 
                           + mySAR(GSPC) + runMean(Cl(GSPC)) )
model_B <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=6) + 
                         myATR(GSPC) +  
                         EMA(Delt(Cl(GSPC))) +  
                         myVolat(GSPC)  + myMACD(GSPC) + RSI(Cl(GSPC)) + runSD(Cl(GSPC)) + GP
                      )
model_C<- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=2) + 
                         myATR(GSPC) + myADX(GSPC) +   
                         myMACD(GSPC) + myMFI(GSPC) +
                         runMean(Cl(GSPC)) +  CMO(Cl(GSPC)) + 
                         GP)
model_D <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=5) + 
                             myATR(GSPC) +  
                              myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + myEMV(GSPC) + 
                             myMACD(GSPC) + myMFI(GSPC) +
                             runMean(Cl(GSPC)) + 
                             GP)
model_E <- specifyModel(T.ind(GSPC) ~ 
                         myATR(GSPC) +  
                         myCLV(GSPC) + myADX(GSPC) + myEMV(GSPC) + 
                         myMACD(GSPC) + myMFI(GSPC) +
                         runMean(Cl(GSPC)) + 
                         GP)
model_F<- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
                             myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
model_G <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                         myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
                         myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
                         CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                         myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                         mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)) + GP)
model_I<-specifyModel(T.ind(GSPC) ~ myVolat(GSPC) + mySAR(GSPC) 
                     + myATR(GSPC) + myEMV(GSPC) + myBB(GSPC) + myMFI(GSPC) + myCLV(GSPC))
model_H <- specifyModel(T.ind(GSPC) ~ 
                             myATR(GSPC) + myADX(GSPC) + 
                             myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myMACD(GSPC) + myMFI(GSPC) +
                             runMean(Cl(GSPC)) + 
                             GP)

#specify training and testing period
T.train <- as.data.frame(modelData(model_A, data.window=c('1980-01-01','1999-12-31')))
T.eval<-na.omit(as.data.frame(modelData(model_A, data.window=c('2000-01-01','2009-09-15'))))
T.form <- as.formula('T.ind.GSPC ~ .')
#set seed and scale the data (for optimization purpose)
library(nnet)
set.seed(1839) 
data<-scale(T.train)
data.e<-scale(T.eval)
#train the data with feed forward neural network with one hidden layer of size 10
nn<-nnet(T.form, data, size = 10, decay = 0.001, maxit = 5000, linout = T, trace = F)
pred <- predict(nn, data.e)
pred.unscale<-unscale(pred, data.e)
sig<-trading.signals(pred.unscale, 0.005, -0.005)
data.e<-data.e[,1]
true.sig<-trading.signals(data.e, 0.005, -0.005)
#use the model to simulate trading and record loss/ gain
t.1<- trading.simulator(market, sig, 
                              'policy.1', list(exp.prof=0.05, bet = 0.2, hold.time = 30))
summary(t.1)
tradingEvaluation(t.1)
t.2<-trading.simulator(market, sig, 
                         'policy.2', list(exp.prof=0.05, bet = 0.2))
summary(t.2)
tradingEvaluation(t.2)


