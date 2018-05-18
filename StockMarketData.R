library(quantmod)

### Get Symbols
symbolList =c('HDFCBANK.BO','HEROMOTOCO.BO','ICICIBANK.BO','ASIANPAINT.BO','INFY.BO',
              'TCS.BO','WIPRO.BO','NTPC.BO','ITC.BO','RELIANCE.BO','ONGC.BO','MARUTI.BO','^BSESN','^NSEI')

### Get all the data for the symbols in one go
Stocks1 = lapply(symbolList, function(sym){
  dailyReturn(na.omit(getSymbols(sym, auto.assign = FALSE)))
  })
### Merge all into 1
Stocks2 = do.call(merge.xts,Stocks1)

### Rename Columns
colnames(Stocks2) =c('HDFCBANK.BO','HEROMOTOCO.BO','ICICIBANK.BO','ASIANPAINT.BO','INFY.BO',
                    'TCS.BO','WIPRO.BO','NTPC.BO','ITC.BO','RELIANCE.BO','ONGC.BO','MARUTI.BO','BSESN','NSEI')

### Remove NA rows
sort(colSums(is.na(Stocks2)),decreasing=T)
Stocks3=data.frame(Stocks2)
Stocks3=na.omit(Stocks3)
Stocks3=Stocks3[-14]

plot(Stocks3$BSESN,type="l")
sort(colSums(is.na(Stocks3)),decreasing=T)
library(corrplot)
M <- cor(Stocks3)
corrplot(M, method="number")


#### Linear Reg

stockreg=lm(BSESN~.,data=Stocks3)
summary(stockreg)

plot(stockreg$fitted.values, type = 'l', col = 'blue')
lines(Stocks3$BSESN, col="red")
lines(Stocks3$ICICIBANK.BO, col="orange")

RMSE = sqrt(mean((stockreg$residuals)^2))
RMSE

##### Neural Net

install.packages(neuralnet)
library(neuralnet)

formula=paste(names(Stocks3), collapse = '+')
formula
stocknn=neuralnet(BSESN~HDFCBANK.BO+HEROMOTOCO.BO+ICICIBANK.BO+ASIANPAINT.BO+INFY.BO+
                    TCS.BO+WIPRO.BO+NTPC.BO+ITC.BO+RELIANCE.BO+ONGC.BO+MARUTI.BO,
                  data=Stocks3,hidden=6)
plot(stocknn)

plot(stocknn$response,type = 'l', col = 'blue')
lines(Stocks3$BSESN, col='red')

RMSE2 = sqrt(mean((stocknn$response-Stocks3$BSESN)^2))

prediction(stocknn)


####Think this as a  Classification problem, create a new column

Stocks3$move = ifelse(Stocks3$BSESN >0, 1,0)
table(Stocks3$move)

stlogit=glm(as.factor(move)~.,data=Stocks3[-13],family="binomial")

summary(stlogit)

table(stlogit$fitted.values>0.5, Stocks3$move)

###### Using NN - Classification

movenn=neuralnet(move~HDFCBANK.BO+HEROMOTOCO.BO+ICICIBANK.BO+ASIANPAINT.BO+INFY.BO+
                    TCS.BO+WIPRO.BO+NTPC.BO+ITC.BO+RELIANCE.BO+ONGC.BO+MARUTI.BO,
                  data=Stocks3,hidden=6)
plot(movenn)
table(movenn$response, Stocks3$move)
prediction(movenn)

#### Using Decision tree

library(rpart)
library(rpart.plot)

dtree=rpart(factor(move)~., data=Stocks3[-13])
rpart.plot(dtree)
rpredict=predict(dtree,type="class")
table(rpredict,Stocks3$move)



####### Using Random Forest

library(randomForest)

rf=randomForest(factor(move)~., data=Stocks3[-13],ntree=1800, type='classification', importance=T,do.trace=1000,proximity=TRUE)
print(rf)

varImpPlot(rf)

plot(rf)

#### Using Support Vector Machine

library(e1071)


svmmodel=svm(factor(move)~.,data=Stocks3[-13],kernel="sigmoid")

summary(svmmodel)
plot(svmmodel)


pred <- fitted(svmmodel)


# Check accuracy:
table(pred, Stocks3$move)



### Using GBM
library(gbm)


Stocks4=Stocks3[-13]
gbmmod=gbm(move~.,data=Stocks4,n.trees=5000,distribution = 'bernoulli',cv.folds=5)

summary(gbmmod)
print(gbmmod)
bestiter=gbm.perf(gbmmod,method = 'cv')


gbmp=predict(gbmmod,type = "response",n.trees = bestiter)
table(gbmp>0.5, Stocks4$move)


#### Using XGBM

library(xgboost)


bst <- xgboost(label=as.matrix(Stocks4$move), data = as.matrix(Stocks4),  max_depth = 3, eta = 1, 
               nrounds = 3, objective = "binary:logistic")



### Charting and Graphing

chartSeries(BSESN,type="candlesticks")
zoomChart("2017-01-01::")
addSMA(50)
addSMI(100)
addBBands()
addRSI()
addCMO()
addCMF()
addMACD()
addVolatility()


chartSeries(INFY.BO,type="candlesticks")

addSMA(50,col="skyblue")
addSMA(200,col="yellow")
addWMA(200, col='pink')
add_EMA()
zoomChart("2017-10-01::")


#dataEnv <- new.env()
#getSymbols(symbolList, env=dataEnv)
#plist <- eapply(dataEnv, Ad)
#pframe <- do.call(merge, plist)
#Set start date
#start_date=as.Date("2014-01-01")
#Create New environment to contain stock price data
#dataEnv<-new.env()
#download data    
#getSymbols(symbolList)
#plot(HDFCBANK.BO)
#getSymbols(symbolList,env=dataEnv)
#finalData=data.frame(as.xts(merge(HDFCBANK.BO,HEROMOTOCO.BO,ICICIBANK.BO,ASIANPAINT.BO,INFY.BO,TCS.BO,WIPRO.BO,NTPC.BO,ITC.BO,RELIANCE.BO,SBIN.BO,ONGC.BO,MARUTI.BO)))
#allReturns(as.zoo(dataEnv))