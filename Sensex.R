## Install quantmod package
library(quantmod)
getSymbols("^BSESN") #to extract BSESN data of 10 years


#golden crossver-shortterm MA crosses over Longterm MA
#death crossover-vice versa
#investopedia website for financial analytics

symbols=c('HDFCBANK.BO','HINDUNILVR.BO','SBIN.BO','TCS.BO',
          'ITC.BO','RELIANCE.BO','ONGC.BO','MARUTI.BO','^BSESN') 
#Type company name in yahoo finance in search and the search for .BO extension file

getSymbols(symbols)

#merge all data sets into single

finaldata <- data.frame(as.xts(merge(HDFCBANK.BO,HINDUNILVR.BO,SBIN.BO,TCS.BO,
                                     ITC.BO,RELIANCE.BO,ONGC.BO,MARUTI.BO,BSESN)))



#Calculation of daily returns

stocks=lapply(symbols, function(sym) {
  dailyReturn(na.omit(getSymbols(sym, auto.assign = FALSE)))
})
stocks1=do.call(merge.xts,stocks)
colnames(stocks1)=c('HDFCBANK','HINDUNILVR','SBIN','TCS',
                    'ITC','RELIANCE','ONGC','MARUTI','SENSEX')
stocks2=na.omit(stocks1)
sort(colSums(is.na(stocks1)),decreasing=T) #to find no. of null values
stocks3=data.frame(stocks1)
stocks3=stocks3[-14] #removing NIFTY as it is havng lots of missing variables
stocks4=na.omit(stocks3)
plot(stocks3$SENSEX,type="l")
cor(stocks4)
#corplot for plotting corr
library(corrplot)
corrplot(cor(stocks4),method='number')
stockreg=lm(SENSEX~.,data=stocks4)
summary(stockreg)
plot(stockreg$fitted.values,type="l",col="blue")
lines(stocks4$SENSEX,col="red")
lines(stocks4$INFY,col="green")
sqrt(mean((stockreg$residuals)^2))
formula=paste(names(stocks4),collapse='+')
formula

#neural net building model
stocknn=neuralnet(SENSEX~HDFCBANK.BO+HINDUNILVR.BO+SBIN.BO+TCS.BO+
                    ITC.BO+RELIANCE.BO+ONGC.BO+MARUTI.BO,data=stocks4,hidden=5)
plot(stocknn)

#predict values
stocknn$response

sqrt(mean((stocks4$SENSEX-stocknn$response)^2))  #since rmse is 0 predicted 100%




######### Classification TYPE ####################

stocks4$move=ifelse(stocks4$SENSEX>0,"Up","Down")
table(stocks4$move)  #dailyreturn is positive-up, dailyreturn is negative-down

stocklogit <- glm(factor(move)~.,data=stocks4[-13],family="binomial") #logistic regression, remove last column, make factor
summary(stocklogit)
table(stocks4$move,stocklogit$fitted.values>0.5) #fr confusion matrix


#install rpart for decision tree
stockrpart=rpart(factor(move)~.,data=stocks4[-13])
summary(stockrpart) #Variable importance
rpart.plot(stockrpart)
rpartpredict=predict(stockrpart,type="class")
table(rpartpredict,stocks4$move)


#RadnomForest
stockrandomforest <- randomForest(factor(move)~.,data=stocks4[-13],ntree=3000,importance=T,do.trace=200)
print(stockrandomforest) #error rate
#Hence 90%
varImpPlot(stockrandomforest)
plot(stockrandomforest) #to find out the point where we get a straight line if not give more number of trees

#kernlab package install for svm
stocksvm <- ksvm(factor(move)~.,data=stocks4[-13])
print(stocksvm) #training erro is 0.07, hence 93% accuracy
table(fitted(stocksvm),stocks4$move)


#GBM
#install gbm package
stocks5=stocks4[-13]
stocks5$Move=ifelse(stocks5$move=="Up",1,0)
stockgbm=gbm(Move~.,data=stocks5[-13],n.trees=1000,distribution="bernoulli",cv.folds=3)
#bernoulli requires reaponse to be 0,1 hence rename the y
print(stockgbm)
bestiter=gbm.perf(stockgbm,method="OOB") #we can use CV insteead of OOB, then we need to give cv.folds in model
#to check the graph where it is straightning ,if not increase number of trees
gbmpredict=predict(stockgbm,type="response",n.trees=bestiter)
table(gbmpredict>0.5,stocks5$Move)


