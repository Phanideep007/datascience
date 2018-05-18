credit <- read.csv("D:/analytics/credit.csv")
str(credit)

#Checking column count
table(credit$checking_balance)
table(credit$savings_balance)
head(credit)

#Creating Training & Test Data
samp <- sample(1000,700)
train_set <- credit[samp,]
test_set <- credit[-samp,]

#Checking how many defaults are there in data
prop.table(table(credit$default))
prop.table(table(train_set$default))
prop.table(table(test_set$default))

#Installing c50 package
install.packages("C50")
library(C50)

#Building Model
model <- C5.0(train_set[-17], train_set$default)
model
summary(model)

#prediction
pred <- predict(model,test_set)
library(gmodels)
CrossTable(test_set$default, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#Boosting using Trials parameters
boost <- C5.0(train_set[-17], train_set$default, trials = 10)
boost
summary(boost)

#Prediction using Boosting
pred_boost <- predict(boost,test_set)
CrossTable(test_set$default, pred, prop.chisq = TRUE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

