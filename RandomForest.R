data1 <- read.csv("D:/analytics/credit.csv")
a <- sample(1000,700)
train1 <- data1[a,]
test <- data1[-a,]
summary(data1)
str(data1)


control <- trainControl(method="repeatedcv", number=5, repeats=2)
rfModel <- train(default ~., data = train1, method='rf', trControl = control)
#~. mean its takes all the coumns into consideration #
purchase_pred_train <-predict(rfModel, train1)

library(gmodels)
CrossTable(train1$default, purchase_pred_train,
           prop.chisq =TRUE, prop.c =FALSE, prop.r =FALSE,
           dnn =c('actual default', 'predicted default'))

purchase_pred_test <-predict(rfModel, test)
CrossTable(test$default, purchase_pred_test,
           prop.chisq =TRUE, prop.c =FALSE, prop.r =FALSE,
           dnn =c('actual default', 'predicted default'))


#Bagging#
BagModel <- train(default ~., data = train1, method='treebag', trControl = control)
purchase_pred_test_bag <-predict(BagModel, test)
CrossTable(test$default, purchase_pred_test_bag,
           prop.chisq =TRUE, prop.c =FALSE, prop.r =FALSE,
           dnn =c('actual default', 'predicted default'))           
