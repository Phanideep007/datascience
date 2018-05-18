install.packages("rpart")
library(rpart)
data1 <- read.csv("D:/analytics/credit.csv")

a <- sample(1000,700)
train <- data1[a,]
test <- data1[-a,]  #remianing values depict -a
summary(data1)
str(data1)

CARTModel <- rpart(default ~ checking_balance + months_loan_duration + credit_history 
                   + purpose + amount + savings_balance + employment_duration 
                   + percent_of_income + years_at_residence
                   + age + other_credit + housing + existing_loans_count + job + dependents + phone, 
                   data = train)
summary(CARTModel)

library(rpart.plot)
library(rattle)
fancyRpartPlot(CARTModel)

purchase_pred_train <-predict(CARTModel, train ,type ="class")

library(gmodels)
CrossTable(train$default, purchase_pred_train,
           prop.chisq =FALSE, prop.c =FALSE, prop.r =FALSE,
           dnn =c('actual default', 'predicted default'))
