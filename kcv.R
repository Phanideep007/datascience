install.packages("caret")
install.packages("e1071")
library(e1071)
data1 <- read.csv("D:/analytics/credit.csv")
library(caret)
control <- trainControl(method = "cv", number = 5)
CARTModel <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose + amount + savings_balance + employment_duration + percent_of_income + years_at_residence
                   + age + other_credit + housing + existing_loans_count + job + dependents + phone, 
                   data = data1, trControl = control, method = "rpart") # for cross validation train is used
pred <- predict(CARTModel, data1)
CrossTable(data1$default, pred, chisq = TRUE)
