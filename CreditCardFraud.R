##Importing the libraries
library(caret)
library(class)
library(e1071)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)

### Get the CC data 
cc <- read.csv("F:/Financial Analytics/creditcard.csv")
str(cc)

## As "Class" is of a class "integer", the factor transformation is performed:
cc$Class <- factor(cc$Class)
set.seed(1256)
sampled <- sample(1:nrow(cc), round(0.2*nrow(cc)))
cc <- cc[sampled, ]
index <- createDataPartition(cc$Class, p = 0.75, list = F)
train <- cc[index, ]
test <- cc[-index, ]

#### Apply the KNN algorithm
knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 5)
confusionMatrix(knn1, test$Class, positive = "1")

### Now using the naive Bayes
bayesmodel <- naiveBayes(Class~., data = train, laplace = 1)
bayesmodel$apriori

### Doing the prediction
pred <- predict(bayesmodel, test)
confusionMatrix(pred, test$Class, positive = "1")

### Graphing the result rate
rawpred <- predict(bayesmodel, test, type = "raw")
ptest <- prediction(rawpred[,2], test$Class)
perf <- performance(ptest, "tpr", "fpr")
plot(perf, colorize = T)

###Using the Randomforest model

rf.model <- randomForest(Class ~ ., data = train, ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, test)
confusionMatrix(test$Class, rf.predict)
varImpPlot(rf.model)
plot.randomForest()


### Using the SVM Model

svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test)
confusionMatrix(test$Class, svm.predict)


####
