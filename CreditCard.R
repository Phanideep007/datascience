##Importing the libraries
library(caret)
library(class)
library(e1071)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)

### Get the CC data 
cc <- read.csv(file.choose())
str(cc)

## As "Class" is of a class "integer", the factor transformation is performed:
cc$Creditability <- factor(cc$Creditability)
set.seed(1256)
sampled <- sample(1:nrow(cc), round(0.2*nrow(cc)))
cc <- cc[sampled, ]
index <- createDataPartition(cc$Creditability, p = 0.75, list = F)
train <- cc[index, ]
test <- cc[-index, ]

#### Apply the KNN algorithm
knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Creditability, k = 5)
confusionMatrix(knn1, test$Creditability, positive = "1")

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



### Using the SVM Model

svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test)
confusionMatrix(test$Class, svm.predict)


####




#H2o

###

library(h2o)
library(randomForest)


### Get the CC data 
cc <- read.csv(file.choose())
str(cc)

### Removing the time columns
cc=cc[-1]

### using Kmeans

cckmeans = kmeans(cc,6)
plot(cc, col=cckmeans$cluster)


### Initialising H2O

h2o.init(ip="localhost", port=54321,max_mem_size="2650m")

response="Creditability"
features = setdiff(colnames(cc),y)

model_nn <-h2o.deeplearning(x= features,
                            y=response,
                            training_frame= as.h2o(cc),
                            model_id="model_nn",
                            autoencoder=F,
                            reproducible=TRUE,
                            ignore_const_cols=FALSE,
                            seed=42,
                            hidden = c(10,10,10),
                            epochs=100,
                            distribution ="AUTO",
                            activation='Tanh')

model_nn