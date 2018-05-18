#Training & Test Distinction
letters <- read.csv("D:/analytics/Machine Learning/letterdata.csv")
letters
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

#Using SVM Package for Prediction
install.packages("kernlab")
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
# linearly seperable part - vaniladot#
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))*100

#improving Model
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))*100
