load("~/R Workbook - 5331 Data Mining/fashion_mnist.RData")

# 300 entries for test data and 700 entries for train data

table(train$label)
# dataset is decently balanced

train$label <- as.factor(train$label)
test$label <- as.factor(test$label)

library(e1071)
myData <- train[ , -1]
svm1 <- svm(label ~ ., train)
svm1_pred <- predict(svm1, test)

svm2 <- svm(label ~., train,
            kernel = "linear")

install.packages("randomForest")
install.packages("tree")
library(randomForest)

treeModel <- tree(label ~., train)


# best classification rate is around 15-26%

# number of factors like number of hidden layers, etc
