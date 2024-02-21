# Data Mining
# Cameron Ervin
# Decision Boundary

load("~/R Workbook - 5331 Data Mining/mini_brest_cancer.RData")

# 1
norm_func <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

breast_cancer$radius <- norm_func(breast_cancer$radius)
breast_cancer$texture <- norm_func(breast_cancer$texture)

head(breast_cancer)

# 2
install.packages("FNN")
library(FNN)
myData <- breast_cancer
trainLabel <- myData[-trainIdx, 3]
pred <- knn(breast_cancer[,-3], test, breast_cancer[,3], k=10)

plot(test[ ,1], test[ ,2],
     pch = 19,
     cex = 0.5,
     col = pred, # class 0: black, class 1: red
     xlab = "radius",
     ylab = "texture")

# 3
pred2 <- knn(breast_cancer[,-3], test, breast_cancer[,3], k=1)

plot(test[ ,1], test[ ,2],
     pch = 19,
     cex = 0.5,
     col = pred2, # class 0: black, class 1: red
     xlab = "radius",
     ylab = "texture")
# the boundary of the 1 nearest neighbors model is not as defined as the boundary of the 
# 10 nearest neighbors model

# 4 
install.packages("tree")
install.packages("randomForest")

library(tree)
breast_cancer_norm <- cbind(breast_cancer_norm, breast_cancer[,3])
treeModel <- tree(breast_cancer[,3], breast_cancer_norm)

m <- prune.tree(treeModel, best = 5)
pred3 <- predict(m, validation, type = "class")

plot(test[ ,1], test[ ,2],
     pch = 19,
     cex = 0.5,
     col = pred3, # class 0: black, class 1: red
     xlab = "radius",
     ylab = "texture")