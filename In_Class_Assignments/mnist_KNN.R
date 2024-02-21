# Data Mining
# Cameron Ervin
# In Class Exercise mnist
load("~/R Workbook - 5331 Data Mining/mnist_data.RData")
# Part 1:
# 1. How many images does training set have?
rowsX <- nrow(Xtrain)
rowsX
# If each row is an image, the training set has 1000 rows and therefore 1000 images.
# 2. Save first image in the training set as firstImg. Which digit does this image show?
firstImg <- Xtrain[1, ]
firstImg
# the image shows a 5
# 3. Create (x,y) coordinate for each pixel of the image by running the following code:
install.packages("ggplot2")
library(ggplot2)
w <- 28
h <- 2
myImg <- data.frame(xVal = rep(1:w, h),
                    yVal = rep(h:1, each = w),
                    rgb = firstImg/255)
# 4. Plot the image using the function ggplot():
g <- ggplot(data = myImg, aes(x = xVal, y = yVal))
g + geom_point(colour = rgb(myImg[c("rgb","rgb","rgb")]))

# Part 2:
# 1. We are going to apply the nearest neighbor method to predict hand written digits.
# Do you think it's a suitable choice of method? Explain.
# Yes I think the method is suitable because KNN is suitable with large amounts of data where labels aren't available like handwriting.
# 2. Using 5 nearest neighbor method (K = 5), what is miss-classification rate?
install.packages("FNN")
library(FNN)
?knn
pred <- knn(Xtrain, Xtest, Ytrain, k=5)
mean(pred != Ytest)
# miss-classification rate = 0.1533
# 3. Print the confustion matrix. Are there any digits that are harder to predict than others?
table(pred, Ytest)
# 4s seem to be harder to predict than other numbers since they are often miss-classified as 9s.
# 4. Implement 5-fold cross validation to find the optimal number of neighbors using K = 5,15,25.
sizeSubset = 200
for (i in 1:5) {
  idxStart = (i - 1)*sizeSubset + 1 
  idxEnd = i*sizeSubset 
  evalSet = Xtrain[idxStart : idxEnd, ] 
  rest = Xtrain[-(idxStart : idxEnd), ] 
  evalLabel = Ytrain[idxStart : idxEnd, ] 
  k <- c(5,15,25) 
  for(i in k){
    pred2 <- knn(Xtrain, Xtest, Ytrain, k)
    mean(pred2 != Ytest)}}
pred2 <- knn(Xtrain, Xtest, Ytrain, 5)
mean(pred2 != Ytest)
pred3 <- knn(Xtrain, Xtest, Ytrain, 15)
mean(pred3 != Ytest)
pred4 <- knn(Xtrain, Xtest, Ytrain, 25)
mean(pred4 != Ytest)