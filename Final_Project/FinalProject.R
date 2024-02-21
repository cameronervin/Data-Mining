install.packages("readr")
install.packages("dplyr")
install.packages("e1071")
install.packages("partykit")
install.packages("caret")
install.packages("DMwR")
install.packages("UBL")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("wesanderson")
install.packages("ggpubr")
install.packages("ggplot")
install.packages("ggarrange")
install.packages("ggalluvial")
install.packages("magrittr")
install.packages("backports")
install.packages("ggpubr")
install.packages('Boruta')
install.packages("tidyverse")  
install.packages("scales")
install.packages("stringr")
install.packages("ggthemes")
library(readr)
library(dplyr)
library(e1071)
library(partykit)
library(rpart)
library(rpart.plot)
library(wesanderson)
library(ggpubr)
library(ggarrange)
library(ggplot2)
library(ggalluvial)
library(magrittr)
library("tidyverse") 
library("scales") 
library("stringr") 
library("ggthemes") 
library(Boruta)


## Data Validation
# read adult train data csv
train<- read.csv("Downloads/adult.data")

# add column names
colnames(train) <- c("age", "workclass", "fnlwgt", "education", "educationNum",
                     "maritalStatus", "spouseAbsent", "occupation",
                     "race", "sex","capitalGain",
                     "capitalLoss", "hoursPerWeek", "nativeCountry", "income")
# remove question marks from data
idx1 <- train == " ?"
is.na(train) <- idx1
train <- na.omit(train)

# change chr columns to factor
train$income <- as.factor(train$income)
train$workclass <- as.factor(train$workclass)
train$education <- as.factor(train$education)
train$maritalStatus <- as.factor(train$maritalStatus)
train$spouseAbsent <- as.factor(train$spouseAbsent)
train$occupation <- as.factor(train$occupation)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$nativeCountry <- as.factor(train$nativeCountry)

# read adult test data csv
test <- read.csv("Downloads/adult.test", skip = 1)

# add column names
colnames(test) <- c("age", "workclass", "fnlwgt", "education", "educationNum",
                    "maritalStatus", "spouseAbsent", "occupation",
                    "race", "sex","capitalGain",
                    "capitalLoss", "hoursPerWeek", "nativeCountry", "income")
# remove question marks from data
idx2 <- test == " ?"
is.na(test) <- idx2
test <- na.omit(test)

# change chr columns to factor
test$income <- as.factor(test$income)
test$workclass <- as.factor(test$workclass)
test$education <- as.factor(test$education)
test$maritalStatus <- as.factor(test$maritalStatus)
test$spouseAbsent <- as.factor(test$spouseAbsent)
test$occupation <- as.factor(test$occupation)
test$race <- as.factor(test$race)
test$sex <- as.factor(test$sex)
test$nativeCountry <- as.factor(test$nativeCountry)
# remove periods from test factor levels
levels(test$income) <- c(" <=50K"," >50K")

# Summary data (min, med, mean, max, etc.)
summary(train)




# What are the most important features and what do they measure?


boruta_output <- Boruta(income ~ ., data=train, doTrace=0) 
boruta_output
summary(boruta_output)
plot(boruta_output)
?plot.Boruta

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# For each of the essential features
    # Range 
range(train$capitalGain)
range(train$educationNum)
range(train$capitalLoss)
range(train$age)
range(train$hoursPerWeek)
    # Mode
mode(train$capitalGain)
mode(train$educationNum)
mode(train$capitalLoss)
mode(train$age)
mode(train$hoursPerWeek)
?mode
    # Mean
mean(train$capitalGain)
mean(train$educationNum)
mean(train$capitalLoss)
mean(train$age)
mean(train$hoursPerWeek)
    # Median
median(train$capitalGain)
median(train$educationNum)
median(train$capitalLoss)
median(train$age)
median(train$hoursPerWeek)
    # Variance
var(train$capitalGain)
var(train$educationNum)
var(train$capitalLoss)
var(train$age)
var(train$hoursPerWeek)
    # Frequency counts
summary(train)

?boxplot
boxplot(train$capitalGain, train$educationNum,train$capitalLoss,train$age,train$hoursPerWeek)
boxplot(train$capitalGain)
boxplot(train$educationNum)
boxplot(train$capitalLoss)
boxplot(train$age)
boxplot(train$hoursPerWeek)

table(train$income)

# Provide 3 visualizations and interpret them
temp <- table(train$income)
temp
barplot(temp,
        main = "Frequency of <=50K vs >50K",
        xlab = "<=50K group (0), >50K(1)",
        ylim = c(0,25000))

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

#INCOME SEX
train %>% group_by(sex,income) %>% count() %>% ggplot(aes(x=reorder(sex,n),y=n))+
  geom_col(aes(fill=income),position = "dodge")+coord_flip()+
  theme_light()+
  scale_fill_manual(values=wes_palette(n=2, name=("BottleRocket1")))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank(),plot.title = element_text(size=12,face="bold"))+
  ggtitle("Income Class Based on Sex")

# EDUCATION INCOME
train %>%group_by(education,income) %>% count() %>% 
  ggplot(aes(education,n))+geom_col(aes(fill=income))+
  theme_light()+
  coord_flip()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank(),plot.title = element_text(size=12,face="bold"))+
  scale_fill_manual(values=wes_palette(n=2, name=("BottleRocket1")))+
  ggtitle("Income by Education Level")

# Use scatter plots, correlation, box plots, cross-tabulation, or group-wise averages to show relationships between features
plot(train$income, train$age, main="Income by Age Boxplot",
     xlab="Income ", ylab="Age", pch=19)


tab2<- table(train$education)
table2<- prop.table(tab2)
table2
df2<-as.data.frame(table2)
names(df2) <- c("education", "Frequency")
ggplot(data=df2)

# Use the proportion table as the data frame in a call to ggplot()
ggplot(data=df2, mapping=aes(x=education, y=Frequency)) + 
  geom_col() 


cor(train$educationNum, train$capitalGain, method = c("pearson", "kendall", "spearman"))
cor.test(train$educationNum, train$capitalGain, method=c("pearson", "kendall", "spearman"))

cor(train$capitalGain, train$hoursPerWeek,  method = "pearson", use = "complete.obs")

## Scatter plot for education number by capital gain correlation
ggscatter(train, x = "educationNum", y = "capitalGain", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Education Number", ylab = "Capital Gains", main = "Correlation between Education Number and Capital Gains")



res <- cor(train[,c(1,3,5,11,12,13)])
round(res, 2)


################################
# KNN
install.packages("FNN")
library(FNN)

# data can only be numeric, therefore, only use numeric (non-factor) columns
pred <- knn(train[,c(1,3,5,11,12,13)], test[,c(1,3,5,11,12,13)], train[,15], k=5)
# misclassification rate
knn_missclass <- mean(pred != test$income) # missclassification rate = 0.2312903
# confusion matrix
table(pred, test[,15])

# Random Forrest
install.packages("tree")
install.packages("randomForest")
library(tree)

# omit native country because tree can only take factors with max 32 levels
treeModel <- tree(income ~ ., train[,-14])
plot(treeModel)
text(treeModel)
# prune tree
library(randomForest)
cv.tree(treeModel,FUN = prune.tree, K=5)
# run best sized tree
# choose size 8 because least deviance according to cross validation
# deviance = 21478.51
rfmodel <- randomForest(income ~.,
                        train,
                        mtry = 8)

# combine train and test data to make sure factor levels are the same across dataset
comb_data <- rbind(train,test)
# decision tree pred
rfpred <- predict(rfmodel, comb_data)
# split pred for testing
rfpred2 <- rfpred[30162:45220]
# missclassification rate
# calculate missclassification rate based off split pred data
rf_missclass <- mean(rfpred2 != test$income) # missclassification rate = 0.1527326
# confusion matrix
table(rfpred2, test$income)

# SVM
install.packages("e1071")
library(e1071)
svm1 <- svm(income ~ ., train)
# SVM pred using combined data
svm1_pred <- predict(svm1, comb_data)
# split pred for testing
svm1_pred2 <- svm1_pred[30162:45220]
# missclassification
# calculate missclassification rate based off split pred data
svm_missclass <- mean(svm1_pred2 != test$income)  # missclassification rate = 0.1519357
# confusion matrix
table(svm1_pred2, test$income)

# prediction accuracy comparison
missclassification <- c(knn_missclass, rf_missclass, svm_missclass)
missclassification <- sort(missclassification, decreasing = FALSE)
names(missclassification) <- c("svm_missclass", "rf_missclass", "knn_missclass")
print(missclassification)
# svm lowest missclassification (0.1519357), rf second (0.1527326), knn last (0.2312903)
