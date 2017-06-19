library("rpart")
library("rpart.plot")
library("caret")
library("ROCR")
library("ggplot2")

#read files
data <- read.csv("C:\\Program Files\\R\\R-3.3.1\\bin\\Titanic.csv")

#read field name
data1 <- subset(data, select = c(PClass, Age, Sex, Survived))

#change value type
data1$Survived <- ifelse(data1$Survived == 1, "Yes", "No");
data1$Age <- ifelse(data1$Age >= 18, "Adult", "Child");

#delete NA value
finaldata <- data1[complete.cases(data1), ]

#test/train data (8:2)
set.seed(22)
train.index <- sample(x=1:nrow(finaldata), size=ceiling(0.8*nrow(finaldata) ))
train <- finaldata[train.index, ]
test <- finaldata[-train.index, ]

# CART model-Y for survived
cart.model<- rpart(Survived ~. ,  data=train)

#decision tree
prp(cart.model,faclen=0,fallen.leaves=TRUE,extra=2)  

pred <- predict(cart.model, newdata=test, type="class")

# ��table�ݹw�������p
table(real=test$Survived, predict=pred)
ConfusionMatrix <- table(real=test$Survived, predict=pred)

TP <- ConfusionMatrix[1]
FP <- ConfusionMatrix[2]
FN <- ConfusionMatrix[3]
TN <- ConfusionMatrix[4]

Sensitivity <- TP/(TP+FN)
Sensitivity
Specificity <- TN/(TN+FP)
Specificity
Precision <- TP/(TP+FP) 
Precision
Recall <- TP/(TP+FN)
Recall

