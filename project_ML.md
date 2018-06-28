---
title: "Coursera Machine Learning Project"
author: "Henk van Essen"
date: "26 juni 2018"
output: 
  html_document:
    keep_md: true
---



## Executive summary
In this study we try to detect the quality of the excercise of weight lifting. The quality of the excersize is defined by classes A through E. The measurements come from accelarometers at the waist, upper arm, fore arm and dumbbell. Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl.

We concluded that we can detect the correct quality class with 99.4% accuracy using a random forest model.

## Loading and processing data

```r
library(rpart)
library(randomForest)
library(caret)
```

```r
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainingdata <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
dim(trainingdata)
```

```
## [1] 19622   160
```

```r
# Exclude variables that can't be used for prediction
cols <- grep("name|timestamp|window|X", colnames(trainingdata), value=F) 
train <- trainingdata[,-cols]
# Exclude variables with more than 20% missing data 
NArate <- apply(train, 2, function(x) sum(is.na(x)))/nrow(train)
train <- train[!(NArate>0.2)]
dim(train)
```

```
## [1] 19622    53
```

```r
# Split in training and test subsets
set.seed(120765)
inTrain <- createDataPartition(train$classe, p=0.7, list=FALSE)
subTrain <- train[inTrain, ]; subTest <- train[-inTrain, ]
subTrain$classe <- factor(subTrain$classe)
```

## Creating and testing models


```r
# Build classification trees using the k-means cluster
modKM <- rpart(classe ~ ., data=subTrain, method="class")
predKMtrain <- predict(modKM, subTrain, type = "class")
# In sample accuracy
confusionMatrix(predKMtrain, subTrain$classe)$overall[1]
```

```
##  Accuracy 
## 0.7401907
```

```r
# Cross validation
predKM <- predict(modKM, subTest, type = "class")
# Out of sample accuracy 
confusionMatrix(predKM, subTest$classe)$overall[1]
```

```
##  Accuracy 
## 0.7284622
```

```r
# Out of sample error of 27% is quite bad

# Build Random Forrest model
modRF <- randomForest(classe ~. , data=subTrain)
predRFtrain <- predict(modRF, subTrain, type = "class")
# In sample Accuracy
confusionMatrix(predRFtrain, subTrain$classe)$overall[1]
```

```
## Accuracy 
##        1
```

```r
# Cross validation
predRF <- predict(modRF, subTest, type = "class")
# Out of sample Accuracy 
confusionMatrix(predRF, subTest$classe)$overall[1]
```

```
##  Accuracy 
## 0.9943925
```

```r
# Out of sample error for this model is 0.6%
confusionMatrix(predRF, subTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1672    3    0    0    0
##          B    1 1131    5    0    0
##          C    1    5 1020   12    0
##          D    0    0    1  949    2
##          E    0    0    0    3 1080
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9944          
##                  95% CI : (0.9921, 0.9961)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9929          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9988   0.9930   0.9942   0.9844   0.9982
## Specificity            0.9993   0.9987   0.9963   0.9994   0.9994
## Pos Pred Value         0.9982   0.9947   0.9827   0.9968   0.9972
## Neg Pred Value         0.9995   0.9983   0.9988   0.9970   0.9996
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2841   0.1922   0.1733   0.1613   0.1835
## Detection Prevalence   0.2846   0.1932   0.1764   0.1618   0.1840
## Balanced Accuracy      0.9990   0.9959   0.9952   0.9919   0.9988
```

## Selection best model
The Random Forest model performs best with an out-of-sample error of 0.4%. The K-means has an accuracy of only 73% and performs poorly for this data set.

## Prediction of Testing data 

```r
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testingdata <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
predict(modRF, testingdata, type="class")
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

## Reference
Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers??? Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6. Cited by 2 (Google Scholar)

Read more: http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz3jOpnStGb
