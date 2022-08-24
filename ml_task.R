####
title: 'Prediction Assignment Writeup, Course: Practical Machine Learning by John
Hopkins university'
author: "Sören Nonnengart"
date: '2022-08-24'
output:
  html_document: default
word_document: default
pdf_document: default


### Introduction

## Here you see the results of my final Coursera’s Practical Machine Learning course, as part of the Data Science 
## Specialization track offered by John Hopkins with coursera.
## In this project, I will use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants 
## to predict the manner in which they did the exercise. This is the “classe” variable in the training set. 
## I will train 4 models overall: Decision Trees, Random Forest, Gradient Boosted Trees, Support Vector Machine 
## I then predict using a validation set randomly selected from the training csv data to obtain the accuracy and 
## out of sample error rate. Based on those numbers, I decide on the best model, and use it to predict 20 cases 
## using the test csv set. 


## Loading Data and Libraries
#### Libraries that will to be used for the analysis
library(caret)
library(kernlab)
library(rattle)
library(lattice)
library(ggplot2)
library(corrplot)
set.seed(318925)

## create a path to my folder
path <- ".../course_8_Machine learning"
setwd(path)
getwd()

## read the trainng and testdatasets
traincsv <- read.csv("pml-training.csv")
testcsv <- read.csv("pml-testing.csv")
dim(traincsv)
dim(testcsv)
### You can see that there are 160 variables and 19622 observations in the training set and 20 for the test dataset.


## Cleaning the Data now

#### Removing NA's in the variables
traincsv <- traincsv[,colMeans(is.na(traincsv)) < .9] 
traincsv <- traincsv[,-c(1:7)] 

#### Removing near zero variance variables.
nvz <- nearZeroVar(traincsv)
traincsv <- traincsv[,-nvz]
dim(traincsv)

#### The removing of the unnecessary variables are finished, so now the training dataset can be split 
#### into a validation and sub training set. The testing set “testcsv” will be left alone, and used for the final 
#### quiz test cases.
inTrain <- createDataPartition(y=traincsv$classe, p=0.7, list=F)
train <- traincsv[inTrain,]
valid <- traincsv[-inTrain,]

## Now create and test the models
control <- trainControl(method="cv", number=3, verboseIter=F)

## Decision Tree
mod_trees <- train(classe~., data=train, method="rpart", trControl = control, tuneLength = 5)
fancyRpartPlot(mod_trees$finalModel)

### prediction  Confusion Matrix and Statistics
pred_trees <- predict(mod_trees, valid)
cmtrees <- confusionMatrix(pred_trees, factor(valid$classe))
cmtrees

### Random forest
mod_rf <- train(classe~., data=train, method="rf", trControl = control, tuneLength = 5)
pred_rf <- predict(mod_rf, valid)
cmrf <- confusionMatrix(pred_rf, factor(valid$classe))
cmrf

### Gradient Boosted Trees
mod_gbm <- train(classe~., data=train, method="gbm", trControl = control, tuneLength = 5, verbose = F)
pred_gbm <- pr  edict(mod_gbm, valid)
cmgbm <- confusionMatrix(pred_gbm, factor(valid$classe))
cmgbm

### Support vector machines
mod_svm <- train(classe~., data=train, method="svmLinear", trControl = control, tuneLength = 5, verbose = F)
pred_svm <- predict(mod_svm, valid)
cmsvm <- confusionMatrix(pred_svm, factor(valid$classe))
cmsvm

## Prediction on test dataset
pred <- predict(mod_rf, testcsv)
print(pred)

### Different plots

corrPlot <- cor(train[, -length(names(train))])
corrplot(corrPlot, method="color")
plot(mod_trees)
plot(mod_rf)
plot(mod_gbm)
