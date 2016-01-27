---
title: "Prediction Assignment Writeup"
author: "Hila Shamoon"
Date: "2016-01-26"
Output: html_document
---

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


### Project goal

The goal of the project is to predict the manner in which each participant exercised.

### Data

The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

```{r}
setInternet2(TRUE)
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
train <- read.csv('pml-training.csv')
test <- read.csv('pml-testing.csv')
```


### Class definition

* Exactly according to the specification (Class A)
* Throwing the elbows to the front (Class B)
* Lifting the dumbbell only halfway (Class C)
* Lowering the dumbbell only halfway (Class D)
* Throwing the hips to the front (Class E)

### Packages 
```{r}
library(caret)
library(randomForest)
library(dplyr)

```


### Clean data

Remove columns that do not contribute to the analysis:
* Columns that are >=95% empty
*Cleaning NearZeroVariance Variables Run
*Removing first colums

```{r}

train <- train[, 6:dim(train)[2]]

treshold <- dim(train)[1] * 0.95
#Remove columns with more than 95% of NA or "" values
xx <- !apply(train, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)

train <- train[, xx]

yy <- nearZeroVar(train, saveMetrics = TRUE)

train <- train[, yy$nzv==FALSE]

names(train)

#Class variable as a factor

train$classe <- as.factor(train$classe)
```

###Set up the overall training parameters
Repeated cross validation with 5 repeats, and the ‘oneSE’ selection function.  
The model gets tested on 10 different parts of the data, repeated 5 times over.
CreateDataPartition to create a a training sample of 75% of the data.

```{r}
ctrl <- trainControl(method="repeatedcv", number=5, repeats=3, selectionFunction = "oneSE")
inTrain <- createDataPartition(train$classe, p=.6, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
```

### Model selection
Selecting between random forest and a generalized boosted model

```{r}
rf_fm <- train(classe ~ . , data=training, method="rf", metric="Kappa",
            trControl=ctrl)

gbm_fm <- train(classe ~ ., data=training, method="gbm", metric="Kappa",
            trControl=ctrl, verbose=FALSE)
```

return models

####random forest
```{r}
rf_fm 
```

####generalized boosted model
```{r}
gbm_fm 
```

####Comparing between the two models

```{r}
resampls = resamples(list(RF = rf_fm,
                          GBM = gbm_fm))

difValues = diff(resampls)
summary(difValues)
```

Randomforest preformed better and was more accurate than the generalized boosted model. 

Test the model
```{r}
testing$pred <- predict(rf_fm,testing, "raw")
confusionMatrix(testing$pred, testing$classe)
```
I tested the model and sure enough the Kappa value was very high ( Kappa : 0.9953). Another way to test the model preformance is  to take a look at the no information rate which is he proportion of classes that you would guess right if you randomly allocated them. It ranked much lower that the Kappa (No Information Rate : 0.2845).


Calculate variable importance

```{r}
varImp <- varImp(rf_fm, scale=FALSE)
plot(varImp, main = "Importance of Top 30 Variables", top = 30)
```


Predict on sample 20 of the given test csv file

```{r}
test_pred <- predict(rf_fm,test)
test_pred
```


```{r}
# Write files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
```


pml_write_files(test_pred)
