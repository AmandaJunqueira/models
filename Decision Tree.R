#loading libraries
library(ggplot2)
library(plyr)
library(lattice)
library(caret)
library(C50)
library(dplyr)
library(cluster)
library(modeldata)

#read data
surveyData <- read.csv("data/surveyBelkinElago.csv", sep = ";", dec = ",")
summary(surveyData)

#select only age and salary
dataData <- surveyData %>% select(age, salary, brand)

#normalization
normalData <- preProcess(dataData, method = c("range"))
print(normalData)
transformed <- predict(normalData, dataData)
summary(transformed)


incompleteSurvey <- transformed %>% filter(brand == " ")

completeSurvey <- transformed %>% filter(brand != " ")

#select 75/100 for training the model
inTrain <- createDataPartition(
  y = completeSurvey$age,
  p = .75,
  list = FALSE
)
 #training and testing
training <- completeSurvey[inTrain, ]
testing <- completeSurvey[-inTrain, ] 

#cross-vaalidation
trainControl <- trainControl(method = "cv",
                             number = 3,
                             search = "grid")

modelTuning <- expand.grid(trials = c(5, 7, 9), model = c("rules", "tree"), winnow = c(TRUE, FALSE))

#training model
modelC <- caret::train(brand ~ .,
               #trials = 3,
               data = training,
               method = "C5.0",
               tuneGrid = modelTuning,
               #tuneLength = 2,
               train = trainControl,
               preProcess = c("range"))

plot(modelC)


summary(modelC)


training$brand = as.character(training$brand)

training$brand = as.factor(training$brand)




