
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
  y = completeSurvey$salary,
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



training$brand = as.character(training$brand)

training$brand = as.factor(training$brand)


modelTuning <- expand.grid(.mtry = seq(4,16,4), ntree = c(700, 1000,2000))

#training
rforest <- train(brand ~ .,
                      data = training,
                      method = "rf",
                      tuneLenght=,
                      train = trainControl)
                    

summary(rforest)                 


