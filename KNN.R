#loading libraries
library(ggplot2)
library(plyr)
library(lattice)
library(caret)
library(C50)
library(dplyr)
library(cluster)
library(modeldata)
library(randomForest)


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

completeSurvey$brand = as.character(completeSurvey$brand)

completeSurvey$brand = as.factor(completeSurvey$brand)

#select 75/100 for training the model
inTrain <- createDataPartition(
  y = completeSurvey$salary,
  p = .75,
  list = FALSE
)
#training and testing
training <- completeSurvey[inTrain, ]
testing <- completeSurvey[-inTrain, ] 

#model
my_knn_model <- train(brand ~ .,
                      method = "knn",
                      data = training,
                      tuneGrid = expand.grid(k = c(60, 70, 80)))

prediction <- predict(my_knn_model, testing)

confusionMatrix(data = prediction, reference = testing$brand)

training$brand = as.character(training$brand)

training$brand = as.factor(training$brand)

predictionIncomplete <- predict(my_knn_model, incompleteSurvey)

incompleteSurvey$brand <- predictionIncomplete

merge.data.frame(incompleteSurvey$brand, completeSurvey$brand, all.y = TRUE)

finalData <- merge.data.frame(incompleteSurvey$brand, 
                              completeSurvey$brand, 
                              all.y = TRUE,
                              sort = TRUE)

table <- count(finalData, vars = "Belkin", "Elago")

matrix <- as.data.frame(table(prediction, testing$brand))




