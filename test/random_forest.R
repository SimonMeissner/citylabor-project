## Set Working Directory
setwd("C:/Users/User/Desktop/StadtLabor/")

##Load Data
plant_data <- read.csv("test_data.csv", header=TRUE)

##Visualize Data
View(plant_data)
str(plant_data)
table(plant_data$plant_name)

####IGNORE THIS SECTION (Factorize plant names) 
#plant_data$plant_name <- as.factor(plant_data$plant_name)
#levels(train$plant_name)
#train$plant_name<-factor(train$plant_name)
#levels(train$plant_name)
#####

## Create Train and Validation sets
set.seed(123)
independent_samples <- sample(nrow(plant_data), 0.7*nrow(plant_data), replace=FALSE)
TrainSet <-plant_data[independent_samples,]
TestSet <- plant_data[-independent_samples,]
library(randomForest)

## Create random Forest model
TrainSet$plant_name <- factor(TrainSet$plant_name)
rf <- randomForest(plant_name~ ., data=TrainSet, importance=TRUE)
rf
attributes(rf)

##Predictions
predTrain <- predict(rf, TrainSet, type="class")
table(predTrain, TrainSet$plant_name)

predValid<-predict(rf, TestSet, type="class")
table(predValid, TestSet, type="class")

mean(predValid ==TestSet$plant_name)

###IGNORE THIS
#library(caret)
#p1 <- predict(rf, train)
#confusionMatrix(p1, train$plant_name)
#p2 <- predict(rf, test)
#confusionMatrix(p2, test$plant_name)
