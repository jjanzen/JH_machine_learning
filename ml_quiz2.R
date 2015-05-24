Question 1
#Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
# Which of the following commands will create training and test sets with about 50% of the observations 
# assigned to each?

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

adData = data.frame(diagnosis,predictors)  - answer
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

#Question 2 - removed from quiz

# Question 3
#Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Find all the predictor variables in the training set that begin with IL. Perform principal components 
#on these variables with the preProcess() function from the caret package. Calculate the number of principal
#components needed to capture 90% of the variance. How many are there?
set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
preObj <- preProcess(training[,IL], method="pca", thresh = 0.9)
?preProcess
preObj
trainIL <- predict(preObj,training[,1])
modelFit <0 
9 #
11
5
8

Question 4
#Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Create a training data set consisting of only the predictors with variable names beginning with 
#IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using 
#PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" 
#in the train function. What is the accuracy of each method in the test set? Which is more accurate?
set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- (predictors[,IL])
df <- data.frame(diagnosis, ILpredictors)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[ inTrain,]
testing = df[-inTrain,]

modelFit <- train(diagnosis~., data=training, method="glm")
# final model
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=testing)
# confusion matrix
confusionMatrix(predictions, testing$diagnosis)
# non-PCA Accuracy : 0.6463 

modelFit <- train(training$diagnosis ~ ., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
# final model
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=testing)
# confusion matrix
confusionMatrix(predictions, testing$diagnosis)
# Accuracy : 0.7195 

Non-PCA Accuracy: 0.65 x
PCA Accuracy: 0.72 x
Non-PCA Accuracy: 0.72 
PCA Accuracy: 0.71
Non-PCA Accuracy: 0.75 
PCA Accuracy: 0.71
Non-PCA Accuracy: 0.74 
PCA Accuracy: 0.74
