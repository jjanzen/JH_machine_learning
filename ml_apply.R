install.packages("caret")
install.packages("kernlab")
library(caret); library(kernlab);
data(spam)
summary(spam)
head(spam)
# data splitting
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
# fit a model
set.seed(32343)
modelFit <- train(type~., data=training, method="rf")
?train
modelFit
# final model
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=testing)
predictions
# confusion matrix
confusionMatrix(predictions, testing$thx)

# k-fold
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=T,returnTrain=T)
sapply(folds,length)

# web clicks practical example
library(caret);
setwd("/Users/a149174/JHDataScience/machine_learning")
list.files()
clicks_data <- read.csv("combined.csv", head=F)
dim(clicks_data)
col_headings <- c('context_id', 'session', 'event', 'homepage', 'product', 'review', 'search', 'deal', 'thx')
names(clicks_data) <- col_headings
# remove context_id column
data <- clicks_data[,2:9]
# convert thx page to "yes" or "no"
data$thx[data$thx > 0] <- "yes"
data$thx[data$thx == 0] <- "no"

#data[,1:7] <- sapply(data[,1:7], as.numeric)
#head(data,100)
dim(data)
# limit data to at least 4 events per user
data <- subset(data,data[,2] >3)
# convert thx to facter
data[,8] <- sapply(data[,8], as.factor)
class(data[,8])
# NEW data exploration not working
library(ggplot2)
pairs.panels(data[, 1:8],pch=21,bg=c('blue','yellow')[data$thx],scale=T,ellipses=FALSE,
             main = "Breast tumor classification and first 5 explanatory variables", jiggle = T)
# data splitting
inTrain <- createDataPartition(y=data$thx, p=0.75, list=F)
training <- data[inTrain,]
testing <- data[-inTrain,]
dim(training)

# fit a model
set.seed(2000)
# 10-fold cross validation

ctrl <- trainControl(method="cv", number = 10, classProbs=T, summaryFunction=defaultSummary)


modelLookup("glm")
modelLookup("pls")
modelFit <- train(training$thx~., data=training, method="glm",
                  trControl = ctrl)
head(training)                  
?train

modelFit
# final model
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=testing)
predictions
# confusion matrix
confusionMatrix(predictions, testing$thx)
levels(predictions)

# find correlation and remove non-correlation for new data output
corrdata <- data[, -8] # remove classifier
data[,1:7] <- sapply(data[,1:7], as.numeric)
dataCorr <- cor(corrdata)
?cor
install.packages("corrplot")
library(corrplot)
corrplot(dataCorr, order = "hclust", tl.cex = .50)
highCorr <- findCorrelation(dataCorr, -.95) #which columns to remove due to high collinearity
names(corrdata[,highCorr])

head(corrdata)
data <- (data[,-1]) # revise data to exclude variable(s) based on correlation
head(data)

# altername method
modelFit <- train(training$thx~., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
?train
