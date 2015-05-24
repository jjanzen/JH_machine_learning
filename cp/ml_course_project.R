getwd()
setwd("/Users/a149174/JHDataScience/machine_learning")
dir.create("cp")
setwd("/Users/a149174/JHDataScience/machine_learning/cp")
list.files()
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="~/JHDataScience/machine_learning/cp/training.csv")
training <- read.csv("training.csv", header=T, na.strings=c("NA",""))
#training[is.na(training)] <- ""
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="~/JHDataScience/machine_learning/cp/testing.csv")
testing <- read.csv("testing.csv", header=T, na.strings=c("NA",""))
# get dimesions before cleaning
dim(training)
dim(testing)
library(caret);
plot(training$classe, col="green", main="Frequency of Classe Levels", xlab="classe levels", ylab="Frequency")

# delete columns with all missing values
training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

# remove first seven columns not valid for machine learning (x, username, timestamps, new_window, and num_window )
training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

set.seed(1000)
inTrain <- createDataPartition(y=training$classe, p=0.75, list=F)
my_training <- training[inTrain,]
my_testing <- training[-inTrain,]

# fit a model for RF
# use random forest with resampling with cross-validation 4-fold
modelFit <- train(classe~., data=my_training, method="rf", trControl=trainControl(method="cv", number = 4))
modelFit
# final model
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=my_testing)
predictions
# confusion matrix
confusionMatrix(predictions, my_testing$classe)

# out of sample error 
insamplepredict=predict(modelFit,my_testing)
confusionMatrix(my_testing$classe, insamplepredict)

# test on original testing set
predictions_test <- predict(modelFit, newdata=testing)
predictions_test
confusionMatrix(predictions_test, testing$classe)

# fit a model for KNN
# use random forest with resampling with cross-validation 4-fold
modelFit_knn <- train(classe~., data=my_training, method="knn", metric = "Accuracy", trControl=trainControl(method="cv", number = 4))
modelFit_knn
# final model
modelFit_knn$finalModel

# prediction of 20 test samples 
predictions <- predict(modelFit_knn, newdata=my_testing)
predictions
# confusion matrix
confusionMatrix(predictions, my_testing$classe)
#B A B A A E D B A A B C B A E E A B B B
