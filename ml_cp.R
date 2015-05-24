Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large 
amount of data about personal activity relatively inexpensively. These type of devices are part of 
the quantified self movement – a group of enthusiasts who take measurements about themselves regularly
to improve their health, to find patterns in their behavior, or because they are tech geeks. 
One thing that people regularly do is quantify how much of a particular activity they do, but 
they rarely quantify how well they do it. In this project, your goal will be to use data from 
accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform 
barbell lifts correctly and incorrectly in 5 different ways. More information is available from 
the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

Data 

The training data for this project are available here: 
    
    https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
    
    https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you 
#create for this class for any purpose please cite them as they have been very generous in allowing their data to be 
#used for this kind of assignment. 

What you should submit

The goal of your project is to predict the manner in which they did the exercise. 
#This is the "classe" variable in the training set. You may use any of the other variables to predict with. 
#You should create a report describing how you built your model, how you used cross validation, what you think 
#the expected out of sample error is, and why you made the choices you did. You will also use your prediction model 
#to predict 20 different test cases. 

#1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. 
#Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it 
#easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
2. You should also apply your machine learning algorithm to the 20 test cases available in the test data 
#above. Please submit your predictions in appropriate format to the programming assignment for automated grading. 
#See the programming assignment for additional details. 

Reproducibility 

Due to security concerns with the exchange of R code, your code will not be run during the evaluation by your 
classmates. Please be sure that if they download the repo, they will be able to view the compiled HTML 
version of your analysis. 
library(caret);
getwd()
setwd("/Users/a149174/JHDataScience/machine_learning")
dir.create("cp")
setwd("/Users/a149174/JHDataScience/machine_learning/cp")
list.files()
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="~/JHDataScience/machine_learning/cp/training.csv")
training <- read.table("training.csv", header=T, sep=",", na.strings=c("NA",""))
#training[is.na(training)] <- ""
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="~/JHDataScience/machine_learning/cp/testing.csv")
testing <- read.csv("testing.csv", header=T, na.strings=c("NA",""))
# get dimesions before cleaning
dim(training)
dim(testing)

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

# fit a model
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

# fit a model
# use random forest with resampling with cross-validation 4-fold
modelFit_knn <- train(classe~., data=my_training, method="knn", metric = "Accuracy", trControl=trainControl(method="cv", number = 4))
modelFit_knn
# final model
modelFit_knn$finalModel
# prediction
predictions <- predict(modelFit_knn, newdata=my_testing)
predictions
# confusion matrix
confusionMatrix(predictions, my_testing$classe)
