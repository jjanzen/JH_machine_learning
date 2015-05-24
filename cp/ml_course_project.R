getwd()
setwd("/Users/a149174/JHDataScience/JH_machine_learning")
dir.create("cp")
setwd("/Users/a149174/JHDataScience/JH_machine_learning/cp")
list.files()
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="~/JHDataScience/JH_machine_learning/cp/training.csv")
training <- read.csv("training.csv", header=T, na.strings=c("NA",""))
#training[is.na(training)] <- ""
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="~/JHDataScience/JH_machine_learning/cp/testing.csv")
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

# check for covairiates with minimul variability
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

set.seed(1000)
inTrain <- createDataPartition(y=training$classe, p=0.75, list=F)
my_training <- training[inTrain,]
my_testing <- training[-inTrain,]

# fit a model for RF
# use random forest with resampling with cross-validation 4-fold
set.seed(1000)
modelFit_rf <- train(classe~., data=my_training, method="rf", trControl=trainControl(method="cv", number = 4))
modelFit_rf
# final model
modelFit_rf$finalModel
# prediction
predictions_rf <- predict(modelFit_rf, newdata=my_testing)
# confusion matrix
confusionMatrix(predictions_rf, my_testing$classe)

# rf classify on original testing set
predictions_final_rf <- predict(modelFit_rf, newdata=testing)
predictions_final_rf

# fit a model for KNN
# use random forest with resampling with cross-validation 4-fold
set.seed(1000)
modelFit_knn <- train(classe~., data=my_training, method="knn", metric = "Accuracy", trControl=trainControl(method="cv", number = 4))
modelFit_knn
# final model
modelFit_knn$finalModel
# prediction
predictions_knn <- predict(modelFit_knn, newdata=my_testing)
# confusion matrix
confusionMatrix(predictions_knn, my_testing$classe)

# knn classify on original testing set
predictions_final_knn <- predict(modelFit_knn, newdata=testing)
predictions_final_knn

# compare results of rf and knn classification
qplot(predictions_final_rf, predictions_final_knn, data=testing)
?qplot
#B A B A A E D B A A B C B A E E A B B B

# final submission of each classifcation
setwd("/Users/a149174/")
dir.create("jh_ml")
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
answers <- predictions_final_rf
answers
pml_write_files(answers)
