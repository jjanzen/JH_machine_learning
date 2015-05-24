install.packages("kernlab")
library(caret); 
library(kernlab); 
data(spam)
summary(spam)
head(spam)

plot(density(spam$your[spam$type=="nonspam"]), col="blue", main="", xlab="Freq of 'your'")
lines(density(spam$your[spam$type=="spam"]), col="red")
abline(v=0.5, col="black")

prediction <- ifelse(spam$your >0.5, "spam", "nonspam")
table(prediction, spam$type)/length(spam$type)
#prediction   nonspam      spam
#nonspam 0.4590306 0.1017170
#spam    0.1469246 0.2923278

# accuracy = 45.9 + 29.2 = 75%

# 5 Suppose that we have created a machine learning algorithm that predicts whether a link will be clicked 
with 99% sensitivity and 99% specificity. The rate the link is clicked is 1/1000 of visits to a website. 
If we predict the link will be clicked on a specific visit, what is the probability it will actually be clicked?
# http://datasciencespecialization.github.io/courses/08_PracticalMachineLearning/006typesOfErrors/#7
1/1000 = 0.1%

  +     -  
+ 99   999
-  1  98901

99/(99+999)
# 0.09

90%
99%
89.9%
9%