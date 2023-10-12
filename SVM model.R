# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : Loan dataset  SVM
###########################################################################################################
##########################################################################################################

rm(list=ls())
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Project/Datasets/Loan")

db = read.csv('preprocessed_LoanDefault_2.csv',header=TRUE)
db = db[,-c(1:4)] #[, -c(5:7)]

db<-na.omit(db)
summary(db)

#Splitting into training and testing
train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

#Factorizing
train_data$diagnosis <- factor(train_data$Status)
test_data$diagnosis <- factor(test_data$Status)

#SVM model building
library(e1071)
svm.model <- svm( Status~ ., data = train_data  )
svm.pred <- predict(svm.model, test_data )

#Testing accuracy and printing model parameters
table(actual=test_data[,1],svm.pred )
SVM_wrong<- (test_data$Status!=svm.pred)
rate<-sum(SVM_wrong)/length(SVM_wrong)
accuracy = 1-rate
accuracy
print(svm.model)
