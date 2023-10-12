# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : Loan dataset  ANN
###########################################################################################################
##########################################################################################################

rm(list=ls())
install.packages("neuralnet tools")
library(neuralnet)
library(NeuralNetTools)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Project/Datasets/Loan")

db = read.csv('preprocessed_LoanDefault_2.csv',header=TRUE)
db = db[,-c(1:4)] #[, -c(5:7)]
#db<-na.omit(db)
summary(db)
View(db)
db$age <- factor(db$age,
                    levels = c("<25","25-34","35-44","45-54","55-64","65-74",">74"),
                    labels = c(1, 2,3,4,5,6,7))
?as.numeric
db$age <- as.integer(db$age)
train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]
?neuralnet
model <- neuralnet(Status~.,data=train_data,hidden=5, threshold=0.01) # WITH hIDDEN LAYER= 10 accuracy increases to 0.9707 from 0.6374 at hidden layer=5
plotnet(model)
?compute
pred<-compute(model ,test_data[,-1])
ann<-c('0','1')[apply(pred$net.result,1,which.max)]


table(Actual=test_data$Status,predition=ann)

inc = (test_data$diagnosis != ann)
accuracy <-1 - sum(inc)/length(inc)
accuracy
