###data preparation
data(Glass, package="mlbench")
##Observe the proportion of different type
round(prop.table(table(Glass$Type))*100,digits = 1)
source("normalize.R")
##normalize the data
A<-as.data.frame(lapply(Glass[,1:9], normalize))
##get the trainset and the testset
A_train<-A[1:154,]
A_test<-A[155:214,]
##keep the labels of the type of trainset and the testset
A_train_la<-Glass[1:154,10]
A_test_la<-Glass[155:214,10]
###train the KNN model
library(class)
A_test_pred<-knn(train = A_train,test = A_test,
                 cl = A_train_la, k = 5)
###test the model
library(gmodels)
CrossTable(x = A_test_pred, y = A_test_la)
table((A_test_pred)==(A_test_la))

#In this case, the effection of the KNNmodel is bad.
###change the way of normalization
A<-as.data.frame(scale(Glass[,-10]))
##repeat the former step

#In this case, the effection of the KNNmodel is also bad.