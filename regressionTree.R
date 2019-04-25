library(tree)
library(randomForest)

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage_noNA.csv',header=TRUE)

date = data$DATE

set.seed(1)

data[is.na(data[,19]),19]=0
data[is.na(data[,18]),18]=0

X = data[,c(4:17,19)]
Y = as.vector(data[,18])

cv.error = matrix(nrow=9,ncol=11,data=NA)

data = data.frame(Y,X)

for (a in 0:10){
  for (i in 1:9){
    traincount = 1
    if(i!= 9){
      while(date[traincount]<1926+i*10){
        traincount = traincount+1
      }
      testcount = traincount
      while(date[testcount]<=1926+i*10){
        testcount = testcount+1
      }
    }else{
      while(date[traincount]<2017){
        traincount = traincount+1
      }
      testcount = traincount
      while(date[testcount]<=2017){
        testcount = testcount+1
      }
    }
    
    trainIndex = seq(1,traincount-1)
    testIndex= seq(traincount,testcount-1)
    
    Xtrain = X[trainIndex,]
    Xtest = X[testIndex,]
    Ytrain = Y[trainIndex]
    Ytest = Y[testIndex]
    
    regTree = randomForest(y=Ytrain,x=Xtrain,importance=TRUE)
    yhat = predict(regTree,newdata=Xtest)
    
    cv.error[i,a+1] = mean((yhat-Ytest)^2)
    
  }
}


mean_error = colMeans(cv.error)
mean_error
varUsed(regTree,count=TRUE)
import = importance(regTree, type =1)