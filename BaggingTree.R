library(gbm)

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='refineddata.csv',header=TRUE)

date = data$DATE

set.seed(1995)

data[is.na(data[,19]),19]=0
data[is.na(data[,18]),18]=0

X = data[,c(4:17,19)]
Y = as.vector(data[,18])

cv.error = rep(0,81)

XY = data.frame(Y,X)

  for (i in 1:81){
    traincount = 1
    if(i!= 81){
      while(date[traincount]<1936+i){
        traincount = traincount+1
      }
      testcount = traincount
      while(date[testcount]<=1936+i){
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
    
    bagTree = gbm(Ytrain~.,data=Xtrain,distribution="gaussian",shrinkage=0.01)
    yhat = predict(bagTree,newdata=Xtest,n.trees=100)
    
    cv.error[i] = mean((yhat-Ytest)^2)
    
  }


#mean_error = colMeans(cv.error)
#mean_error
#varUsed(bagTree,count=TRUE)
#import = importance(bagTree, type =1)
