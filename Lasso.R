#Lasso model

library("boot")
library("glmnet")
library("caret")

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata.csv',header=TRUE)

date = data$DATE

set.seed(1)

X = data.matrix(data[,4:17])
Y = as.vector(data[,18])
grid = 10^seq(10,-2,length = 100)
cv.error = rep(0,9)

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
  
  cv.out=cv.glmnet(Xtrain,Ytrain,alpha=0,lambda=grid, family = "gaussian")
  bestlam=cv.out$lambda.min
  
  ridge.mod=glmnet(Xtrain,Ytrain,alpha=0,lambda=bestlam, family = "gaussian")
  ridge.pred = predict.cv.glmnet(cv.out,s = bestlam, newx = Xtest, type = "link")
  
  cv.error[i] = mean((ridge.pred-Ytest)^2)
  
}

#need to calculate training error
#caluclate bias and variance
#save coefficeints of each model

mean_error = mean(cv.error)