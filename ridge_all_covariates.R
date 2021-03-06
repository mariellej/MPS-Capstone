#Lasso model

library("boot")
library("glmnet")
library("caret")

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage.csv',header=TRUE)

date = data$DATE

set.seed(1)

data[is.na(data[,19]),19]=0

X = data.matrix(data[,c(4:10,19)])
Y = as.vector(data[,18])
grid = 10^seq(10,-2,length = 100)
cv.error = matrix(nrow=9,ncol=11,data=NA)

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
    
    cv.out=cv.glmnet(Xtrain,Ytrain,alpha=a/10,lambda=grid, family = "gaussian")
    bestlam=cv.out$lambda.min
    
    ridge.mod=glmnet(Xtrain,Ytrain,alpha=a/10,lambda=bestlam, family = "gaussian")
    ridge.pred = predict.cv.glmnet(cv.out,s = bestlam, newx = Xtest, type = "link")
    
    cv.error[i,a+1] = mean((ridge.pred-Ytest)^2)
    
  }
}



#need to calculate training error
#caluclate bias and variance
#save coefficeints of each model

mean_error = colMeans(cv.error)