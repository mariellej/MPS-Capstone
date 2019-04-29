library(e1071)

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage_noNA.csv',header=TRUE)

date = data$DATE

set.seed(1)

data[is.na(data[,19]),19]=0
data[is.na(data[,18]),18]=0

X = data[,c(4:17,19)]
Y = as.vector(data[,18])

YX = data.frame(Y,X)

traincount=1
while(date[traincount]<2017){
  traincount = traincount+1
}
testcount = traincount
while(date[testcount]<=2017){
  testcount = testcount+1
}

trainIndex = seq(1,traincount-1)
testIndex= seq(traincount,testcount-1)

my_trainset = YX[trainIndex,]
Xtest = X[testIndex,]
Ytest = Y[testIndex]


tuned_parameters <- tune.svm(Y~., data = my_trainset, gamma = 10^(-5:-1), cost = 10^(-3:1))


