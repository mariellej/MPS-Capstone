library(e1071)

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage_noNA.csv',header=TRUE)

date = data$DATE

set.seed(1)

data[is.na(data[,19]),19]=0
data[is.na(data[,18]),18]=0

X = data[,c(4:17,19)]
Y = as.vector(data[,18])
cv.error = rep(0,9)

SVR = svm(Y~.,data=X)

# perform a grid search
tuneResult <- tune(svm, Y ~ .,  data = X,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tunedModel <- tuneResult$best.model