library(splines)
library(gam)
library(car)
library(mgcv)

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage_noNA.csv',header=TRUE)

date = data$DATE

set.seed(1)

data[is.na(data[,19]),19]=0
data[is.na(data[,18]),18]=0

X = data[,c(4:17,19)]
Y = as.vector(data[,18])

#cv.error = matrix(nrow=9,ncol=11,data=NA)
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

linear_fit = lm(Y[trainIndex]~.,data=X[trainIndex,])
Anova(linear_fit)

#ANOVA indicates that Production_Q Labor_EXP, and LOC are not important

X = data[,c(4:7,9:14,16,19)]
Y = as.vector(data[,18])

gam1 = gam(Y[trainIndex]~COUNTY+STATE+s(DATE)+s(AREAHARVESTED)+s(YIELD)+s(P_PRICE)+s(CROP_DEN)+s(P_VALUE)+s(EXP_VAL)+s(IMP_VAL)+s(DOM_CONSUMP)+s(deltayhat),data=X[trainIndex,])


b1 <- mgcv::gam(y ~ X[,1]+X[,2],s(X[,3], bs='ps', sp=0.6) + s(X[2], bs='ps', sp=0.6) + s(X[,4], bs='ps', sp=0.6)+ s(X[,5], bs='ps', sp=0.6)+s(X[,6], bs='ps', sp=0.6)+ s(X[,7], bs='ps', sp=0.6),s(X[,8], bs='ps', sp=0.6)+s(X[,9], bs='ps', sp=0.6)+s(X[,10], bs='ps', sp=0.6)+s(X[,11], bs='ps', sp=0.6)+s(X[,12], bs='ps', sp=0.6))
summary(b1)
plot(b1)
