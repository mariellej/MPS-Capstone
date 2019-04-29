library(tseries)
library(MARSS)
library("urca")

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='unprocessed_data.csv',header=TRUE)

date = data$DATE

county_names=unique(as.character(data$COUNTY))

index = which(data$COUNTY==county_names[1])
years = date[index]
response = as.numeric((as.character(data$AREAPLANTED)[index]))
covariates = data[index,c(7:9)]

#fill in nas
covariates[93,1]=covariates[92,1]
covariates[93,2]=covariates[92,2]

for (i in 1:3){
  covariates[,i]=as.numeric(as.character(covariates[,i]))
}

#look at raw data
plot(ts(response))
plot(x=years,y=response,type="p")
#take lags for mean stationarity
plot(x=years[-1],y=diff(response),type="l")

adf.test(diff(response))
#ADF test for stationarity: p-value <0.05, reject null hypothesis of non-stationarity


for (i in 1:3){
  plot(covariates[,i],type="l")
}

response=t(response)
covariates=t(covariates)
#standardize our data
the.mean = apply(response,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(response,1,var,na.rm=TRUE))
response = (response-the.mean)*(1/the.sigma)

the.mean = apply(covariates,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(covariates,1,var,na.rm=TRUE))
covariates = (covariates-the.mean)*(1/the.sigma)

#can't plot time series object with more than 10 series ):
LWA <- ts(cbind(t(response), t(covariates)), start=1926, end=2018)
plot(LWA, main="", yax.flip=TRUE)

#model specificationw with observation errors only
model.list1 <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
                   D="unconstrained", d=covariates,
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0 )
fit.1 <- MARSS(c(response), model=model.list1, method = "kem")

#model specificationw with process errors only
model.list2 <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
                   C="unconstrained", c=covariates,
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0 )
fit.2 <- MARSS(c(response), model=model.list2, method = "BFGS")


##########assess goodness of fit for two models

#acf of resduals is white noise--> good fit
plot.ts(residuals(fit.1)$model.residuals[1,])
acf(residuals(fit.1)$model.residuals[1,])
#acf plot of residuals is white noise
plot.ts(residuals(fit.2)$model.residuals[1,])
acf(residuals(fit.2)$model.residuals[1,])

AIC(fit.1)
AIC(fit.2)
#fit 1 is better, lower AIC values



