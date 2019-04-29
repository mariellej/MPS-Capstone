library(tseries)
library(MARSS)
library("urca")

setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='unprocessed_data.csv',header=TRUE)

date = data$DATE

county_names=unique(as.character(data$COUNTY))


index = which(data$COUNTY==county_names[1])
n=length(index)
years = date[index]
response = as.numeric((as.character(data$AREAPLANTED)[index]))
test = response[n]
response[n]<-NA
covariates = data[index,c(7:9)]

#fill in nas
covariates[93,1]=covariates[92,1]
covariates[93,2]=covariates[92,2]

for (i in 1:3){
  covariates[,i]=as.numeric(as.character(covariates[,i]))
}

response=t(response)
covariates=t(covariates)
#standardize our data
the.mean = apply(response,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(response,1,var,na.rm=TRUE))
response = (response-the.mean)*(1/the.sigma)
scaled_true_value = (test-the.mean)*(1/the.sigma)

the.mean = apply(covariates,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(covariates,1,var,na.rm=TRUE))
covariates = (covariates-the.mean)*(1/the.sigma)

#can't plot time series object with more than 10 series ):
LWA <- ts(cbind(t(response), t(covariates)), start=1926, end=2018)
plot(LWA, main="", yax.flip=TRUE)

#model specification with observation errors only
model.list1 <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
                    D="unconstrained", d=covariates,
                    Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                    x0=matrix("mu"), tinitx=0 )
fit.1 <- MARSS(c(response), model=model.list1, method = "kem")
#fit.1 <- MARSS(c(response), model=model.list1, method = "BFGS",inits = fit.1kem)

#model specificationw with process errors only
model.list2 <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
                    C="unconstrained", c=covariates,
                    Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                    x0=matrix("mu"), tinitx=0 )
fit.2 <- MARSS(c(response), model=model.list2, method = "BFGS")
#fit.2 <- MARSS(c(response), model=model.list1, method = "BFGS",inits = fit.2kem)

#forecasts
forecast1 = c(fit.1$ytT)[is.na(response)]
forecast2 = c(fit.2$ytT)[is.na(response)]


mse1 = (forecast1 - scaled_true_value)^2
mse2 = (forecast2 - scaled_true_value)^2

#plot raw data
plot(x=years, y=response,type="l")
points(x=years[is.na(response)],y=scaled_true_value)
#model 1
lines(x=years[is.na(response)], y=forecast1, col = "red",type="p")
lines(years[is.na(response)], c(fit.1$ytT + qnorm(0.975)*fit.1$ytT.se)[is.na(response)], col = "red",
      type="p",pch=0)
lines(years[is.na(response)], c(fit.1$ytT - qnorm(0.975)*fit.1$ytT.se)[is.na(response)], col = "red",
      type="p",pch=0)
#model2
lines(years[is.na(response)], forecast2, col = "green",type="p")
lines(years[is.na(response)], c(fit.2$ytT + qnorm(0.975)*fit.2$ytT.se)[is.na(response)], col = "green",
      type="p",pch=0)
lines(years[is.na(response)], c(fit.2$ytT - qnorm(0.975)*fit.2$ytT.se)[is.na(response)], col = "green",
      type="p",pch=0)

legend("bottomleft",bty="n",cex = 0.7, legend = c("True Value","Observation Errors only","Process Errors only"),fill=c("black","red","green"))
legend("bottomright",bty = "n",cex=0.7,legend=c("forecast","Confidence Interval"),col=c("red","red"),pch=c(1,0))
