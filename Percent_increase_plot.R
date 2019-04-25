
setwd("~/Documents/Git/MPS-Capstone/")

data=read.csv(file='finaldata_percentage_noNA.csv',header=TRUE)

date = data$DATE
percent_change = data$deltay

plot(x=date, y=percent_change,type="p")