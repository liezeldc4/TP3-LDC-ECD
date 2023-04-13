# Multiple Linear Regression on Car Price Prediction data

library(tidyverse)
dataset=read.csv("car_price_prediction_for_R.csv")

#First we will explore the dataset to have a better understanding of what's in it
view(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Here are a couple of plots that help illustrate a couple of the variables in this dataset
ggplot(data=dataset,
       aes(Manufacturer))+
  geom_bar()

ggplot(data=dataset,
       aes(Color))+
  geom_bar()


#Missing value
colSums(is.na(dataset))
#From this function we see that there are no missing values

#Splitting the data into 2 sets
library(caTools)
set.seed(100)
split=sample.split(dataset$Price, SplitRatio = 0.8) #80% training 20% testing
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split=FALSE)

#Multiple linear regression training

names(dataset)
MLR=lm(formula=Price~.,
       data=training_set)
summary(MLR)

# Due to how many variables there are it is not practical or useful to write out the formula
#           for the regression. 
#Note that when looking at a specific vaiable,
#            if the p value of the variable is >.05, the variable claimed as not statistically 
#           significant

#Mean square error
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste("Mean squared error", MSE)
# Our mean squared error is approximately: 131,416,298.81


#R-square = 69.69% from summary
summary(MLR)
#How much a variation of a dependent variable is explained by independent variables
#Higher value means good model
#This model R square value is 69.69% and that means its a good model
# A value less than 50% it is not a good model
# A value less than 30% is is a poor model
