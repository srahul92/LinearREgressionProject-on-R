setwd("/Users/rahulsharma/Documents/R+class/dataset")
library(caret)
library(boom)
library(dpylr)
library(dummy)
library(ggplot2)
library(Metrics)

raw_data = data.frame(read.csv("insurance.csv"))

sum(is.na(raw_data))
summary(raw_data)

# outliers 
boxplot(raw_data$age,xlab = 'age', ylab = 'count', main = 'Boxplot of age')
boxplot(raw_data$bmi,xlab = 'bmi', ylab = 'count', main = 'Boxplot of BMI')
boxplot(raw_data$children,xlab = 'children', ylab = 'count', main = 'Boxplot of children')
boxplot(raw_data$charges,xlab = 'charges', ylab = 'count', main = 'Boxplot of charges')

# 

quantile(raw_data$charges)


# upper limit
iqr = IQR(raw_data$charges)
iqr

upper_limit = 16639.913 + 1.5 * iqr
upper_limit

data = subset(raw_data, raw_data$charges < upper_limit)
data

boxplot(data$charges, xlab = 'charges', ylab = 'count', main = 'Boxplot of plot')

# categorical features

barplot(sort(table(data$smoker),decreasing = T))

barplot(sort(table(data$region),decreasing = T))

data_selected = subset(data,select = -c(region))
data_selected

# assumptions
cor(data_selected[c('age', 'children','bmi')])

# now check normality
# mormality is used by the histograms
hist(data$bmi,xlab = 'bmi',ylab = 'count',main = 'dist of bmi')
hist(data$charges,xlab = 'charges',ylab = 'count',main = 'dist of charges')
hist(data$age,xlab = 'age',ylab = 'count',main = 'dist of age')

plot(charges ~ age,data = data_selected)
plot(charges ~ bmi,data = data_selected)

# dummy creation
dmy = dummyVars("~.", data = data_selected, fullRank = T) # tilda. is used to change all 
# of the variables into dummy creation
# fullrank = T meaning only take only single value to create the dummy
dmy

data_dummies = data.frame(predict(dmy,newdata = data_selected))
data_dummies



?dummyVars


# standardization of data 
data_standardized = data.frame(scale(data_dummies))
data_standardized
# lthis data have the mean 0 and standard deviation 1


summary(data_standardized)

# model creation LR model
## Train-test Split
dt = sort(sample(nrow(data_standardized), nrow(data_standardized)*.8))

train = data_standardized[dt,]
test = data_standardized[-dt,] 

# Building the model
linear_model = lm(charges~age+bmi+sexmale+children+smokeryes,data = data.frame(train))

summary(linear_model)

# hence from this we can see that the lower p value is significant but higher p-value is 
## insignificant like p value of 0.05 and more than that is not important

# make predictions

distpred = predict((linear_model),data = data.frame(test))

# RMSE
rmse(data.frame(test)$charges,distpred)










