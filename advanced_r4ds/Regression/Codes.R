#project Advanced Data Analysis with R 
#author : Aliahmadi ahmadikatouli@gmail.com
library(tidyverse)
df = reatidyversedf = read.csv("D:\\R4ds-FarzadMinoui/project_Materials/dataset1.csv")
#overview of data
str(df)
summary(df)
dim(df)
head(df)
#Data cleaning
#there is no NA's in dataset
sum(is.na(df))
#change class of variables (FuleType,MetColor,Automatic,Doors) to factors
class(df$FuelType)
table(df$FuelType)
df$FuelType = factor(df$FuelType,levels = c("Petrol","Diesel","CNG"))
df$MetColor = as.factor(df$MetColor)
df$Automatic = as.factor(df$Automatic)
df$Doors = as.factor(df$Doors)
table(df$FuelType)
# Data inspection
#histogram of continous variables

continous_variables = c("Price","Age","KM","HP","Weight")
par(mfrow = c(2,3))
for (i in continous_variables){
  hist(df[,i],main = paste0("Histogram of ",i),xlab = i)
}
par(mfrow = c(1,1))
par(mfrow = c(2,3))

for (i in continous_variables[-1]){
  plot(df[,i],df$Price,main = paste0("Plot of Price vs ",i),xlab = i,ylab =  "Price")
}
par(mfrow = c(1,1))
cor_table = round(cor(df[,continous_variables]),2)
cor_table

factorial_variables = c("FuelType","MetColor","Automatic","Doors")
for (i in factorial_variables){
  print(paste0("Frequency Table of Variable ",i))
  print(table(df[,i]))
}



###Q4
#Devide dataset
set.seed(123456)
train_cases <- sample(1:nrow(df), nrow(df) * 0.7)
train <- df[train_cases,]
test <- df[- train_cases,]
dim(train)
summary(train)
dim(test)
summary(test)
#Q5
#Simple Regression of KM over Price

m1 <- lm(Price~KM,data = train)
summary(m1)
#check normality assumption of residuals
hist(m1$residuals,probability = TRUE)
lines(density(m1$residuals),col = "red",lwd = 3)

qqnorm(m1$residuals)
qqline(m1$residuals,col = "red")

plot(m1)

#Q6
m2 <- lm(Price~KM+I(KM^2),data = df)
summary(m2)
hist(m2$residuals,probability = TRUE)
lines(density(m2$residuals),col = "red",lwd = 3)

qqnorm(m2$residuals)
qqline(m2$residuals,col = "red")

plot(m2)

ggplot(train,aes(KM,Price))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x + I(x^2),color = "blue",se = FALSE)+
  geom_smooth(method = "lm",formula = y~x , color = "red",se = FALSE)+
  ggtitle("Price vs KM")

#Q7
train_2 =train[-which(rownames(train) == 32|
                        rownames(train)== 33|
                        rownames(train) == 7),] 
dim(train)
dim(train_2)
m2_2 <- lm(Price~KM + I(KM^2),data = train)
summary(m2_2)
plot(m2_2)
