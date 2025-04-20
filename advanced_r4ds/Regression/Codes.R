#project Advanced Data Analysis with R 
#author : Aliahmadi ahmadikatouli@gmail.com
library(ggplot2)
library(moments)
df = read.csv("D:\\Programming Practices/R4ds/advanced_r4ds/Regression/dataset1.csv")
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


#-------------------------------------------------------------------------------
#Q4
set.seed(123456)
train_cases <- sample(1:nrow(df), nrow(df) * 0.7)
train <- df[train_cases,]
test <- df[- train_cases,]
dim(train)
summary(train)
dim(test)
summary(test)

#-------------------------------------------------------------------------------
# Q5: Simple Regression of KM over Price
# Fit a linear regression model with Price as the dependent variable and KM as the independent variable
m1 <- lm(Price ~ KM, data = train)

# Display the summary of the regression model
summary(m1)

# Check the normality assumption of residuals
# Plot a histogram of the residuals with a density line overlaid
hist(m1$residuals, probability = TRUE, 
     main = "Histogram of Residuals", 
     xlab = "Residuals")
lines(density(m1$residuals), col = "red", lwd = 3)

# Create a Q-Q plot to visually assess the normality of residuals
qqnorm(m1$residuals, main = "Q-Q Plot of Residuals")
qqline(m1$residuals, col = "red")

# Test for skewness and kurtosis of residuals
# These tests are suitable for sample sizes greater than 25

# Perform the Jarque-Bera test for normality
# If p-value < 0.05, reject the null hypothesis of normality
jarque.test(m1$residuals)

# Perform the Anscombe-Glynn test for kurtosis
# If p-value < 0.05, reject the null hypothesis of normality
anscombe.test(m1$residuals)

# Diagnostic plots for the regression model
# These plots help assess the assumptions of linear regression
plot(m1)
#-------------------------------------------------------------------------------
#Q6
m2 <- lm(Price~KM+I(KM^2),data = train)
summary(m2)
hist(m2$residuals,probability = TRUE)
lines(density(m2$residuals),col = "red",lwd = 3)

qqnorm(m2$residuals)
qqline(m2$residuals,col = "red")

jarque.test(m1$residuals)
anscombe.test(m1$residuals)

plot(m2)

ggplot(train,aes(KM,Price))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x + I(x^2),color = "blue",se = FALSE)+
  geom_smooth(method = "lm",formula = y~x , color = "red",se = FALSE)+
  ggtitle("Price vs KM")
#-------------------------------------------------------------------------------
#Q7
train2 =train[-which(rownames(train) == 32|
                        rownames(train)== 33|
                        rownames(train) == 7),] 
dim(train)
dim(train2)
m2_2 <- lm(Price~KM + I(KM^2),data = train2)
summary(m2_2)

hist(m2_2$residuals,probability = TRUE)
lines(density(m2_2$residuals),col = "red",lwd = 3)

qqnorm(m2_2$residuals)
qqline(m2_2$residuals,col = "red")

jarque.test(m2_2$residuals)
anscombe.test(m2_2$residuals)

plot(m2_2)

summary(m2)
summary(m2_2)
#-------------------------------------------------------------------------------

#Q8
##Check for Multicollinearity--------------------
#Calculate variance inflation factor (VIF):  VIF = 1/(1-R^2)
car :: vif(m2_2)
#If VIF > 10 then multicollinearity is high


train2$KM_scaled <- scale(train2$KM)
head(train2)
m2_3 <- lm(Price ~ KM_scaled + I(KM_scaled^2), data = train2)
#summary of m2_3 model
summary(m2_3)
#check normality assumption of residuals
hist(m2_3$residuals,probability = TRUE)
lines(density(m2_3$residuals),col = "red")

qqnorm(m2_3$residuals)
qqline(m2_3$residuals)

# Perform the Jarque-Bera test for normality
# If p-value < 0.05, reject the null hypothesis of normality
jarque.test(m2_3$residuals)
# Perform the Anscombe-Glynn test for kurtosis
# If p-value < 0.05, reject the null hypothesis of normality
anscombe.test(m2_3$residuals)
#If VIF > 10 then multicollinearity is high
car::vif(m2_3)
plot(m2_3)

#-------------------------------------------------------------------------------
#Q9:
m3 <- lm(Price~ Age+FuelType+HP+MetColor+Automatic+CC+Doors+Weight+KM_scaled,data = train2)
#summary of model m3
summary(m3)

m3 <- lm(Price~ Age+FuelType+HP+Automatic+CC+Weight+KM_scaled,data = train2)
summary(m3)

#-------------------------------------------------------------------------------

#Q10:
train2$IfPetrol <- ifelse(train2$FuelType == "Petrol", TRUE, FALSE)
head(train2)

m4 <- lm(Price~ Age+HP+Automatic+Weight+KM_scaled+IfPetrol,data = train2)
#summary of model m3
summary(m4)

hist(m4$residuals,probability = TRUE)
lines(density(m4$residuals),col = "red")

qqnorm(m4$residuals)
qqline(m4$residuals)

# Perform the Jarque-Bera test for normality
# If p-value < 0.05, reject the null hypothesis of normality
jarque.test(m4$residuals)
# Perform the Anscombe-Glynn test for kurtosis
# If p-value < 0.05, reject the null hypothesis of normality
anscombe.test(m4$residuals)
#If VIF > 10 then multicollinearity is high
car::vif(m4)
plot(m4)

#-------------------------------------------------------------------------------
dim(train2)
train3 <- train2[-which(rownames(train2) == 112|
                                 rownames(train2)== 850|
                                 rownames(train2) == 491|
                                  rownames(train2) == 284),]
m4_2 <- lm(Price~ Age+HP+Automatic+Weight+KM_scaled+IfPetrol,data = train3)
#summary of model m4
summary(m4_2)
plot(m4_2)

train3 <- train3[-which(rownames(train3) == 114|
                          rownames(train3)== 544|
                          rownames(train3) == 293|
                          rownames(train3) == 881),]
m4_2 <- lm(Price~ Age+Weight+KM_scaled+IfPetrol,data = train3)
summary(m4_2)
plot(m4_2)

train3 <- train3[-which(rownames(train3) == 803|
                          rownames(train3)== 948|
                          rownames(train3) == 1325|
                          rownames(train3) == 77),]
m4_2 <- lm(Price~ Age+Weight+KM_scaled+IfPetrol,data = train3)
summary(m4_2)
plot(m4_2)

train3 <- train3[-which(rownames(train3) == 586|
                          rownames(train3)== 127|
                          rownames(train3) == 1241|
                          rownames(train3) == 944),]
m4_2 <- lm(Price~ Age+Weight+KM_scaled+IfPetrol,data = train3)
summary(m4_2)
plot(m4_2)

train3 <- train3[-which(rownames(train3) == 1272|
                          rownames(train3)== 354|
                          rownames(train3) == 716),]
dim(train3)
m4_2 <- lm(Price~ Age+Weight+KM_scaled+IfPetrol,data = train3)
summary(m4_2)
plot(m4_2)

hist(m4_2$residuals,probability = TRUE)
lines(density(m4_2$residuals),col = "red")

qqnorm(m4_2$residuals)
qqline(m4_2$residuals)

# Perform the Jarque-Bera test for normality
# If p-value < 0.05, reject the null hypothesis of normality
jarque.test(m4_2$residuals)
# Perform the Anscombe-Glynn test for kurtosis
# If p-value < 0.05, reject the null hypothesis of normality
anscombe.test(m4_2$residuals)
#If VIF > 10 then multicollinearity is high
car::vif(m4_2)

#-------------------------------------------------------------------------------
#Q11:
#Test the model
test$KM_scaled <- scale(test$KM)
test$IfPetrol <- ifelse(test$FuelType == "Petrol", TRUE, FALSE)
dim(test)
dim(train)
test$pred <- predict(m4_2, test)

# محاسبه خطاها و قدر مطلق خطاها
test$error <- test$Price - test$pred
test$abs_error <- abs(test$error)

# محاسبه آماره‌های مورد نظر
mean_abs_error <- mean(test$abs_error)
sd_abs_error <- sd(test$abs_error)
max_abs_error <- max(test$abs_error)
min_abs_error <- min(test$abs_error)

# چاپ نتایج
cat("میانگین قدر مطلق خطاها:", mean_abs_error, "\n")
cat("انحراف معیار قدر مطلق خطاها:", sd_abs_error, "\n")
cat("بیشینه قدر مطلق خطاها:", max_abs_error, "\n")
cat("کمینه قدر مطلق خطاها:", min_abs_error, "\n")


#predicted vs actual
plot(test$Price, test$pred, main = "Predicted vs Actual Price")
abline(a = 0, b = 1, col = "red", lwd = 2)

hist(test$error, 
     main = "Histogram of Prediction Errors", 
     xlab = "Error (Price - Predicted)", 
     col = "lightblue", 
     border = "darkblue")

boxplot(test$error, 
        main = "Boxplot of Prediction Errors", 
        ylab = "Error (Price - Predicted)", 
        col = "orange")

test$percent_errorabs<- (test$Price - test$pred)/test$Price * 100

mean_per_error <- mean(test$percent_errorabs)
sd_per_error <- sd(test$percent_errorabs)
max_per_error <- max(test$percent_errorabs)
min_per_error <- min(test$percent_errorabs)

# چاپ نتایج
cat("میانگین قدر مطلق خطاها:", mean_per_error, "\n")
cat("انحراف معیار قدر مطلق خطاها:", sd_per_error, "\n")
cat("بیشینه قدر مطلق خطاها:", max_per_error, "\n")
cat("کمینه قدر مطلق خطاها:", min_per_error, "\n")

#percentage of error between -15% and + 15%
sum(test$percent_errorabs < 15 & test$percent_errorabs > -15) / dim(test)[1] * 100