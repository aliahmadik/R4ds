
#author :Ali Ahmadi
#email : ahmadikatouli@gmail.com
#Q1
library(caret)
library(e1071)
library(dplyr)
data2 <- read.csv("D:\\R4ds/project_Materials/dataset2.csv")
#-------------------------------------------------------------------------------
#Q2
str(data2)
summary(data2)
dim(data2)
head(data2)
table(data2$PersonalLoan)

#change categorical variables into factors
data2$PersonalLoan = as.factor(data2$PersonalLoan)
data2$CreditCard = as.factor(data2$CreditCard)
data2$Online = as.factor(data2$Online)
data2$CDAccount = as.factor(data2$CDAccount)
data2$SecuritiesAccount = as.factor(data2$SecuritiesAccount)
data2$Education = as.factor(data2$Education)
data2$Family = as.factor(data2$Family)
#-------------------------------------------------------------------------------
#Q3

#1. Histogram of continous variables
continous_variables <- c("Age","Experience","Income","CCAvg","Mortgage")
par(mfrow = c(2,3))
for (var in continous_variables){
  hist(data2[,var],main = paste0("Histogram of ",var),xlab = var)
}
par(mfrow = c(1,1))

#2. Correlation table of continous variable
cor_table = round(cor(data2[,continous_variables]),3)
cor_table

#3. Frequency distribution table of categorical variables
factorial_variables = c("Family","Education","SecuritiesAccount","CDAccount","Online","CreditCard","PersonalLoan")
for (var in factorial_variables){
  print(paste0("Frequency Table of Variable ",var))
  print(table(data2[,var]))
}
#remove Experience < 0 from dataset
data2 <- data2[-which(data2$Experience < 0 ),]
#-------------------------------------------------------------------------------
#Q4
#split data set into train and test
set.seed(1234)
train_cases <- sample(1:nrow(data2),nrow(data2)*0.8)
train = data2[train_cases,]
test = data2[-train_cases,]
#1.
summary(train)
summary(test)

#-------------------------------------------------------------------------------
#Q5 : Logistic Regression m1

m1 <- glm(PersonalLoan~.,data = train,family = "binomial")
#1. summary of model
summary(m1)
#2. wald test and chi-square
modelChi1 <- m1$null.deviance - m1$deviance
Chidf1    <- m1$df.null - m1$df.residual
Chisq_prob1 <- 1 - pchisq(modelChi1, Chidf1)
Chisq_prob1
#AIC: 931.31
#wald test --> Age,Experience,Faimily2,Mortgage 

#3. size of sample
table(train$PersonalLoan)
nrow(train[train$PersonalLoan == 1,])/nrow(train) * 100
#380/4000
length(names(train))-1
# 10 * (#number of predictors) / pr( y = 1)
print(paste0("train size should be at least greater than ",round((10*11)/0.0955)))
#-------------------------------------------------------------------------------

#Q6 Logistic Regression m2
#train without family == 2
train_new <- train %>%
                        filter(Family != 2)
m2 <- glm(PersonalLoan~Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard,
          data = train_new,family = "binomial")
#1. summary of model
summary(m2)
#2. wald test and chi-square
modelChi1 <- m2$null.deviance - m2$deviance
Chidf1    <- m2$df.null - m2$df.residual
Chisq_prob1 <- 1 - pchisq(modelChi1, Chidf1)
Chisq_prob1
#AIC: 634.09

#-------------------------------------------------------------------------------
#Q7 
test_new <- test %>%
  filter(Family != 2)
test_new$probs <- predict(m2, test_new, type = "response")
test_new$pred_lg <- ifelse(test_new$probs >= 0.5, 1, 0)

#check the percent of accurate prediction
sum(test_new$pred_lg == test_new$PersonalLoan) / nrow(test_new) * 100 
#use confusion matrix in caret library
confusion_matrix <- table(Actual = test_new$PersonalLoan, Predicted = test_new$pred_lg)
print(confusion_matrix)
# Extract values from the confusion matrix
TP <- confusion_matrix[2, 2]  # True Positives
TN <- confusion_matrix[1, 1]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
# Compute Accuracy, Precision,Sensitivity,Specificity
accuracy <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

#-------------------------------------------------------------------------------
#Q8
m3 <- naiveBayes(PersonalLoan~. , data = train_new)
summary(m3)
test_new$naive_probs <- predict(m3,test_new)
#check the percent of accurate prediction
sum(test_new$naive_probs == test_new$PersonalLoan) / nrow(test_new) * 100 
confusion_matrix <- table(Actual = test_new$PersonalLoan, Predicted = test_new$naive_probs)
print(confusion_matrix)
# Extract values from the confusion matrix
TP <- confusion_matrix[2, 2]  # True Positives
TN <- confusion_matrix[1, 1]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
# Compute Accuracy, Precision,Sensitivity,Specificity
accuracy <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
