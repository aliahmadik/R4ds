# Author :Ali Ahmadi 
# Email : ahmadikatouli@gmail.com
#Project : Feature of Breast Cancer and their relation to the Target Variable

#import necessary libraries
library(tidyverse)
library(dplyr)
library(class)
library(epitools)

#load original data ( for rerun the project change this address with your local file address)
data <- read.csv('D://DLearn/rmda/breastcancer/breastcancer.csv',header = TRUE)

#-------------------------------------------------------------------------------
# Cleaning Data set
#-------------------------------------------------------------------------------
#check for NA's
sum(is.na(data))
#check for other symbols for NA's
sum(data == '?')
#remove these data with '?' symbol as NA
data[data == '?'] = NA
data = drop_na(data)

summary(data)
# all columns are character . however all of them has categorical meaning.
# for our purpose here, we change them to factor

data <- data %>%
  mutate(across(everything(),as.factor))
str(data)  

#check for duplicated rows
sum(duplicated(data))
# there are 14 duplicated rows
data = unique(data)
 
#-------------------------------------------------------------------------------
# logistic Regression
#-------------------------------------------------------------------------------

data_clean <- data %>%
  mutate( target= ifelse(target == "recurrence-events", 1, 0))%>%
  mutate( breast= ifelse(breast == "right", 1, 0))%>%
  mutate( node.caps= ifelse(node.caps == "yes", 1, 0))%>%
  mutate( irradiat= ifelse(irradiat == "yes", 1, 0))%>%
  mutate(breast.quad = recode(breast.quad,"central" = 1, "left_low" = 2,"left_up" = 3,
                              "right_low" = 4,"right_up" = 5))%>%
  mutate(menopause = recode(menopause, "ge40" = 1,"lt40" = 2,"premeno" = 3))


unique(data_clean$age)
data_clean <- data_clean %>%
  mutate(age = recode(age ,"20-29" = 1,"30-39" = 2, "40-49" = 3,"50-59" = 4, "60-69" = 5,"70-79" = 6))

data_clean <- data_clean %>%
  mutate(tumor.size = recode(tumor.size, "0-4" = 1,"5-9" = 2,"10-14"=3, "15-19" =4,
                             "20-24" = 5,"25-29" = 6 ,"30-34" = 7, "35-39" = 8, "40-44" = 9, "45-49" = 10,  "50-54" = 11
                             ))
data_clean <- data_clean %>%
  mutate(inv.nodes = recode(inv.nodes,  "0-2" = 1,"3-5" = 2,"6-8"=3,"9-11" = 4,"12-14" = 5,"15-17" = 6,"24-26" = 7 ))
data_clean$deg.malig = as.numeric(data$deg.malig)
str(data_clean)

# Normalize Data
dlog<- data_clean
numeric_vars <- c("menopause", "node.caps", "deg.malig", "breast", "breast.quad", "irradiat","age","tumor.size","inv.nodes")
dlog[numeric_vars] <- lapply(data_clean[numeric_vars], scale)


# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dlog), 0.7 * nrow(dlog))
train_data <- dlog[train_indices, ]
test_data <- dlog[-train_indices, ]

names(dlog)
# Fit the Logistic Regression Model
model <- glm(target ~ .,
             data = train_data, 
             family = binomial)


summary(model)

predicted_probs <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary outcome (use a threshold of 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(test_data$target, predicted_classes)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

plot(model)

#-------------------------------------------------------------------------------
#k-Nearest Neighbors
#-------------------------------------------------------------------------------
# k= 6

dk <- data_clean[, c("target","menopause", "node.caps", "deg.malig", "breast", "breast.quad", "irradiat","age","tumor.size", "inv.nodes")]
train_indices <- sample(1:nrow(dk), 0.7 * nrow(dk))
train_features <- dk[train_indices, ]
test_features <- dk[-train_indices, ]

knn_predictions <- knn(train = train_features,
                       test = test_features,
                       cl = train_features$target,
                       k = 6)
# Model Evaluation
confusion_matrix <- table(Predicted = knn_predictions, Actual = test_features$target)
# Create confusion matrix
print(confusion_matrix)
correct_predictions <- sum(diag(confusion_matrix))
total_predictions <- sum(confusion_matrix)

# Calculate accuracy
accuracy <- correct_predictions / total_predictions
# Calculate precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[ ,2])
# Calculate recall
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

print (confusion_matrix)

print(paste("Accuracy :", round(accuracy, 3)))
print(paste("Recall for k = 6:", round(recall, 3)))
print(paste("Precision for k = 6:", round(precision, 3)))

#-------------------------------------------------------------------------------
#chi-square test
#-------------------------------------------------------------------------------

df = data_clean
str(df)
# Identify categorical columns (factor type)
df <- lapply(df, as.factor)
categorical_cols <- sapply(df, is.factor)
categorical_cols <- names(categorical_cols[categorical_cols])

# Create an empty data frame to store the results
results <- data.frame(
  Variable = character(),
  Chi_Square = numeric(),
  df = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

#Loop through categorical columns (excluding "target")
for (col in categorical_cols[!categorical_cols %in% "target"]) {
  # check if the variable has more than one level
  if (nlevels(df[[col]]) > 1){
    # Create a contingency table
    contingency_table <- table(df$target, df[[col]])
    # Perform chi-square test
    chi_square_test <- chisq.test(contingency_table)
    # Store results
    results <- rbind(results, data.frame(
      Variable = col,
      Chi_Square = chi_square_test$statistic,
      df = chi_square_test$parameter,
      p_value = chi_square_test$p.value
    ))
  } else {
    print(paste("The variable ", col, " have one level only, no test possible"))
  }
}
variables <- results[results$p_value <= 0.05,'Variable']
# These variables have p-value less than 0.05
variables

# Plot these variables 
str(data)
data$inv.nodes = factor(data$inv.nodes,levels = c("0-2","3-5","6-8","9-11","12-14","15-17","24-26"))
ggplot(data,aes(x = tumor.size,fill = target))+geom_bar()
ggplot(data,aes(x = inv.nodes,fill = target))+geom_bar()
ggplot(data,aes(x = node.caps,fill = target))+geom_bar()
ggplot(data,aes(x = deg.malig,fill = target))+geom_bar()
ggplot(data,aes(x = irradiat,fill = target))+geom_bar()
#-------------------------------------------------------------------------------
#man-vitney test
#-------------------------------------------------------------------------------

categorical_cols <- names(data_clean)

# Create an empty data frame to store the results
results_df <- data.frame(
  Variable = character(),
  man_vitny = numeric(),
  df = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through categorical columns (excluding "target")
for (col in categorical_cols[!categorical_cols == "target"]) {
  # Check if the variable has exactly two levels
  if (length(unique(data_clean[[col]])) == 2) {
    # Create a formula string for Wilcoxon test
    formula_string <- paste(col, "~ target")
    
    # Perform the Wilcoxon test
    test_result <- wilcox.test(as.formula(formula_string), data = data_clean)
    print(paste0(col,' ,p_value= ',test_result$p.value))
  }
}

#-------------------------------------------------------------------------------
# Cross - Sectional Analysis 
#-------------------------------------------------------------------------------

d_or = data_clean
freqtable <- lapply(d_or,table)
features <- c("irradiat", "breast", "node.caps")

for (feature in features) {
  
  crosstable <- table(d_or[[feature]], d_or$target)
  propT <- addmargins(prop.table(crosstable))
  propT2 <- prop.table(crosstable)              
  chi <- chisq.test(crosstable)
  or <- epitools::oddsratio(crosstable)
  epi_table <- epitab(crosstable, method = c("oddsratio"), conf.level = 0.95, rev = c("columns"))
  
  # Print results
  cat("Results for feature:", feature, "\n")
  print(propT)       # Print proportion table with margins
  print(chi)        # Print Chi-squared test results
  print(or)         # Print odds ratio
  print(epi_table)  # Print epidemiological table
  cat("\n\n")
}
