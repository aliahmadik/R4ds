# Advanced Data Analysis of a Banking Campaign using R

## Description
This project is part of an advanced data analysis course using R. The goal is to build a predictive model to determine whether customers will apply for a personal loan after being exposed to a marketing campaign. The dataset contains demographic and banking-related characteristics for 5,000 customers, with the target variable indicating whether they applied for a loan (`1` for yes, `0` for no).

## Dataset
The dataset used in this project is named `dataset2.csv` and includes the following variables:

- **PersonalLoan**: Indicates if the customer applied for a loan (`1` for yes, `0` for no).
- **Age**: The age of the customer.
- **Experience**: The number of years of professional experience.
- **Income**: The income level of the customer.
- **Family**: The number of family members.
- **Education**: The highest education level, categorized into three levels.
- **CCAvg**: Average monthly credit card spending.
- **Mortgage**: Amount of mortgage.
- **SecuritiesAccount**, **CDAccount**: Indicator variables for having respective accounts.
- **Online**: Indicates access to online banking.
- **CreditCard**: Indicates if the customer has a credit card.

## Analysis Steps

### 1. Data Loading
The dataset was loaded into R and stored in a dataframe named `data2` using the following libraries:
```r
library(caret)
library(dplyr)
library(e1071)

data2 <- read.csv("path/to/dataset2.csv")
```

### 2. Data Exploration
Various functions were used to explore the dataset:

- `str(data2)`: Structure of the dataset
- `summary(data2)`: Descriptive statistics
- `dim(data2)`: Dimensions of the dataset
- `head(data2)`: First few rows of the dataset

Categorical variables were changed into factors for better analysis.

### 3. Statistical Analysis of Variables
- Histograms were created for continuous variables to visualize distributions.
- A correlation table was generated for continuous variables to analyze their relationships.
- Frequency distribution tables were created for categorical variables.

### 4. Train-Test Split
The dataset was split into training (80%) and testing (20%) sets using:
```r
set.seed(1234)

train_cases <- sample(1:nrow(data2), nrow(data2) * 0.8)

train = data2[train_cases,]

test = data2[-train_cases,]
```

### 5. Logistic Regression Model
A logistic regression model was built using:
```r
m1 <- glm(PersonalLoan ~ ., data = train, family = "binomial")
```
Several variables were evaluated for significance and some were removed based on p-values and correlation.

### 6. Model Refinement
A new model (`m2`) was created by removing non-significant variables, leading to improved performance metrics, including AIC.

### 7. Predictions and Evaluation
The final model (`m4`) was used to predict loan applications on the testing dataset, leading to an accuracy of 95.90%, with a confusion matrix revealing the specifics of true positives and false positives.

### 8. Alternative Model: Naive Bayes
A Naive Bayes model (`m3`) was also created for comparison, achieving an accuracy of 93%. Performance metrics were compared against the logistic regression model.

## Conclusion
The logistic regression model (`m2`) was preferred due to its better performance in minimizing false positives while maintaining a high precision. This project demonstrates the step-by-step analysis and modeling process, offering insights into customer behaviors and optimizing marketing strategies for personal loans.

## Installation and Usage
To run this project locally, ensure you have R and the necessary libraries installed. Clone this repository and execute the R script in an R environment.
```bash
git clone <repository-url>