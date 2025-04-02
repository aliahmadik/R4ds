# Regression Analysis of Survey Data

This project performs regression analysis on survey data collected from a recreational park. The analysis explores the relationships between various factors (e.g., waiting times, number of children, cleanliness) and overall visitor satisfaction. The project includes data cleaning, visualization, and multiple regression models to identify significant predictors of satisfaction.

## Table of Contents
- [Overview](#overview)
- [Dataset](#dataset)
- [Analysis Steps](#analysis-steps)
- [Requirements](#requirements)
---

## Overview
The goal of this project is to:
1. Understand the relationships between survey variables and overall satisfaction.
2. Build regression models to predict visitor satisfaction.
3. Evaluate the assumptions of regression models (e.g., normality, multicollinearity).
4. Identify significant predictors and interaction effects.

---

## Dataset
The dataset contains the following variables:
- **overall**: Overall visitor satisfaction (target variable).
- **wait**: Waiting time for rides.
- **rides**: Number of rides taken.
- **games**: Number of games played.
- **clean**: Cleanliness rating.
- **distance**: Distance traveled to the park.
- **weekend**: Whether the visit occurred on a weekend (`Yes`/`No`).
- **num.child**: Number of children accompanying the visitor.

The dataset is loaded from the file `survey.csv`.

---

## Analysis Steps
The analysis is divided into the following steps:
1. **Data Loading**: Load the dataset and inspect its structure.
2. **Data Cleaning**: Handle missing values and convert categorical variables.
3. **Data Visualization**: Plot histograms and compute correlations.
4. **Simple Regression**: Fit a simple linear regression model.
5. **Multiple Regression**: Fit a model with multiple predictors.
6. **Model Diagnostics**: Evaluate residuals, normality, and multicollinearity.
7. **Interaction Effects**: Include interaction terms in the regression model.
8. **Final Model**: Build the final regression model with significant predictors.

---

## Requirements
To run this project, you need the following:
- **R** (version 4.0 or higher)
- R packages:
  - `knitr`
  - `ggplot2`
  - `moments`
  - `car`

Install the required packages using the following command in R:
```{r}
install.packages(c("knitr", "ggplot2", "moments", "car"))
```
