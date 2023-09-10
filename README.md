# Telecom Customer Churn Analysis

![Project Image](https://editor.analyticsvidhya.com/uploads/94357telecom%20churn.png)

## Overview
This repository contains code and analysis for a Telecom Customer Churn Analysis project. The project aims to answer various business questions related to customer churn by analyzing telecom customer data.

## Project Structure
- `TelcoChurnReport.pdf`: A detailed report of the project's findings.
- `TelcoChurn.r`: The R script containing data preprocessing, exploratory data analysis (EDA), and modeling.
- `TelcoChurn.xlsx`: The dataset used for the analysis.
- `README.md` (this file): Provides an overview of the project and its contents.

## How the Data Was Processed
The data processing steps in the R script (`TelcoChurn.r`) include:
- Importing the dataset.
- Feature engineering to convert categorical variables into numeric format.
- Handling missing values.
- Conducting exploratory data analysis (EDA).

## Model Building
Three logistic regression models were built:
1. Model for telephone customers.
2. Model for internet service customers.
3. Model for customers with both phone and internet services.

## Data Splitting
The data was split into training and test sets using a 75:25 ratio with a random seed of 1024.

## Top Predictors of Churn
The top predictors of churn were identified for each customer category (telephone, internet, both services) using logistic regression coefficients. Marginal effects were calculated to understand the predictor contributions.

## Model Evaluation
Each model was evaluated using test data, and the following metrics were computed:
- Recall
- Precision
- F1-Score
- AUC (Area Under the ROC Curve)

## Findings and Conclusions
Please refer to the `TelcoChurnReport.pdf` for detailed findings and conclusions from the analysis.

## How to Use This Repository
1. Clone the repository to your local machine.
2. Run the R script (`TelcoChurn.r`) to reproduce the analysis.
3. Review the `TelcoChurnReport.pdf` for insights and conclusions.

Feel free to reach out if you have any questions or need further assistance with the project.

Happy analyzing!

