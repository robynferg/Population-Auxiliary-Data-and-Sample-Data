# Readme

## Scripts
- newEstimator.R: R script containing function for applying new estimation method using either OLS, random forest, or sample mean
- collegeTuition.R: R script for predicting mean college tuition from admission rate, graduation rate, student to staff ratio, public or private, and highest degree offered. Uses 'collegeData.csv'. Using a simple random sample, this script compares the bias of the new method to OLS imputation, difference between MSE using the new method and OLS imputation, and standard error of the new method compared to the sample mean. Using a non-random sample, gives confidence intervals for bias using the new method, OLS imputation, and sample mean. 

## Data
- collegeData.csv: csv file containing data on colleges for use in 'collegeTuition.R' script. Data was originally obtained from the IPEDS database (https://nces.ed.gov/ipeds/).
