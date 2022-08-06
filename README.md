# Predicting State Medicare Spending Per Beneficiary
Xuefei Qiao

## Abstract
This is the report to explore what features may influence the State Medicare Spending Per Beneficiary (MSPB) and how they influence it through picking the best-fit Regression model among one pruned tree, simple linear regression, bagged trees and random forests to forecast the value.

*Index Terms: medical spending, simple linear regression, one pruned tree, bagged trees, random forests*


## Introduction
The State Medicare Spending Per Beneficiary (MSPB) measure can be used to evaluate medical efficiency and the cost of services performed by hospitals and other healthcare providers. According to the report from KFF (Kaiser Family Foundation), Medicare spending was 15 percent of total federal spending in 2018, and is projected to rise to 18 percent by 2029, and was 21% of total health spending in 2018 in the United States. 

It can be seen from the above that MSPB is a huge cost, and the large number of enrollees (92.9 million) also shows that the importance of the Medicare Plan, so I think it is necessary to learn how to forecast the MSPB accurately based on some features’ information to better control the medical budget.

## The Dataset
This is the nationwide small dataset with 51 observations and 12 variables provided by  [KFF](https://www.kff.org/state-category/medicare/medicare-enrollment-by-eligibility-category/). There is no missing value in the dataset, and 10 out of 12 variables will be used as predictors, and ‘MSPB’ will be used as a response variable.

Please click [here](https://github.com/xqiao123/Prediction_US_Medical_Spending/blob/main/Report.pdf) to view the complete report. 
