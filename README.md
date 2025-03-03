# Problem Set 1 - Predicting Income 
## Juan Pablo Grimaldos 
## Isabella Garzón 

This is our Problem Set 1 Repository for Big Data & Machine Learning for Applied Economics

---

This repository contains the following folders: 
---

**document:**
 The `document/` folder contains the final project document in `pdf` format. 
 
 - [Problem Set 1](document/Problem Set 1.pdf)

---

**scripts:**
  The `scripts/` folder contains the following files, which include functions and analysis in R for this project: 
  Each script is documented with comments to facilitate understanding and usage.

- [Cleaning Data](scripts/1-CleaningData.R): Loading and cleaning the dataset.  
- [Descriptive Variables](scripts/2-DescriptiveVariables.R): Computes summary statistics.  
- [Age-Wage Profile](scripts/3-AgeWageProfile.R): Analyzes wage distribution by age.  
- [Gender Earnings Gap](scripts/4-GenderEarningsGap.R): Analyzes gender-based wage differences.  
- [Predicting Earnings](scripts/5-PredictingEarnings.R): Uses alternative tests to evaluate the predictive pattern of previous specifications.  


---

**stores:**

 The `stores/` folder contains a single, csv file where all of the data chunks are included. This is the database our project utilizes. The data corresponds to a sample of the GEIH 2018.

- [GEIH 2018 full sample database](stores/GEIH2018_FULLCOMBINEDTABLES.csv): The full database used. Contains all 10 data chunks. 
---

**views:**

  The `views/` folder contains the eight (8) tables / figures utilized in the project results document. Tables are included in html (for easy visualization) and LaTeX format, and the figures are included in PDF format. 
  DISCLAIMER: PLEASE DOWNLOAD THE HTML FILES AND OPEN THEM IN A BROWSER IN ORDER TO VISUALIZE THE TABLE OUTPUT FROM R.
  
- [Table 1. Descriptive statistics – Continuous variables](views/summarystatscont22.htm): Table 1, containing descriptive statistics for continuous variables.
- [Table 2. Regression Results: Nominal and Real Income](views/regtable31.htm): Table 2, showing regression results from running Models 2, 3, 4, and 5 mentioned in the document.
- [Figure 1. Predicted Age-Wage Profile with Confidence Intervals](views/age_wage_plot.pdf): Figure 1, displaying the predicted age-wage profile of Models 2 and 3 with confidence intervals in a graph.
- [Table 3. Regression Results: Wage Gap Models](views/regression_results41.tex): Table 3, displaying the estimated gender-earnings gap for Models 6, 7, and Model 7 being ran by Frisch-Waugh-Lovell decomposition.
- [Figure 2. Predicted Age-Wage Profile with Confidence Intervals by Sex](views/age_wage_plot_by_sex.pdf): Figure 2, displaying the predicted age-wage profile (by sex) of Models 7.1 and 7.2 with confidence intervals in a graph.
- [Table 4. Predictive Performance Results: RMSE](views/table51.htm): Table 4, which contains a summary of the predictive performance of specified models in terms of RMSE.
- [Figure 3. Distribution of Errors in the Model With Best Performance](views/error_distribution.pdf): Figure 3, displaying a graph with the distribution of errors of the model with best fit, diagnosing potetial outliers.
- [Table 5. Comparison LOOCV and Validation Set Approach](views/table52.html): Table 5, displaying the LOOCV estimation for the two models with the lowest prediction errors, comparing with the validation set approach.



---



