# Problem Set 1
##########################################################
# Gender-Earnings Gap 
# Authors: Juan Pablo Grimaldos & Isabella Garzón 
##########################################################


# ------------------------------ GENDER-EARNINGS GAP ---------------------------

#a) Unconditional Wage Gap Model

# Estimate a linear regression to examine the effect of gender on log nominal hourly wage

model6 <- lm(log_nominal_income  ~ female, data= db)

# Display regression results for nominal wage model in a formatted table
stargazer(model6, type="text", 
          covariate.labels=c("Female"), 
          dep.var.labels = "Log Nominal Hourly Wage", 
          title = 'Unconditional Wage Gap Model')

#b) Conditional Wage Gap Model 

# Estimate a multiple regression model controlling for various variables

model7 <- lm(log_nominal_income ~ female + age +age2 + Employment_Sector + Head_Female +
               Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel, data=db)

omit_vars <- c("age", "age2", "Employment_Sector", "Head_Female", 
               "Weekly_Hours_Worked", "formal", "sizeFirm", "maxEducLevel")

# Generate the table
stargazer(model6, model7, 
          type = "text",
          covariate.labels = c("Female"),  
          dep.var.labels = c("Log Nominal Hourly Wage"),
          title = "Unconditional vs. Conditional Wage Gap Model",
          column.labels = c("Model 6", "Model 7"),  
          omit = omit_vars,  
          notes = "Model 7 includes additional controls: Age, Employment Sector, Female Household Head, Weekly Hours Worked, Formality, Firm Size, and Education.",
          style = "default",    # Explicitly setting style
          align = TRUE          # Ensure proper alignment
)
# FWL (Frisch-Waugh-Lovell) Decomposition --------------------------------------

# Compute residuals of female  ~ X 
# Isolating the variation in female that is uncorrelated with controls

db<-db %>% mutate(femaleResidX=lm(female~ age + age2 + Employment_Sector + Head_Female +
                                    Weekly_Hours_Worked + formal + 
                                    sizeFirm + maxEducLevel, db)$residuals) 

# Compute residuals of log nominal hourly wage ~ X 

db<-db %>% mutate(loghwageResidX=lm(log_nominal_income~ age + age2  
                                    + Employment_Sector + Head_Female +
                                      Weekly_Hours_Worked + formal + 
                                      sizeFirm + maxEducLevel, db)$residuals) 

# Model using residuals to confirm FWL theorem 

model7FWL <- lm(loghwageResidX~femaleResidX, db)
stargazer(model7, model7FWL, type="text",digits=7,
          covariate.labels=c("Female", "Age", "Employment Sector",
                             "Female Household Head", "Total Hours Worked", 
                             "Formal", "Size of Firm", "Max. Education Level", "FWL estimate"), 
          dep.var.labels = c("Log Nominal Hourly Wage", "Residualized Model"), 
          title = 'OLS vs FWL estimates')

# Computing the sum of squared residuals (SSR) for both models to confirm equivalence
SSROLSmodel7 <-sum(resid(model7)^2)
SSRFWLmodel7 <-sum(resid(model7FWL)^2) #Both models have the same SSR.

##Adjusting the degrees of freedom by the 7 residualized predictors:

df_OLSmodel7 = model7$df[1]
df_FWLmodel7 = model7FWL$df[1]

# Compute standard error adjustments 
sqrt(diag(vcov(model7FWL))*(df_FWLmodel7/df_OLSmodel7))[2]

sqrt(diag(vcov(model7)))[2] #Degrees of freedom have been succesfully adjusted.
#Standard error now matches the one of the original regression.



#FWL WITH BOOTSTRAP ------------------------------------------------------------

#Displaying regression and y_hat function by sex:
# Model for men:
model7men <- lm(log_nominal_income ~ female + age +age2 + Employment_Sector + Head_Female +
                  Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel, data=db,
                subset = (female == 0))
# Model for female: 
model7females <- lm(log_nominal_income ~ female + age +age2 + Employment_Sector + Head_Female +
                      Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel,
                    data=db,
                    subset = (female == 1))

# Create predicted values with NA for the opposite gender
db <- db %>%
  mutate(yhat1men = ifelse(female == 0, predict(model7men, newdata = db), NA),
         yhat2females = ifelse(female == 1, 
                               predict(model7females, newdata = db), NA))

# Summarize men observed and predicted wages by gender and age 
summ2 <- db %>%  
  group_by(
    female, age, age2
  ) %>%  
  summarize(
    mean_hourly_salary= mean(log_real_income),#Mean hourly salary for  sample
    yhat_reg7men = mean(yhat1men), #Predicted avg. hourly salary for this male
    yhat_reg7females = mean(yhat2females), .groups="drop"
  ) #Predicted avg. hourly salary for this female


# Separate male and female datasets 
summ2_men <- summ2 %>% filter(!is.na(yhat_reg7men)) #Data for males
summ2_females <- summ2 %>% filter(!is.na(yhat_reg7females)) #Data for females


#GRAPHING

# Open PDF device for saving in latex
pdf("../views/age_wage_plot_by_sex.pdf", width = 7, height = 5)  

# Compute standard errors for confidence intervals
summ2_men <- summ2_men %>%
  mutate(se = sd(yhat_reg7men, na.rm = TRUE) / sqrt(n()),
         lower = yhat_reg7men - 1.96 * se,
         upper = yhat_reg7men + 1.96 * se)

summ2_females <- summ2_females %>%
  mutate(se = sd(yhat_reg7females, na.rm = TRUE) / sqrt(n()),
         lower = yhat_reg7females - 1.96 * se,
         upper = yhat_reg7females + 1.96 * se)

# Graphing with confidence intervals and proper labels predicted wage profiles
ggplot() + 
  # Scatter plot of actual mean hourly salary
  geom_point(data = summ2, aes(x = age, y = mean_hourly_salary), 
             color = "blue", size = 2, alpha = 0.5) + 
  
  # Confidence interval for males
  geom_ribbon(data = summ2_men, 
              aes(x = age, ymin = lower, ymax = upper), 
              fill = "red", alpha = 0.2) +
  
  # Line for males
  geom_line(data = summ2_men, 
            aes(x = age, y = yhat_reg7men, color = "Males"), 
            linewidth = 1.5) + 
  
  # Confidence interval for females
  geom_ribbon(data = summ2_females, 
              aes(x = age, ymin = lower, ymax = upper), 
              fill = "purple", alpha = 0.2) +
  
  # Line for females
  geom_line(data = summ2_females, 
            aes(x = age, y = yhat_reg7females, color = "Females"), 
            linewidth = 1.5) +
  
  # Labels and title

  labs(
    title = "Predicted Age-Wage Profile with CIs by Sex",
    subtitle = "Based on regression models estimated separately for
    males and females",
    x = "Age",
    y = "Log Nominal Hourly Wages",
    color = "Sex",
    caption = "Note: Predicted values are 
    from separate regressions for males and females.\nConfidence 
    intervals (CIs) represent 95% uncertainty bands."
  ) +
  
  #Theme
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))


# Close PDF device and saves in latex
dev.off()

#BOOTSTRAPPING

# Set seed to make sure that results are reproducible 
set.seed(101112)

B <- 1000 # bootstrap replications

# Creating empty vectors of size B to score the estimates and peak age values for each iteration 
estimates_FWLmodel7<-rep(NA,B)

agesmax_FWLmodel7men<-rep(NA,B)
agesmax_FWLmodel7females<-rep(NA,B)

# Using a for loop
for(i in 1:B){
  
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
  
  #takes a sample with replacement of the same size of the original sample.
  
  femaleResidXbs <- lm(female~ age + age2+ Employment_Sector + Head_Female +
                         Weekly_Hours_Worked + formal + 
                         sizeFirm + maxEducLevel,
                       data = db_sample)$residuals
  
  # Computes residuals of regression female ~ X 
  
  loghwageResidXbs <- lm(log_nominal_income~ age +
                           age2 + Employment_Sector + Head_Female +
                           Weekly_Hours_Worked + formal + 
                           sizeFirm + maxEducLevel, 
                         data = db_sample)$residuals
  
  # Compute residuals of regression log nominal hourly wage ~ X 
 
  # Frisch-Waugh-Lovell (FWL) regression using the previously obtained residuals
  
  model7FWLbs <- lm(loghwageResidXbs~femaleResidXbs, db_sample)
  
  model7bsmen <- lm(log_nominal_income ~ female + age +age2 + Employment_Sector + Head_Female +
                      Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel, 
                    subset = (female == 0 ),
                    data=db_sample)
  
  model7bsfemales <- lm(log_nominal_income ~ female + age +age2 + 
                          Employment_Sector + Head_Female +
                          Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel, 
                        subset = (female == 1 ),
                        data=db_sample)
  
  # Extracts the coefficient of interest 
  beta1RES_fwlbs <-model7FWLbs$coefficients[2] 
  
  beta1_fwlbsmen <- model7bsmen$coefficients[3]
  beta2_fwlbsmen <- model7bsmen$coefficients[4]
  
  beta1_fwlbsfemales <- model7bsfemales$coefficients[3]
  beta2_fwlbsfemales <- model7bsfemales$coefficients[4]
  
  age_maxbbsmen <- -beta1_fwlbsmen / (2 * beta2_fwlbsmen)
  age_maxbbsfemales <- -beta1_fwlbsfemales / (2 * beta2_fwlbsfemales)
  
  estimates_FWLmodel7[i] <- beta1RES_fwlbs
  
  agesmax_FWLmodel7men[i]<- age_maxbbsmen #saves it in the above vector
  agesmax_FWLmodel7females[i]<- age_maxbbsfemales
}

length(estimates_FWLmodel7)

length(agesmax_FWLmodel7men)
length(agesmax_FWLmodel7females)

# Bootstrapping Results 

plot(hist(estimates_FWLmodel7))

plot(hist(agesmax_FWLmodel7men))
plot(hist(agesmax_FWLmodel7females))

meancoefFWLbs <- mean(estimates_FWLmodel7)

meanpeakagebsmen <- mean(agesmax_FWLmodel7men)
meanpeakagebsfemales <- mean(agesmax_FWLmodel7females)

# Computes the standard error for each bootstrap estimate
secoefFWLbs <- sqrt(var(estimates_FWLmodel7))

sepeakagebsmen <- sqrt(var(agesmax_FWLmodel7men))
sepeakagebsfemales <- sqrt(var(agesmax_FWLmodel7females))

#Computes the 95% confidence intervals using the 2.5% and 97.5% percentiles
ci_lowercoefFWLbs <- quantile(estimates_FWLmodel7, 0.025)
ci_uppercoefFWLbs <- quantile(estimates_FWLmodel7, 0.975)

ci_lowerbsmen <- quantile(agesmax_FWLmodel7men, 0.025)
ci_upperbsmen <- quantile(agesmax_FWLmodel7men, 0.975)

ci_lowerbsfemales <- quantile(agesmax_FWLmodel7females, 0.025)
ci_upperbsfemales <- quantile(agesmax_FWLmodel7females, 0.975)

# Creating the male estimates table
summary_table1bsmen <- data.frame(
  Statistic = c("Bootstrap Mean", "Standard Error", 
                "Lower 95% CI", "Upper 95% CI"),
  Male_Value = c(meanpeakagebsmen, sepeakagebsmen, 
                 ci_lowerbsmen, ci_upperbsmen)
)

# Creating the female estimates table
summary_table1bsfemales <- data.frame(
  Statistic = c("Bootstrap Mean", "Standard Error", 
                "Lower 95% CI", "Upper 95% CI"),
  Female_Value = c(meanpeakagebsfemales, sepeakagebsfemales, 
                   ci_lowerbsfemales, ci_upperbsfemales)
)

# Merging the tables on the Statistic column
merged_table2 <- merge(summary_table1bsmen,
                       summary_table1bsfemales, by = "Statistic")

# Printing table - Peak age by gender
stargazer(merged_table2, summary = FALSE, type = "text",
          title = "Bootstrap Peak Age Estimates by Sex", digits = 2
          )

#Creating FWL Bootstrap 'female' coefficient estimation table:

summary_tableBETA1s <- data.frame(
  Statistic = c("Bootstrap + FWL Mean", "Standard Error", 
                "Lower 95% CI", "Upper 95% CI", 'FWL-only value', 'OLS value'),
  Beta1Coef_Value = c(meancoefFWLbs, secoefFWLbs, 
                      ci_lowercoefFWLbs, ci_uppercoefFWLbs, 
                      model7FWL$coefficients[2], model7$coefficients[2])
)

# Printing table - Bootstrap 'female' coefficient estimation table:
stargazer(summary_tableBETA1s, summary = FALSE, type = "text",
          title = "Bootstrap + FWL Coefficient Stats vs FWL and OLS Coefficients", 
          digits = 4)

# Printing table - Regression results for wage gap models (HTML)
stargazer(model6, model7, model7FWL, 
          type = "html",  
          covariate.labels = c("Female", "Female FWL"),  
          dep.var.labels = c("Log Nominal Hourly Wage", "Residualized Log Nominal Hourly Wage"),
          title = "Table 3. Regression Results: Wage Gap Models",
          column.labels = c("Model 6", "Model 7", "Model 7 FWL"),  
          omit = omit_vars,  # Hide control variables
          notes = "Model 7 includes additional controls: Age, Employment Sector, Female Household Head, Weekly Hours Worked, Formality, Firm Size, and Education. FWL refers to the Frisch-Waugh-Lovell decomposition. Bootstrap models account for resampling variability.",
          out = "../views/regression_results41.htm")  # Save in 'views' folder


# Printing table - Regression results for wage gap models (LATEX)
stargazer(model6, model7, model7FWL, 
          type = "latex",  # Change to LaTeX format
          covariate.labels = c("Female", "Female FWL"),  
          dep.var.labels = c("Log Nominal Hourly Wage", "Residualized Log Nominal Hourly Wage"),
          title = "Table 3. Regression Results: Wage Gap Models",
          column.labels = c("Model 6", "Model 7", "Model 7 FWL"),  
          omit = omit_vars,  # Hide control variables
          notes = "Model 7 includes additional controls: Age, Employment Sector, Female Household Head, Weekly Hours Worked, Formality, Firm Size, and Education. FWL refers to the Frisch-Waugh-Lovell decomposition. Bootstrap models account for resampling variability.",
          out = "../views/regression_results41.tex")  # Save in 'views' folder


