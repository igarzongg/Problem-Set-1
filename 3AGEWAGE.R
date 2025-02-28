# AGE-WAGE PROFILFE ------------------------------------------------------------

db <- db %>% mutate(age2= age^2)

model2 <- lm(log_nominal_income  ~ age,  data= db)

model3 <- lm(log_nominal_income  ~ age + age2, data= db)

model4 <- lm(log_real_income  ~ age,  data= db)

model5 <- lm(log_real_income  ~ age + age2, data= db)

stargazer(model2, model3, type="text", covariate.labels=c("Age","Agesq"))
#Model results for Nominal Hourly Wage

stargazer(model4, model5, type="text", covariate.labels=c("Age","Agesq"))
#Model results for Real Hourly Wage

residualsmodel2 <- residuals(model2)
residualsmodel3 <- residuals(model3)

residualsmodel4 <- residuals(model4)
residualsmodel5 <- residuals(model5)

ggplot(data= db, 
       mapping = aes(x=residualsmodel2)) +
  theme_bw() + 
  geom_density() 

ggplot(data= db, 
       mapping = aes(x=residualsmodel3)) +
  theme_bw() + 
  geom_density() 

ggplot(data= db, 
       mapping = aes(x=residualsmodel4)) +
  theme_bw() + 
  geom_density() 

ggplot(data= db, 
       mapping = aes(x=residualsmodel5)) +
  theme_bw() + 
  geom_density() 

ggplot(db , aes(y = residualsmodel2 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = residualsmodel3 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = residualsmodel4 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = residualsmodel5 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels


db<-db %>% mutate(m2_std_residuals= studres(model2) )
db<-db %>% mutate(m3_std_residuals= studres(model3) )

db<-db %>% mutate(m2r_std_residuals= studres(model4) )
db<-db %>% mutate(m3r_std_residuals= studres(model5) )


ggplot(db , aes(y = m2_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = m3_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = m2r_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = m3r_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db <- db %>% 
  filter(m2_std_residuals < 2 & m2_std_residuals > -2 & 
           m3_std_residuals < 2 & m3_std_residuals > -2 & m2r_std_residuals 
         < 2 & m2r_std_residuals > -2 & 
           m3r_std_residuals < 2 & m3r_std_residuals > -2)

## Estimation of age-wage reg -------------------------------------------------

#Now that our db has studentized outliers

model2 <- lm(log_nominal_income  ~ age,  data= db)

model3 <- lm(log_nominal_income  ~ age + age2, data= db)

model4 <- lm(log_real_income  ~ age,  data= db)

model5 <- lm(log_real_income  ~ age + age2, data= db)

stargazer(model2, model3, model4, model5, 
          title = "Regression Results: Nominal and Real Income",
          align = TRUE,
          covariate.labels = c("Age", "Age Squared"),
          dep.var.labels = c("Log Nominal Hourly Wage", "Log Real Hourly Wage"),
          column.labels = c("Model 2", "Model 3", "Model 4", "Model 5"),
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          notes = "This table presents the estimation results for nominal and real income models, considering age and its quadratic term. Standard errors are in parentheses.",
          notes.align = "l",
          type = "text",
          out = "regtable31.htm")


## Estimation procedure for NOMINAL Hourly Wage --------------------------------

model3 <- lm(log_nominal_income  ~ age + age2, data= db)

# Extract coefficients
beta1 <- coef(model3)["age"]
beta2 <- coef(model3)["age2"]

# Compute the age at which income is maximized
age_max <- -beta1 / (2 * beta2)

age_max #Income is maximized at this age 

model2 <- lm(log_nominal_income  ~ age, data= db)


db <- db  %>% mutate(yhat2=predict(model2), yhat3=predict(model3)) 

summ <- db %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_y = mean(log_nominal_income),
    yhat_reg2 = mean(yhat2),
    yhat_reg3 = mean(yhat3), .groups="drop"
  ) 

head(summ)

# Compute standard errors for confidence intervals
summ <- summ %>%
  mutate(
    se_yhat2 = sd(yhat_reg2, na.rm = TRUE) / sqrt(n()),
    lower_yhat2 = yhat_reg2 - 1.96 * se_yhat2,
    upper_yhat2 = yhat_reg2 + 1.96 * se_yhat2,
    
    se_yhat3 = sd(yhat_reg3, na.rm = TRUE) / sqrt(n()),
    lower_yhat3 = yhat_reg3 - 1.96 * se_yhat3,
    upper_yhat3 = yhat_reg3 + 1.96 * se_yhat3
  )

# Graphing with confidence intervals and proper formatting
ggplot() + 
  # Scatter plot of observed mean hourly wages
  geom_point(
    data = summ, 
    aes(x = age, y = mean_y),
    color = "blue", size = 2, alpha = 0.5
  ) + 
  
  # Confidence interval for Model 2
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat2, ymax = upper_yhat2, fill = "Model 2"),
    alpha = 0.2
  ) +
  
  # Line for Model 2
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg2, color = "Model 2"),
    linewidth = 1.5
  ) + 
  
  # Confidence interval for Model 3
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat3, ymax = upper_yhat3, fill = "Model 3"),
    alpha = 0.2
  ) +
  
  # Line for Model 3
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg3, color = "Model 3"),
    linewidth = 1.5
  ) +
  
  # Labels and title
  labs(
    title = "Predicted Age-Wage Profile with Confidence Intervals",
    subtitle = "Based on two different regression models",
    x = "Age",
    y = "Log Nominal Hourly Wages",
    color = "Model",
    fill = "Conf. Interval",
    caption = "Note: The dots represent observed mean wages,
    while the lines indicate predicted wages from two models.\nShaded areas 
    represent 95% confidence intervals."
  ) +
  
  # Custom theme
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


## Finding the 'peak-age' with CI's according by bootstrapping the the model---


set.seed(101110)

B <- 1000

estimates_model3<-rep(NA,B)

for(i in 1:B){
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
  #takes a sample with replacement of the same size of the original sample 
  
  model3 <- lm(log_nominal_income  ~ age + age2, db_sample)
  
  beta1<-model3$coefficients[2] # gets the coefficient of interest 
  beta2 <- model3$coefficients[3]
  
  age_maxb <- -beta1 / (2 * beta2)
  
  estimates_model3[i]<- age_maxb #saves it in the above vector
}



length(estimates_model3)

plot(hist(estimates_model3))

meanpeakage <- mean(estimates_model3)

sepeakage <- sqrt(var(estimates_model3))

ci_lower <- quantile(estimates_model3, 0.025)
ci_upper <- quantile(estimates_model3, 0.975)

## Estimation procedure for REAL Hourly Wage -----------------------------------

model5 <- lm(log_real_income  ~ age + age2, data= db)

# Extract coefficients
beta1r <- coef(model5)["age"]
beta2r <- coef(model5)["age2"]

# Compute the age at which income is maximized
age_maxr <- -beta1r / (2 * beta2r)

age_maxr #Income is maximized at this age 

model4 <- lm(log_real_income  ~ age, data= db)

stargazer(model5, model4, type="text",
          covariate.labels=c("Age","Squared Age"), 
          dep.var.labels = "Log Real Hourly Wage", 
          title = 'Quadratic vs Linear model')

db <- db  %>% mutate(yhat1r=predict(model5), yhat2r=predict(model4)) 

summ <- db %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_yr = mean(log_real_income),
    yhat_reg1r = mean(yhat1r),
    yhat_reg2r = mean(yhat2r), .groups="drop"
  ) 

head(summ)

## Graph displaying relationship between variables (REAL hourly wage)

# Compute standard errors for confidence intervals
summ <- summ %>%
  mutate(
    se_yhat1r = sd(yhat_reg1r, na.rm = TRUE) / sqrt(n()),
    lower_yhat1r = yhat_reg1r - 1.96 * se_yhat1r,
    upper_yhat1r = yhat_reg1r + 1.96 * se_yhat1r,
    
    se_yhat2r = sd(yhat_reg2r, na.rm = TRUE) / sqrt(n()),
    lower_yhat2r = yhat_reg2r - 1.96 * se_yhat2r,
    upper_yhat2r = yhat_reg2r + 1.96 * se_yhat2r
  )

# Graphing with confidence intervals and proper formatting
ggplot() + 
  # Scatter plot of observed real hourly wages
  geom_point(
    data = summ, 
    aes(x = age, y = mean_yr),
    color = "blue", size = 2, alpha = 0.5
  ) + 
  
  # Confidence interval for Model 1
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat1r, ymax = upper_yhat1r, fill = "Model 5"),
    alpha = 0.2
  ) +
  
  # Line for Model 1
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg1r, color = "Model 5"),
    linewidth = 1.5
  ) + 
  
  # Confidence interval for Model 2
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat2r, ymax = upper_yhat2r, fill = "Model 4"),
    alpha = 0.2
  ) +
  
  # Line for Model 2
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg2r, color = "Model 4"),
    linewidth = 1.5
  ) +
  
  # Labels and title
  labs(
    title = "Predicted Age-Wage Profile (REAL Wages) with Confidence Intervals",
    subtitle = "Based on two different regression models",
    x = "Age",
    y = "Log Real Hourly Wages",
    color = "Model",
    fill = "Confidence Interval",
    caption = "Note: The dots represent observed mean real wages, 
    while the lines indicate predicted wages from two models.\nShaded 
    areas represent 95% confidence intervals."
  ) +
  
  # Theme
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


## Finding the 'peak-age' with CI's according by bootstrapping the the model----

set.seed(101111)

B <- 1000

estimates_model5<-rep(NA,B)

for(i in 1:B){
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
  #takes a sample with replacement of the same size of the original sample.
  
  model5 <- lm(log_real_income  ~ age + age2, db_sample)
  
  beta1r<-model5$coefficients[2] # gets the coefficient of interest 
  beta2r <- model5$coefficients[3]
  
  age_maxbr <- -beta1r / (2 * beta2r)
  
  estimates_model5[i]<- age_maxbr #saves it in the above vector
}

length(estimates_model5)

plot(hist(estimates_model5))

meanpeakager <- mean(estimates_model5)

sepeakager <- sqrt(var(estimates_model5))


ci_lowerr <- quantile(estimates_model5, 0.025)
ci_upperr <- quantile(estimates_model5, 0.975)


## Creating the summary table(s) with the relevant estimate statistics

summary_table <- data.frame(
  Statistic = c("Bootstrap Mean", "Standard Error", "Lower 95% CI",
                "Upper 95% CI", "Original OLS Peak Age"),
  Value = c(meanpeakage, sepeakage, ci_lower, ci_upper, age_max)
)

summary_table2 <- data.frame(
  Statistic = c("Bootstrap Mean", "Standard Error", "Lower 95% CI",
                "Upper 95% CI", "Original OLS Peak Age"),
  Value = c(meanpeakager, sepeakager, ci_lowerr, ci_upperr, age_max)
)

# Create merged summary table
merged_table <- data.frame(
  Statistic = summary_table$Statistic,  # Keeping Statistic names
  Nominal_Value = summary_table$Value,  # Values from first table
  Real_Value = summary_table2$Value     # Values from second table
)

# Print merged table 
stargazer(merged_table, summary = FALSE, type = "text",
     title = "Comparison of Nominal and Real Bootstrap Estimates of Peak Age", 
          digits = 2)

