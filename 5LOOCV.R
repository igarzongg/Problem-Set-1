# PREDICTING EARNINGS ----------------------------------------------------------

set.seed(10101)

#a). 

inTrain <- createDataPartition(
  y = db$log_nominal_income,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db %>% 
  filter(row_number() %in% inTrain)

testing  <- db %>% 
  filter(!row_number() %in% inTrain)

# Create data for visualization
split_data <- data.frame(
  Split = factor(c("Training", "Testing")),
  Count = c(nrow(training), nrow(testing)),
  Percentage = c(nrow(training)/nrow(db)*100, nrow(testing)/nrow(db)*100)
)

# Create the visualization
ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Train-Test Split Distribution",
       y = "Number of Observations",
       x = "") +
  theme_bw() +
  ylim(0, max(split_data$Count) * 1.2)  # Add some space for labels

# b) 

# PREDICTIVE PERFORMANCE IN TERMS OF THE RMSE ----------------------------------
# --------------- model 1 & model 2 results for Nominal Hourly Wage ------------
# MODEL 1 ----------------------------------------------------------------------

reg_1<- log_nominal_income ~ age + age2

model1 <- lm(reg_1,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model1 <- predict(model1, testing)


score1<- RMSE(prediction_model1, testing$log_nominal_income )
score1
# 0.3489662

# MODEL 2 ----------------------------------------------------------------------

reg_2<- log_nominal_income ~ age

model2 <- lm(reg_2,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model2 <- predict(model2, testing)


score2<- RMSE(prediction_model2, testing$log_nominal_income )
score2
# 0.3510614

# ---------- model 3 & model 4 results for real Hourly Wage -----------------
# MODEL 3 ----------------------------------------------------------------------

reg_3<- log_real_income  ~ age + age2

model3 <- lm(reg_3,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model3 <- predict(model3, testing)


score3<- RMSE(prediction_model3, testing$log_real_income )
score3
# 0.3241643

# MODEL 4 ----------------------------------------------------------------------

reg_4<- log_real_income  ~ age 

model4 <- lm(reg_4,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model4 <- predict(model4, testing)


score4<- RMSE(prediction_model4, testing$log_real_income )
score4
# 0.3254666

# MODEL 5 ----------------------------------------------------------------------

reg_5<- log_nominal_income  ~ female

model5 <- lm(reg_5,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model5 <- predict(model5, testing)


score5<- RMSE(prediction_model5, testing$log_nominal_income )
score5
# 0.3524142

# MODEL 6 ----------------------------------------------------------------------

reg_6 <- log_real_income  ~ female

model6 <- lm(reg_6,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model6 <- predict(model6, testing)


score6<- RMSE(prediction_model6, testing$log_real_income )
score6
# 0.3263867 

# MODEL 7 ----------------------------------------------------------------------

reg_7 <- log_nominal_income ~ female + age +age2 + Employment_Sector + Head_Female +
  Weekly_Hours_Worked + formal + sizeFirm + maxEducLevel

model7 <- lm(reg_7,
             data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
prediction_model7 <- predict(model7, testing)


score7<- RMSE(prediction_model7, testing$log_nominal_income )
score7
# 0.2709422

# MODELOS CON MAYOR COMPLEJIDAD/NO LINEALES ------------------------------------

# MODELO 8 - polynomial regression (age^3) -------------------------------------

reg_8 <- log_nominal_income ~ age + poly(age, 3) + female

model8 <- lm(reg_8, 
             data = training)

# PERFORMANCE RMSE 

prediction_model8 <- predict(model8, testing)

score8 <- RMSE(prediction_model8, testing$log_nominal_income)
score8
# 0.3476926

# MODELO 9 - polynomial regression on age, education and hours worked-----------
# non-linearity in education and hours worked (posibles rendimientos marginales)

reg_9 <- log_nominal_income ~ poly(age, 3) + poly(maxEducLevel, 2) +
  
  poly(Weekly_Hours_Worked, 2) + female + Employment_Sector + formal + sizeFirm

model9 <- lm(reg_9, data = training)

# PERFORMANCE RMSE 

predictions_model9 <- predict(model9, testing)

score9 <- RMSE(predictions_model9, testing$log_nominal_income)
score9

# 0.2672922

# MODELO 10 - interaction age & gender -----------------------------------------

reg_10 <- log_nominal_income ~ age+ age * female + age2 + Employment_Sector + Weekly_Hours_Worked + 
  maxEducLevel
model10 <- lm(reg_10, 
              data = training)

# PERFORMANCE RMSE 

prediction_model10 <- predict(model10, testing)

score10 <- RMSE(prediction_model10, testing$log_nominal_income)
score10
# 0.312069

# MODELO 11 - interaction formal work & gender ---------------------------------

reg_11 <- log_nominal_income ~
  female+
  formal*female + age + age2 + Employment_Sector + Weekly_Hours_Worked + maxEducLevel + formal
model11 <- lm(reg_11, 
              data = training)

# PERFORMANCE RMSE 

prediction_model11 <- predict(model11, testing)

score11 <- RMSE(prediction_model11, testing$log_nominal_income)
score11
# 0.2743492

# MODELO 12 - multiple interactions --------------------------------------------

reg_12 <- log_nominal_income ~ age + age2 + female + maxEducLevel + Weekly_Hours_Worked +
  formal + Head_Female +
  maxEducLevel * female +
  formal * Weekly_Hours_Worked + age * Head_Female  + Employment_Sector * Weekly_Hours_Worked

model12 <- lm(reg_12, 
              data = training)

# PERFORMANCE RMSE 

prediction_model12 <- predict(model12, testing)

score12 <- RMSE(prediction_model12, testing$log_nominal_income)
score12

# 0.2640155

# PERFORMANCE RESULT TABLE 

# Load libraries
library(officer)
library(flextable)

# Create the data frame
results <- data.frame(
  model = c("Model 1", "Model 2", "Model 3", 
            "Model 4", "Model 5", "Model 6", 
            "Model 7", "Model 8", "Model 9", 
            "Model 10", "Model 11", "Model 12"),
  RMSE = c(score1, score2, score3, score4, score5, score6, score7, 
           score8, score9, score10, score11, score12)
)

# Order by RMSE
results <- results[order(results$RMSE), ]

# Identify the model with the lowest RMSE
lowest <- results$model[1]
lowest_RMSE <- results$RMSE[1]

# Create a flextable for formatting
results_table <- flextable(results) %>%
  theme_vanilla() %>%  # Optional: Apply a clean theme
  autofit()  # Adjust column widths automatically

# Create a Word document
doc <- read_docx()
doc <- body_add_flextable(doc, results_table)

# Add a summary of the best model
doc <- body_add_par(doc, paste("Best Model: ", lowest), style = "Normal")
doc <- body_add_par(doc, paste("Lowest RMSE: ", lowest_RMSE), style = "Normal")

# Save as Word file
print(doc, target = "results_table51.docx")

## Prediction errors in test sample --------------------------------------------

model <- c("Model 1", "Model 2", "Model 3", 
           "Model 4", "Model 5", "Model 6", 
           "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12")

best_model_name <- results$model[1]
best_model_rmse <- results$RMSE[1]

print(paste("Best Model:", best_model_name, "with RMSE:", best_model_rmse))

# Step 2: Retrieve predictions and compute residuals for the best model
if (best_model_name == "Model 1") {
  residuals_best <- testing$log_nominal_income - prediction_model1
} else if (best_model_name == "Model 2") {
  residuals_best <- testing$log_nominal_income - prediction_model2
} else if (best_model_name == "Model 3") {
  residuals_best <- testing$log_real_income - prediction_model3
} else if (best_model_name == "Model 4") {
  residuals_best <- testing$log_real_income - prediction_model4
} else if (best_model_name == "Model 5") {
  residuals_best <- testing$log_nominal_income - prediction_model5
} else if (best_model_name == "Model 6") {
  residuals_best <- testing$log_real_income - prediction_model6
} else if (best_model_name == "Model 7") {
  residuals_best <- testing$log_nominal_income - prediction_model7
} else if (best_model_name == "Model 8") {
  residuals_best <- testing$log_nominal_income - prediction_model8
} else if (best_model_name == "Model 9") {
  residuals_best <- testing$log_nominal_income - predictions_model9
} else if (best_model_name == "Model 10") {
  residuals_best <- testing$log_nominal_income - prediction_model10
} else if (best_model_name == "Model 11") {
  residuals_best <- testing$log_nominal_income - prediction_model11
} else if (best_model_name == "Model 12") {
  residuals_best <- testing$log_nominal_income - prediction_model12
}

#Analyze residuals
summary(residuals_best)

#Plot residuals distribution
ggplot(data.frame(residuals_best), aes(x = residuals_best)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  ggtitle(paste("Residuals Distribution -", best_model_name)) +
  xlab("Residuals") + ylab("Frequency")

#Identify extreme residuals (outliers)
threshold <- mean(residuals_best) + 2 * sd(residuals_best)
outliers <- residuals_best[abs(residuals_best) > threshold]

print(paste("Number of potential outliers:", length(outliers)))
print("Outliers:")
print(outliers)


## LOOCV

ctrl <- trainControl(
  method = "LOOCV")

## Model 12 ---- first lowest predictive error 

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
model1_best_loocv <- train(reg_12,
                           data = db,
                           method = 'lm', 
                           trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")
model1_best_loocv

score_best1 <-RMSE(model1_best_loocv$pred$pred, db$log_nominal_income)
score_best1

## Model 9 ---- second lowest predictive error 

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
model2_best_loocv <- train(reg_9,
                           data = db,
                           method = 'lm', 
                           trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")
model2_best_loocv

score_best2 <-RMSE(model2_best_loocv$pred$pred, db$log_nominal_income)
score_best2


#RE-ASSURING RESULTS WITH LEVERAGE

##PROCESS FOR MODEL 12

full_model12 <- lm(reg_12,
                   data = db )

X<- model.matrix(full_model12)
y <- model.response(model.frame(full_model12))

beta_hat12 <- full_model12$coefficients

## Calculate the inverse of  (X'X), call it G_inv
G_inv<- solve(t(X)%*%X)

## and 1/1-hi
vec<- 1/(1-hatvalues(full_model12))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta12<- beta_hat12  
  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model12$residuals[i]
  ## get the new error
  new_error12<- (y[i]- (X[i, ] %*% new_beta12))^2
  LOO[i]<-  new_error12
}

looCV_error12 <- mean(LOO)
score_best1_lev <-sqrt(looCV_error12) ##Yields the same result as LOOCV



##PROCESS FOR MODEL 9

full_model9 <- lm(reg_9,
                  data = db )

X<- model.matrix(full_model9)
y <- model.response(model.frame(full_model9))

beta_hat9 <- full_model9$coefficients

## Calculate the inverse of  (X'X), call it G_inv
G_inv<- solve(t(X)%*%X)

## and 1/1-hi
vec<- 1/(1-hatvalues(full_model9))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta9<- beta_hat9  
  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model9$residuals[i]
  ## get the new error
  new_error9<- (y[i]- (X[i, ] %*% new_beta9))^2
  LOO[i]<-  new_error9
}

looCV_error9 <- mean(LOO)
score_best2_lev <- sqrt(looCV_error9) ##Yields the same result as LOOCV
score_best2_lev



## COMPARISON WITH VALIDATION SET APPROACH -------------------------------------

# Install required packages

library(officer)
library(flextable)

# Create your data frame
comparison <- data.frame(
  model = c("Model 12: Validation set approach", "Model 12: LOOCV",
            "Model 12: Leverage", "Model 9: Validation set approach", 
            "Model 9: LOOCV", "Model 9: Leverage"),
  RMSE = c(score12, score_best1, score_best1_lev,
           score9, score_best2, score_best2_lev)
)

# Convert to flextable
comparison_table <- flextable(comparison)
print(comparison_table)

# Create a Word document and add the table
doc <- read_docx()
doc <- body_add_flextable(doc, comparison_table)

# Save to Word
print(doc, target = "regression_table52.docx")

