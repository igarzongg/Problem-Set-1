# Problem Set 1
##########################################################
# Descriptive Variables 
# Authors: Juan Pablo Grimaldos & Isabella Garzón 
##########################################################

# -------------------------- DESCRIPTIVE VARIABLES -----------------------------

# CATEGORICAL VARIABLES

# Create frequency and proportion tables for gender 

freq_table_female <- table(db$female) # counting occurrences of each gender category
prop_table_female <- prop.table(table(db$female)) # compute proportions of each category
summary_table_female <- data.frame(
  Sex = c("0 (Male)", "1 (Female)"),  
  Count = as.numeric(freq_table_female),     
  Proportion = round(as.numeric(prop_table_female), 4) 
)

# Creating Household Head indicator variable
# values: 1 if the person is the household head, 0 otherwise

db <- db %>% 
  mutate(H_Head = ifelse( p6050== 1, 1, 0))
freq_table_HHead <- table(db$H_Head) # counting occurrences of each gender category
prop_table_HHead <- prop.table(table(db$H_Head)) # compute proportions of each category
summary_table_HHead <- data.frame(
  HouseholdStatus = c("0 (N/HH)", "1 (Household Head)"),  
  Count = as.numeric(freq_table_HHead),     
  Proportion = round(as.numeric(prop_table_HHead), 4) 
)

# Creating Female Household Head indicator variable
# values: 1 if the person is the household head, 0 otherwise

db <- db %>% #Female HH variable created.
  mutate(Head_Female = H_Head*(female))
freq_table_HHeadF <- table(db$Head_Female) # counting occurrences of each gender category
prop_table_HHeadF <- prop.table(table(db$Head_Female)) # compute proportions of each category
summary_table_HHeadF <- data.frame(
  HHeadSex = c("0 (N/FHH)", "1 (Female Household Head)"),  
  Count = as.numeric(freq_table_HHeadF),     
  Proportion = round(as.numeric(prop_table_HHeadF), 4) 
)

# Assign no education if there are missing values in our Max Education Variable

db$maxEducLevel[is.na(db$maxEducLevel)] <- 1 


# Create frequency and proportion tables for education levels 

freq_table_educ <- table(db$maxEducLevel) # counting occurrences of each gender category
prop_table_educ <- prop.table(table(db$maxEducLevel)) # compute proportions of each category
summary_table_educ <- data.frame(
  Academic_achievement = c("1 (None)", 
                           "3 (Primary incomplete)",
                           "4 (Primary complete)",
                           "5 (Secondary incomplete)",
                           "6 (Secondary complete)",
                           "7 (Terciary)"),  
  Count = as.numeric(freq_table_educ),     
  Proportion = round(as.numeric(prop_table_educ), 4)  
)

# Create frequency and proportion tables for employment type

freq_table_form <- table(db$formal) # counting occurrences of each gender category
prop_table_form <- prop.table(table(db$formal)) # compute proportions of each category
summary_table_form <- data.frame(
  Employment_type = c("0 (Informal)", "1 (Formal / Social Security)"),
  Count = as.numeric(freq_table_form),
  Proportion = round(as.numeric(prop_table_form), 4)  
)

# Create frequency and proportion tables for firm size 

freq_table_sfirm <- table(db$sizeFirm) # counting occurrences of each gender category
prop_table_sfirm <- prop.table(table(db$sizeFirm)) # compute proportions of each category
summary_table_sfirm <- data.frame(
  Firm_size = c("self-employed ", "2-5 workers ", "6-10 workers ",
                "11-50 workers", 
                ">50 workers"),
  Count = as.numeric(freq_table_sfirm),
  Proportion = round(as.numeric(prop_table_sfirm), 4)  
)

unique(db$Employment_Sector)
summary(db$Employment_Sector)

# Tranform employment sector variable to a factor with 
# meaningful labels 

db$Employment_Sector_factor <- factor(
  db$Employment_Sector, 
  levels = 1:9, 
  labels = c(
    "1 (Private sector worker)", 
    "2 (Government worker)", 
    "3 (Domestic worker)", 
    "4 (Self-employed)", 
    "5 (Employer)", 
    "6 (Unpaid family worker)", 
    "7 (Unpaid worker in others' businesses)", 
    "8 (Day laborer)", 
    "9 (Other)"
  )
)

# Creating frequency and proportion tables for emplyment sector 

freq_table_Employment_Sector <- table(db$Employment_Sector_factor) # counting occurrences of each gender category
prop_table_Employment_Sector <- prop.table(freq_table_Employment_Sector) # compute proportions of each category

summary_table_Employment_Sector <- data.frame(
  Occupation = names(freq_table_Employment_Sector),
  Count = as.numeric(freq_table_Employment_Sector),
  Proportion = round(as.numeric(prop_table_Employment_Sector), 4) 
)

library(knitr)

# Add a 'Variable' column to identify each categorical variable

summary_table_female <- summary_table_female %>% 
  mutate(Variable = "Sex")
summary_table_HHeadF <- summary_table_HHeadF %>% 
  mutate(Variable = "Female Household Head?")
summary_table_educ <- summary_table_educ %>% 
  mutate(Variable = "Max. Education Level")
summary_table_form <- summary_table_form %>%
  mutate(Variable = "Formal worker?")
summary_table_sfirm <- summary_table_sfirm %>% 
  mutate(Variable = "Size of Firm")
summary_table_Employment_Sector <- summary_table_Employment_Sector %>% 
  mutate(Variable = "Employment Sector")

# Check column names before renaming
print(colnames(summary_table_female))

# Renaming category columns dynamically 
summary_table_female <- summary_table_female %>% 
  rename(Category = everything()[1])
summary_table_HHeadF <- summary_table_HHeadF %>% 
  rename(Category = everything()[1])
summary_table_educ <- summary_table_educ %>% 
  rename(Category = everything()[1])
summary_table_form <- summary_table_form %>% 
  rename(Category = everything()[1])
summary_table_sfirm <- summary_table_sfirm %>% 
  rename(Category = everything()[1])
summary_table_Employment_Sector <- summary_table_Employment_Sector %>% 
  rename(Category = everything()[1])

# Combining all tables into one
summary_table_categorical <- bind_rows(
  summary_table_female,
  summary_table_HHeadF,
  summary_table_educ,
  summary_table_form,
  summary_table_sfirm,
  summary_table_Employment_Sector
)

# Reordering columns
summary_table_categorical <- summary_table_categorical %>% 
  dplyr::select(Variable, Category, Count, Proportion)

# Print formatted table
kable(summary_table_categorical, 
      caption = "Summary Statistics of Categorical Variables")


#CONTINUOUS VARIABLES

# Convert to data frame
db <- as.data.frame(db)

# Creating exp income variables
db <- db %>%
  mutate(exp_log_nominal_income =  exp(log_nominal_income)) # Reverse log transformation 
db <- db %>%
  mutate(exp_log_real_income =  exp(log_real_income)) # Reverse log transformation


# Selecting only numeric variables for summary statistics

vars <- db[, c("log_nominal_income","log_real_income",
               "exp_log_nominal_income", "exp_log_real_income", 
               "age", "Weekly_Hours_Worked")]

# Rename variables in dataset
vars_renamed <- vars
names(vars_renamed) <- var_labels[names(vars_renamed)]

#Creates views folder

if (!dir.exists("../views")) dir.create("../views")

# Generate summary statistics table (HTML)

library(stargazer)

# Define variable names for display
var_labels <- c(
  "log_nominal_income" = "Log. Nominal hourly wage",
  "log_real_income" = "Log. Real hourly wage",
  "exp_log_nominal_income" = "Nominal hourly wage",
  "exp_log_real_income" = "Real hourly wage",
  "age" = "Age",
  "Weekly_Hours_Worked" = "Weekly work hours"
)

# Generate summary statistics table in HTML format
stargazer(vars_renamed, 
          type = "html", 
          summary.stat = c("n", "mean", "sd", "min", "max"), 
          title = "Table 1. Descriptive statistics – continuous variables",
          digits = 2,
          notes = "Note: This table presents the descriptivte statistics for the continuous variables used in this project.",
          notes.align = "l",
          out = "../views/summarystatscont22.htm")  # Save in 'views' folder


# Generate summary statistics table (LATEX)

stargazer(vars, 
          type = "latex",       # Export as LaTeX
          summary.stat = c("n", "mean", "sd", "min", "max"),
          title = "Summary Statistics - Continuous Variables",
          digits = 2, 
          notes = "Note: This table presents the descriptivte statistics for the continuous variables used in this project.",
          notes.align = "l",
          out = "../views/summarystatscont22.tex")  # Save in 'views' folder


