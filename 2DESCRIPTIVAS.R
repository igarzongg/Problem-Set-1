# DESCRIPTIVE VARIABLES --------------------------------------------------------

# CATEGORICAL VARIABLES

freq_table_female <- table(db$female)
prop_table_female <- prop.table(table(db$female))
summary_table_female <- data.frame(
  Sex = c("0 (Male)", "1 (Female)"),  
  Count = as.numeric(freq_table_female),     
  Proportion = round(as.numeric(prop_table_female), 4) 
)

db <- db %>% #Household head variable created.
  mutate(H_Head = ifelse( p6050== 1, 1, 0))
freq_table_HHead <- table(db$H_Head)
prop_table_HHead <- prop.table(table(db$H_Head))
summary_table_HHead <- data.frame(
  HouseholdStatus = c("0 (N/HH)", "1 (Household Head)"),  
  Count = as.numeric(freq_table_HHead),     
  Proportion = round(as.numeric(prop_table_HHead), 4) 
)

db <- db %>% #Female HH variable created.
  mutate(Head_Female = H_Head*(female))
freq_table_HHeadF <- table(db$Head_Female)
prop_table_HHeadF <- prop.table(table(db$Head_Female))
summary_table_HHeadF <- data.frame(
  HHeadSex = c("0 (N/FHH)", "1 (Female Household Head)"),  
  Count = as.numeric(freq_table_HHeadF),     
  Proportion = round(as.numeric(prop_table_HHeadF), 4) 
)

db$maxEducLevel[is.na(db$maxEducLevel)] <- 1 
#Assigns no education if there are missing values in our Max Education Variable.

freq_table_educ <- table(db$maxEducLevel)
prop_table_educ <- prop.table(table(db$maxEducLevel))
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

freq_table_form <- table(db$formal)
prop_table_form <- prop.table(table(db$formal))
summary_table_form <- data.frame(
  Employment_type = c("0 (Informal)", "1 (Formal / Social Security)"),
  Count = as.numeric(freq_table_form),
  Proportion = round(as.numeric(prop_table_form), 4)  
)

freq_table_sfirm <- table(db$sizeFirm)
prop_table_sfirm <- prop.table(table(db$sizeFirm))
summary_table_sfirm <- data.frame(
  Firm_size = c("self-employed ", "2-5 workers ", "6-10 workers ",
                "11-50 workers", 
                ">50 workers"),
  Count = as.numeric(freq_table_sfirm),
  Proportion = round(as.numeric(prop_table_sfirm), 4)  
)

unique(db$Employment_Sector)
summary(db$Employment_Sector)

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

freq_table_Employment_Sector <- table(db$Employment_Sector_factor)
prop_table_Employment_Sector <- prop.table(freq_table_Employment_Sector)

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

# Rename dynamically
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

# Bind all tables into one
summary_table_categorical <- bind_rows(
  summary_table_female,
  summary_table_HHeadF,
  summary_table_educ,
  summary_table_form,
  summary_table_sfirm,
  summary_table_Employment_Sector
)

# Reorder columns
summary_table_categorical <- summary_table_categorical %>% 
  dplyr::select(Variable, Category, Count, Proportion)

# Print formatted table
kable(summary_table_categorical, 
      caption = "Summary Statistics of Categorical Variables")


#CONTINUOUS VARIABLES

db <- as.data.frame(db)

db <- db %>%
  mutate(exp_log_nominal_income =  exp(log_nominal_income))
db <- db %>%
  mutate(exp_log_real_income =  exp(log_real_income))


# Select only numeric columns for summary statistics
vars <- db[, c("log_nominal_income","log_real_income",
               "exp_log_nominal_income", "exp_log_real_income", 
               "age", "Weekly_Hours_Worked")]

# Generate the summary statistics table
stargazer(vars, 
          type = "text",       # Can be "text", "html", or "latex"
          summary.stat = c("n", "mean", "sd", "min", "max"),
          title = "Summary Statistics - Continuous Variables",
          digits = 2, out='summarystatscont22.htm')

