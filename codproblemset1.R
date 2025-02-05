
# PROBLEM SET 1

# Poner resto de nombres y codigos
#Juan Pablo Grimaldos Olivella - 202122627

library("pacman")
p_load(rio,
       tidyverse, 
       skimr,
       gridExtra,
       visdat, 
       corrplot, 
       stargazer,
       visdat,
       dplyr,
       rvest,
       caret,
       boot,
       MASS,
       lintr)

lintr::lint_dir()

# IMPORTING DATABASES ----------------------------------------------------------

#OJO - esta es temporal

db <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
db <- as_tibble(db) 

# CLEANING DATA ----------------------------------------------------------------

skim(db) %>% head()

summary(db$age)

freq_table_ocu <- table(db$ocu)
prop_table_ocu <- prop.table(table(db$ocu))
summary_table_ocu <- data.frame(
  Status = c("0 (N/Employed)", "1 (Employed)"), #Not employed contains all unemployed, economically inactive population, and population that is not working age.
  Count = as.numeric(freq_table_ocu),     
  Proportion = round(as.numeric(prop_table_ocu), 4)  
)
print(summary_table_ocu, row.names = FALSE)

db <- db %>%
  filter(age > 18, ocu == 1)

# DATA TRANSFORMATION  ---------------------------------------------------------

#MISSING VALUES

db_miss <- skim(db) %>% dplyr::select( skim_variable, n_missing)
Nobs <- nrow(db) 
Nobs
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)
db_miss <- db_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))
### Keep only variables with missing 
db_miss<- db_miss %>% filter(n_missing!= 0)
head(db_miss, 10)
tail(db_miss, 10)

# DESCRIPTIVE VARIABLES --------------------------------------------------------

# CATEGORICAL VARIABLES

freq_table_sex <- table(db$sex)
prop_table_sex <- prop.table(table(db$sex))
summary_table_sex <- data.frame(
  Sex = c("0 (Female)", "1 (Male)"),  
  Count = as.numeric(freq_table_sex),     
  Proportion = round(as.numeric(prop_table_sex), 4) 
)
print(summary_table_sex, row.names = FALSE)

freq_table_coll <- table(db$college)
prop_table_coll <- prop.table(table(db$college))
summary_table_coll <- data.frame(
  Academic_achievement = c("0 (Below Tertiary Education)", "1 (Tertiary Education or more)"),  
  Count = as.numeric(freq_table_coll),     
  Proportion = round(as.numeric(prop_table_coll), 4)  
)
print(summary_table_coll, row.names = FALSE)

freq_table_form <- table(db$formal)
prop_table_form <- prop.table(table(db$formal))
summary_table_form <- data.frame(
  Employment_type = c("0 (Informal)", "1 (Formal / Social Security)"),
  Count = as.numeric(freq_table_form),
  Proportion = round(as.numeric(prop_table_form), 4)  
)
print(summary_table_form, row.names = FALSE)

freq_table_sfirm <- table(db$sizeFirm)
prop_table_sfirm <- prop.table(table(db$sizeFirm))
summary_table_sfirm <- data.frame(
  Firm_size = c("self-employed ", "2-5 workers ", "6-10 workers ", "11-50 workers", 
                ">50 workers"),
  Count = as.numeric(freq_table_sfirm),
  Proportion = round(as.numeric(prop_table_sfirm), 4)  
)
print(summary_table_sfirm, row.names = FALSE)

unique(db$relab)
summary(db$relab)

db$relab_factor <- factor(
  db$relab, 
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

freq_table_relab <- table(db$relab_factor)
prop_table_relab <- prop.table(freq_table_relab)

summary_table_relab <- data.frame(
  Occupation = names(freq_table_relab),
  Count = as.numeric(freq_table_relab),
  Proportion = round(as.numeric(prop_table_relab), 4)
)

print(summary_table_relab, row.names = FALSE)

#CONTINUOUS VARIABLES

summary_table <- data.frame(
  Statistic = c("N", "Mean", "St. Dev.", "Min", "Max"),
  y_ingLab_m_ha = c(sum(!is.na(db$y_ingLab_m_ha)),
                    mean(db$y_ingLab_m_ha, na.rm = TRUE),
                    sd(db$y_ingLab_m_ha, na.rm = TRUE),
                    min(db$y_ingLab_m_ha, na.rm = TRUE),
                    max(db$y_ingLab_m_ha, na.rm = TRUE)),
  age = c(sum(!is.na(db$age)),
          mean(db$age, na.rm = TRUE),
          sd(db$age, na.rm = TRUE),
          min(db$age, na.rm = TRUE),
          max(db$age, na.rm = TRUE)),
  totalHoursWorked = c(sum(!is.na(db$age)),
          mean(db$age, na.rm = TRUE),
          sd(db$age, na.rm = TRUE),
          min(db$age, na.rm = TRUE),
          max(db$age, na.rm = TRUE))
)
print(summary_table)

#TABLES

#Hourly salary distribution by age grouped by employment type (Formal or Informal)

plot1 <- ggplot(db, aes(x = age, y = y_ingLab_m_ha, color = as.factor(formal))) +
  geom_point() +
  scale_color_manual(
    values = c("red", "blue"), 
    labels = c("Informal", "Formal") 
  ) +
  labs(
    color = "Employment Type",  
    title = "Hourly Salary Distribution by Age",
    subtitle = "Grouped by Employment Type (Formal or Informal)",
    x = "Age (Years)",  
    y = "Hourly Salary (COP)"  
  ) +
  theme_minimal()

#Distribution of Hourly Salary by Job Type

plot2 <- ggplot(db, aes(x = as.factor(relab), y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Hourly Salary by Job Type",
    x = "Job Type",
    y = "Hourly Salary (COP)"
  ) +
  theme_minimal()

#Density plot of Hourly Salary grouped by access to tertiary education

plot3 <- ggplot(db, aes(x = y_ingLab_m_ha, fill = as.factor(college))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Hourly Salary Distribution by College Education",
    x = "Hourly Salary (COP)",
    fill = "College (0 = No, 1 = Yes)"
  ) +
  theme_minimal()

#Histogram of Hourly Salary grouped by sex

plot4 <- ggplot(data = db) + 
  geom_histogram(mapping = aes(x = y_ingLab_m, group = as.factor(sex), fill = as.factor(sex)), bins = 30) + 
  scale_fill_manual(
    values = c("0" = "purple", "1" = "green"), 
    labels = c("0" = "Female", "1" = "Male"), 
    name = "Sex"
  ) +
  labs(
    title = "Histogram of Hourly Salary by Sex",
    x = "Hourly Salary (COP)",
    y = "Count"
  ) +
  theme_minimal()

#Employment Types for People with a College Degree OJO PONER MAS BONITO

plot5 <- ggplot(data = db) + 
  geom_bar(mapping = aes(x = relab, group = as.factor(college), fill = as.factor(college)), width = 0.7) + 
  scale_fill_manual(
    values = c("Yes" = "blue", "No" = "red"), 
    labels = c("Yes" = "College Degree", "No" = "No College Degree"), 
    name = "College Degree"
  ) +
  labs(
    title = "Employment Types for People with a College Degree",
    x = "Employment Type",
    y = "Number of People"
  ) +
  theme_minimal() 
