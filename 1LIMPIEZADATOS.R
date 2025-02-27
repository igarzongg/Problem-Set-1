# PROBLEM SET 1

#Juan Pablo Grimaldos Olivella - 202122627
#Isabella Garzón González - 202122524

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
       mosaic,
       officer,
       flextable,
       grid,
       lintr)

# IMPORTING DATABASES ----------------------------------------------------------

library(rvest)
library(dplyr)
all_tables <- list()

b_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

for (i in 1:10){
  url <- paste0(b_url, i , ".html")
  print(url)
  tab <- read_html(url) %>%
    html_element("table") %>%
    html_table()
  all_tables[[i]] <- tab
}

db <- bind_rows(all_tables)
head(db)

# CLEANING DATA ----------------------------------------------------------------

skim(db) %>% head()

summary(db$age)

freq_table_ocu <- table(db$ocu)
prop_table_ocu <- prop.table(table(db$ocu))
summary_table_ocu <- data.frame(
  Status = c("0 (N/Employed)", "1 (Employed)"), #Not employed contains all 
  #unemployed, economically inactive population, and population 
  #that is not working age. This is why we will use ocu as our emp. variable.
  Count = as.numeric(freq_table_ocu),     
  Proportion = round(as.numeric(prop_table_ocu), 4)  
)
print(summary_table_ocu, row.names = FALSE)

#Our analysis will only consider individuals with 18 or more years of age.

db <- db %>%
  filter(age > 18, ocu == 1)

#We will transform the sex variable to make its affirmative (1) value female.
#This means that the variable will display 1 if the ob's sex is female.

db <- db %>% 
  mutate(female = ifelse(sex==0,1,0))

#Before transforming our income variable(s), we have to take a look at its dist.

#Nominal hourly salaries

summary(db$y_ingLab_m_ha)

plot0 <- ggplot(db, aes(x = y_ingLab_m_ha )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Total Hourly Nominal Income", y = "N. Obs") +
  theme_bw() 

plot0

#Real hourly salaries

summary(db$log_real_income)

plot00 <- ggplot(db, aes(x = y_salary_m_hu )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Total Hourly Real Income", y = "N. Obs") +
  theme_bw() 

plot00

#There is no observations with 0 hourly income. Nonetheless, the dist. of this
#variable has a very long right tail. We will use the log of income instead.

db <- db %>% 
  mutate(log_nominal_income = ifelse(y_ingLab_m_ha>0, log(y_ingLab_m_ha), 0))

db <- db %>% 
  mutate(log_real_income = ifelse(y_salary_m_hu>0, log(y_salary_m_hu), 0))

# DATA TRANSFORMATION  ---------------------------------------------------------

#MISSING VALUES

db_miss <- skim(db) %>% dplyr::select( skim_variable, n_missing)
Nobs <- nrow(db) 
Nobs

db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

db_miss <- db_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))

db_miss<- db_miss %>% filter(n_missing!= 0)
head(db_miss, 10)
tail(db_miss, 10)


##Visualizing the structure of missing data
ggplot(db_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5))  


##Visualizing the 40 variables with the most missing values
ggplot(head(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) ,
                              y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))  # Set size for axis labels

##Visualizing the 40 variables with the least missing values

ggplot(tail(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) , 
                              y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))  # Set size for axis labels

## Selecting interest variables some variables

db <- db %>% #Household head variable created.
  mutate(H_Head = ifelse( p6050== 1, 1, 0))

db <- db %>% #Weekly work hours variable created
  mutate(Weekly_Hours_Worked = totalHoursWorked)

db <- db %>% #Weekly work hours variable created
  mutate(Employment_Sector = relab)


db_1 <- db %>% dplyr::select(directorio, secuencia_p, orden, female, age, ocu, 
                             Employment_Sector, H_Head, Weekly_Hours_Worked,
                             formal, sizeFirm, maxEducLevel, log_nominal_income,
                             log_real_income)

## Look at the missing variables by type. 
vis_dat(db_1)
vis_miss(db_1)

## Creating a dataset with all variables== 1 if missing

db_missing <- db_1 %>% mutate_all(~ifelse(!is.na(.), 1, 0))

## Dropping variables with not missing or  with all missing.

db_missing <-  db_missing %>%  dplyr::select(which(apply(db_missing, 2, 
                                                         sd) > 0))

## Correlation matrix for missing values

M <- cor(db_missing)
corrplot(M) ##Looks like most of our missing values are in the wage variables.

db_missing_salary <- db_1 %>%  filter(formal == 1)
vis_miss(db_missing_salary) 

db_missing_salary <- db_1 %>%  filter(formal == 0)
vis_miss(db_missing_salary) #Aha! Most missing values in salary come from 
#informal workers. This makes sense because informal workers don't usually know 
#their hourly wage, since they usually don't have a fixed income.

## Mean / Median visualization of nominal salary

ggplot(db, aes(log_nominal_income)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_nominal_income, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_nominal_income, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Nominal Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

##Graph shows evidence of a distribution of total income with a right tail. 
#This indicates that missing value imputation is more adequate via the median.

##Imputing missing values using the median salary of informal workers.

median_y_formal0 <- median(db$log_nominal_income[db$formal == 0], na.rm = TRUE)

db$log_nominal_income <- ifelse(is.na(db$log_nominal_income) & db$formal == 0, 
                                median_y_formal0, 
                                db$log_nominal_income)

## We will do the same for formal workers.

median_y_formal1 <- median(db$log_nominal_income[db$formal == 1], na.rm = TRUE)

db$log_nominal_income <- ifelse(is.na(db$log_nominal_income) & db$formal == 1, 
                                median_y_formal1, 
                                db$log_nominal_income)

## Mean / Median visualization of real salary

ggplot(db, aes(log_real_income)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_real_income, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_real_income, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Real Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

##Same thing happens here.

## Now, we check for how the data looks after the transformation.

median_y_salary0 <- median(db$log_real_income[db$formal == 0], na.rm = TRUE)

db$log_real_income <- ifelse(is.na(db$log_real_income) & db$formal == 0, 
                             median_y_salary0, 
                             db$log_real_income)

## We will do the same for formal workers.

median_y_salary1 <- median(db$log_real_income[db$formal == 1], na.rm = TRUE)

db$log_real_income <- ifelse(is.na(db$log_real_income) & db$formal == 1, 
                             median_y_salary1, 
                             db$log_real_income)




db_1 <- db %>% dplyr::select(directorio, secuencia_p, orden, female, age, ocu, 
                             Employment_Sector, H_Head, Weekly_Hours_Worked,
                             formal, sizeFirm, maxEducLevel, log_nominal_income,
                             log_real_income)

vis_miss(db_1)

ggplot(db, aes(log_nominal_income)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_nominal_income, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_nominal_income, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Nominal Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(db, aes(log_real_income)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_real_income, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_real_income, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Real Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Looks complete. The distribution for both income variables has now a median 
#closer to the mean.

#OUTLIERS ----------------------------------------------------------------------

#NOMINAL VARIABLE
#Winsorizing - Since distribution is not quite normal and we have skews.

print("Checking quantiles before winsorization:")
print(quantile(db$log_nominal_income, c(0.025, 0.975), na.rm = TRUE))

print(paste("Max before winsorization:", max(db$log_nominal_income, na.rm = TRUE)))

# Winsorizing at 97.5th percentile (adjusted slightly)
up_threshold_nomh <- quantile(db$log_nominal_income, 0.975, na.rm = TRUE) - 0.001 
# Adjust for rounding

db$log_nominal_income <- ifelse(db$log_nominal_income > up_threshold_nomh,
                                up_threshold_nomh, db$log_nominal_income)

print(paste("Max after winsorization:", max(db$log_nominal_income, na.rm = TRUE)))

# Verify the new summary
summary(db$log_nominal_income)

#REAL VARIABLE

#Winsorizing - Since distribution is not quite normal and we have skews.

print("Checking quantiles before winsorization:")
print(quantile(db$log_real_income, c(0.025, 0.975), na.rm = TRUE))

print(paste("Max before winsorization:", max(db$log_real_income, na.rm = TRUE)))

# Winsorizing at 97.5th percentile (adjusted slightly)
up_threshold_realh <- quantile(db$log_real_income, 0.975, na.rm = TRUE) - 0.001 
# Adjust for rounding

db$log_real_income <- ifelse(db$log_real_income > up_threshold_realh,
                             up_threshold_realh, db$log_real_income)

print(paste("Max after winsorization:", max(db$log_real_income, na.rm = TRUE)))

# Verify the new summary
summary(db$log_real_income)

#TOTAL HOURS WORKED

#Winsorizing - Since distribution is not quite normal and we have skews.

print("Checking quantiles before winsorization:")
print(quantile(db$Weekly_Hours_Worked, c(0.025, 0.975), na.rm = TRUE))

print(paste("Max before winsorization:", max(db$Weekly_Hours_Worked, na.rm = TRUE)))

# Winsorizing at 97.5th percentile (adjusted slightly)
up_threshold_hw <- quantile(db$Weekly_Hours_Worked, 0.975, na.rm = TRUE) - 0.001 
# Adjust for rounding

db$Weekly_Hours_Worked <- ifelse(db$Weekly_Hours_Worked > up_threshold_hw,
                                 up_threshold_hw, db$Weekly_Hours_Worked)

print(paste("Max after winsorization:", max(db$Weekly_Hours_Worked, na.rm = TRUE)))

# Verify the new summary
summary(db$Weekly_Hours_Worked)
