
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

summary(db$y_salary_m_hu)

plot00 <- ggplot(db, aes(x = y_salary_m_hu )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Total Hourly Real Income", y = "N. Obs") +
  theme_bw() 

plot00

#There is no observations with 0 hourly income. Nonetheless, the dist. of this
#variable has a very long right tail. We will use the log of income instead.

db <- db %>% 
  mutate(y_ingLab_m_ha = ifelse(y_ingLab_m_ha>0, log(y_ingLab_m_ha), 0))

db <- db %>% 
  mutate(y_salary_m_hu = ifelse(y_salary_m_hu>0, log(y_salary_m_hu), 0))

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

db_1 <- db %>% dplyr::select(directorio, secuencia_p, orden, female, age, ocu, 
                       relab, p6050, totalHoursWorked,
                    formal, sizeFirm, maxEducLevel, y_ingLab_m_ha,
                    y_salary_m_hu)

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

ggplot(db, aes(y_ingLab_m_ha)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$y_ingLab_m_ha, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$y_ingLab_m_ha, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Nominal Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

##Graph shows evidence of a distribution of total income with a right tail. 
#This indicates that missing value imputation is more adequate via the median.

##Imputing missing values using the median salary of informal workers.

median_y_formal0 <- median(db$y_ingLab_m_ha[db$formal == 0], na.rm = TRUE)

db$y_ingLab_m_ha <- ifelse(is.na(db$y_ingLab_m_ha) & db$formal == 0, 
                           median_y_formal0, 
                           db$y_ingLab_m_ha)

## We will do the same for formal workers.

median_y_formal1 <- median(db$y_ingLab_m_ha[db$formal == 1], na.rm = TRUE)

db$y_ingLab_m_ha <- ifelse(is.na(db$y_ingLab_m_ha) & db$formal == 1, 
                           median_y_formal1, 
                           db$y_ingLab_m_ha)

## Mean / Median visualization of real salary

ggplot(db, aes(y_salary_m_hu)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$y_salary_m_hu, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$y_salary_m_hu, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Real Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

##Same thing happens here.

## Now, we check for how the data looks after the transformation.

median_y_salary0 <- median(db$y_salary_m_hu[db$formal == 0], na.rm = TRUE)

db$y_salary_m_hu <- ifelse(is.na(db$y_salary_m_hu) & db$formal == 0, 
                           median_y_salary0, 
                           db$y_salary_m_hu)

## We will do the same for formal workers.

median_y_salary1 <- median(db$y_salary_m_hu[db$formal == 1], na.rm = TRUE)

db$y_salary_m_hu <- ifelse(is.na(db$y_salary_m_hu) & db$formal == 1, 
                           median_y_salary1, 
                           db$y_salary_m_hu)

db_1 <- db %>% dplyr::select(directorio, secuencia_p, orden, female, age, ocu, 
                             relab, p6050, totalHoursWorked,
                             formal, sizeFirm, maxEducLevel, y_ingLab_m_ha,
                             y_salary_m_hu)

vis_miss(db_1)

ggplot(db, aes(y_ingLab_m_ha)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$y_ingLab_m_ha, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$y_ingLab_m_ha, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Nominal Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

ggplot(db, aes(y_salary_m_hu)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$y_salary_m_hu, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$y_salary_m_hu, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribution of Log Real Hourly Salary (COP)") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Looks complete. The distribution for both income variables has now a median 
#closer to the mean.

# DESCRIPTIVE VARIABLES --------------------------------------------------------

# CATEGORICAL VARIABLES

freq_table_female <- table(db$female)
prop_table_female <- prop.table(table(db$female))
summary_table_female <- data.frame(
  Sex = c("0 (Male)", "1 (Female)"),  
  Count = as.numeric(freq_table_female),     
  Proportion = round(as.numeric(prop_table_female), 4) 
)
print(summary_table_female, row.names = FALSE)

db <- db %>% #Household head variable created.
  mutate(H_Head = ifelse( p6050== 1, 1, 0))
freq_table_HHead <- table(db$H_Head)
prop_table_HHead <- prop.table(table(db$H_Head))
summary_table_HHead <- data.frame(
  HouseholdStatus = c("0 (N/HH)", "1 (Household Head)"),  
  Count = as.numeric(freq_table_HHead),     
  Proportion = round(as.numeric(prop_table_HHead), 4) 
)
print(summary_table_HHead, row.names = FALSE)

db <- db %>% #Female HH variable created.
  mutate(Head_Female = H_Head*(female))
freq_table_HHeadF <- table(db$Head_Female)
prop_table_HHeadF <- prop.table(table(db$Head_Female))
summary_table_HHeadF <- data.frame(
  HHeadSex = c("0 (N/FHH)", "1 (Female Household Head)"),  
  Count = as.numeric(freq_table_HHeadF),     
  Proportion = round(as.numeric(prop_table_HHeadF), 4) 
)
print(summary_table_HHeadF, row.names = FALSE)

freq_table_coll <- table(db$college)
prop_table_coll <- prop.table(table(db$college))
summary_table_coll <- data.frame(
  Academic_achievement = c("0 (Below Tertiary Education)", 
                           "1 (Tertiary Education or more)"),  
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
  Firm_size = c("self-employed ", "2-5 workers ", "6-10 workers ",
                "11-50 workers", 
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
  y_salary_m_hu =  c(sum(!is.na(db$y_salary_m_hu)),
                     mean(db$y_salary_m_hu, na.rm = TRUE),
                     sd(db$y_salary_m_hu, na.rm = TRUE),
                     min(db$y_salary_m_hu, na.rm = TRUE),
                     max(db$y_salary_m_hu, na.rm = TRUE)),
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

#TABLES -----------------------------------------------------------------------

#Hourly salary distribution by age grouped by employment type 
#(Formal or Informal)

plot1 <- ggplot(db, aes(x = age, y = y_ingLab_m_ha, 
                        color = as.factor(formal))) +
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
    y = "Log Nominal Hourly Salary (COP)"  
  ) +
  theme_minimal()

plot1

#Distribution of Hourly Salary by Job Type

plot2 <- ggplot(db, aes(x = as.factor(relab), y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Hourly Salary by Job Type",
    x = "Job Type",
    y = "Log Nominal Hourly Salary (COP)"
  ) +
  theme_minimal()

plot2 #Here we can start to identify a lot of observations with significant leverage.

#Density plot of Hourly Salary grouped by access to tertiary education

plot3 <- ggplot(db, aes(x = y_ingLab_m_ha, fill = as.factor(college))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Hourly Salary Distribution by College Education",
    x = "Log Nominal Hourly Salary (COP)",
    fill = "College (0 = No, 1 = Yes)"
  ) +
  theme_minimal()

plot3

#Histogram of Hourly Salary grouped by sex

plot4 <- ggplot(data = db) + 
  geom_histogram(mapping = aes(x = y_ingLab_m, group = as.factor(female), 
                               fill = as.factor(female)), bins = 30) + 
  scale_fill_manual(
    values = c("0" = "green", "1" = "purple"), 
    labels = c("0" = "Male", "1" = "Female"), 
    name = "Sex"
  ) +
  labs(
    title = "Histogram of Hourly Salary by Sex",
    x = "Log Nominal Hourly Salary (COP)",
    y = "Count"
  ) +
  theme_minimal()

plot4

#Employment Types for People with a College Degree OJO PONER MAS BONITO

plot5 <- ggplot(data = db) + 
  geom_bar(mapping = aes(x = relab, group = as.factor(college),
                         fill = as.factor(college)), width = 0.7) + 
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

plot5

#Total hours worked by female household head-ship status


plot6 <- ggplot(db, aes(x = as.factor(Head_Female), y = totalHoursWorked)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Hourly Salary by Job Type",
    x = "Job Type",
    y = "Log Nominal Hourly Salary (COP)"
  ) +
  theme_minimal()

plot6 


# AGE-WAGE PROFILFE ------------------------------------------------------------

db <- db %>% mutate(age2= age^2)

model1 <- lm(y_ingLab_m_ha  ~ age + age2, data= db)


model2 <- lm(y_ingLab_m_ha  ~ age, data= db)

stargazer(model1, model2, type="text",
           covariate.labels=c("Age","Agesq"))

residualsmodel1 <- residuals(model1)
residualsmodel2 <- residuals(model2)

ggplot(data= db, 
       mapping = aes(x=residualsmodel1)) +
  theme_bw() + 
  geom_density() 

ggplot(data= db, 
       mapping = aes(x=residualsmodel2)) +
  theme_bw() + 
  geom_density() 

ggplot(db , aes(y = residualsmodel1 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

ggplot(db , aes(y = residualsmodel2 , x = orden )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db<-db %>% mutate(m1_std_residuals= studres(model1) )
db<-db %>% mutate(m2_std_residuals= studres(model2) )


ggplot(db , aes(y = m1_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db <- db %>% 
  filter(m1_std_residuals < 2 & m1_std_residuals > -2 & 
           m2_std_residuals < 2 & m2_std_residuals > -2)


model1 <- lm(y_ingLab_m_ha  ~ age + age2, data= db)

# Extract coefficients
beta1 <- coef(model1)["age"]
beta2 <- coef(model1)["age2"]

# Compute the age at which income is maximized
age_max <- -beta1 / (2 * beta2)

age_max #Income is maximized at this age 

model2 <- lm(y_ingLab_m_ha  ~ age, data= db)

stargazer(model1, model2, type="text",
          covariate.labels=c("Age","Squared Age"))

db <- db  %>% mutate(yhat1=predict(model1), yhat2=predict(model2)) 

summ <- db %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_y = mean(y_ingLab_m_ha),
    yhat_reg1 = mean(yhat1),
    yhat_reg2 = mean(yhat2), .groups="drop"
  ) 


head(summ)

## Graph displaying relationship between variables

ggplot(summ) + 
  geom_point(
    aes(x = age, y = mean_y),
    color = "blue", size = 2
  ) + 
  geom_line(
    aes(x = age, y = yhat_reg1), 
    color = "green", linewidth = 1.5
  ) + 
  geom_line(
    aes(x= age, y= yhat_reg2),
    color = "orange", lindewidth = 1.5
    
  ) +
  labs(
    title = "ln Hourly Wages by Age in the GEIH",
    x = "Age",
    y = "ln Hourly Wages"
  ) +
  theme_bw()

## Finding the 'peak-age' with CI's according by bootstrapping the the model

set.seed(101110)

B <- 1000

estimates_model1<-rep(NA,B)

for(i in 1:B){
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  model1 <- lm(y_ingLab_m_ha  ~ age + age2, db_sample)
  
  beta1<-model1$coefficients[2] # gets the coefficient of interest 
  beta2 <- model1$coefficients[3]
  
  age_maxb <- -beta1 / (2 * beta2)
  
  estimates_model1[i]<- age_maxb #saves it in the above vector
}

length(estimates_model1)

plot(hist(estimates_model1))

mean(estimates_model1)

sqrt(var(estimates_model1))

confint(estimates_model1, level=0.95) #CI from Bootstrap Distribution with (B=1000)





