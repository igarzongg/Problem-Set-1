
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
print(summary_table_educ, row.names = FALSE)

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

#OUTLIERS ----------------------------------------------------------------------


##boxplot for Hours Worked

boxplotynomh <- ggplot(data= db, 
       mapping = aes(y=y_ingLab_m_ha, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Total Hours Worked")+
  xlab("")

low_nomh <- mean(db$y_ingLab_m_ha) - 2* sd(db$y_ingLab_m_ha)
up_nomh <- mean(db$y_ingLab_m_ha) + 2* sd(db$y_ingLab_m_ha)

cut_nomh <- boxplotynomh + geom_hline(yintercept = 
                              low_nomh,linetype="solid",color="red",size=0.7) +
  geom_hline(yintercept = up_nomh,linetype="solid",color="red",size=0.7)

#Maybe it's more convenient to winsorize by quantiles

#Winsorizing

treshold_nomh<- quantile(db$y_ingLab_m_ha, 0.975, na.rm=T)


db<- db %>% mutate(y_ingLab_m_ha= 
                     ifelse( test=( y_ingLab_m_ha>= treshold_nomh), 
                                                yes= treshold_nomh,
                                                no= y_ingLab_m_ha))
boxwinsornomh <-ggplot(data= db, 
          mapping = aes(y=y_ingLab_m_ha, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Log Nominal Hourly Wage")+
  xlab("")

low_nomh <- mean(db$y_ingLab_m_ha) - 2* sd(db$y_ingLab_m_ha)
up_nomh <- mean(db$y_ingLab_m_ha) + 2* sd(db$y_ingLab_m_ha)

boxwinsornomh <- boxwinsornomh +
  geom_hline(yintercept = low_nomh,linetype="solid",color="blue",size=0.7) +
  geom_hline(yintercept = up_nomh,linetype="solid",color="blue",size=0.7)
boxwinsornomh


grid.arrange(boxwinsornomh, cut_nomh, ncol = 2)

#DESC. STATS GRAPHS ------------------------------------------------------------

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

plot2 #Here we can start to identify a lot of observations with high leverage.

#Density plot of Hourly Salary grouped by access to tertiary education

plot3 <- ggplot(db, aes(x = y_ingLab_m_ha, fill = as.factor(maxEducLevel))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Hourly Salary Distribution by Education Level",
    x = "Log Nominal Hourly Salary (COP)",
    fill = "Education Level"
  ) +
  theme_minimal()

plot3

#Histogram of Hourly Salary grouped by sex

plot4 <- ggplot(data = db) + 
  geom_histogram(mapping = aes(x = y_ingLab_m_ha, group = as.factor(female), 
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

#PONER MAS GRAFICOS

#Total hours worked by female household head-ship status


plot6 <- ggplot(db, aes(x = as.factor(Head_Female), y = totalHoursWorked)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Total Hours Worked comparison with Female Household Heads",
    x = "(0) Not Female HH (1) Female HH",
    y = "Log Nominal Hourly Salary (COP)"
  ) +
  theme_minimal()

plot6 


# AGE-WAGE PROFILFE ------------------------------------------------------------

db <- db %>% mutate(age2= age^2)

model1 <- lm(y_ingLab_m_ha  ~ age + age2, data= db)

model2 <- lm(y_ingLab_m_ha  ~ age, data= db)

model3 <- lm(y_salary_m_hu  ~ age + age2, data= db)

model4 <- lm(y_salary_m_hu  ~ age, data= db)

stargazer(model1, model2, type="text", #Model results for Nominal Hourly Wage
           covariate.labels=c("Age","Agesq"))

stargazer(model1, model2, type="text", #Model results for Real Hourly Wage
          covariate.labels=c("Age","Agesq"))

residualsmodel1 <- residuals(model1)
residualsmodel2 <- residuals(model2)

residualsmodel3 <- residuals(model3)
residualsmodel4 <- residuals(model4)

ggplot(data= db, 
       mapping = aes(x=residualsmodel1)) +
  theme_bw() + 
  geom_density() 

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


db<-db %>% mutate(m1_std_residuals= studres(model1) )
db<-db %>% mutate(m2_std_residuals= studres(model2) )

db<-db %>% mutate(m3_std_residuals= studres(model3) )
db<-db %>% mutate(m4_std_residuals= studres(model4) )


ggplot(db , aes(y = m1_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

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

ggplot(db , aes(y = m4_std_residuals , x = orden)) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db <- db %>% 
  filter(m1_std_residuals < 2 & m1_std_residuals > -2 & 
           m2_std_residuals < 2 & m2_std_residuals > -2 & m3_std_residuals 
         < 2 & m3_std_residuals > -2 & 
           m4_std_residuals < 2 & m4_std_residuals > -2)

## Estimation procedure for NOMINAL Hourly Wage --------------------------------

model1 <- lm(y_ingLab_m_ha  ~ age + age2, data= db)

# Extract coefficients
beta1 <- coef(model1)["age"]
beta2 <- coef(model1)["age2"]

# Compute the age at which income is maximized
age_max <- -beta1 / (2 * beta2)

age_max #Income is maximized at this age 

model2 <- lm(y_ingLab_m_ha  ~ age, data= db)

stargazer(model1, model2, type="text",
          covariate.labels=c("Age","Squared Age"), 
          dep.var.labels = "Log Nominal Hourly Wage)", 
          title = 'Quadratic vs Linear model')

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

# Compute standard errors for confidence intervals
summ <- summ %>%
  mutate(
    se_yhat1 = sd(yhat_reg1, na.rm = TRUE) / sqrt(n()),
    lower_yhat1 = yhat_reg1 - 1.96 * se_yhat1,
    upper_yhat1 = yhat_reg1 + 1.96 * se_yhat1,
    
    se_yhat2 = sd(yhat_reg2, na.rm = TRUE) / sqrt(n()),
    lower_yhat2 = yhat_reg2 - 1.96 * se_yhat2,
    upper_yhat2 = yhat_reg2 + 1.96 * se_yhat2
  )

# Graphing with confidence intervals and proper formatting
ggplot() + 
  # Scatter plot of observed mean hourly wages
  geom_point(
    data = summ, 
    aes(x = age, y = mean_y),
    color = "blue", size = 2, alpha = 0.5
  ) + 
  
  # Confidence interval for Model 1
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat1, ymax = upper_yhat1, fill = "Model 1"),
    alpha = 0.2
  ) +
  
  # Line for Model 1
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg1, color = "Model 1"),
    linewidth = 1.5
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
  
  # Labels and title
  labs(
    title = "Predicted Age-Wage Profile with Confidence Intervals",
    subtitle = "Based on two different regression models",
    x = "Age",
    y = "Log Nominal Hourly Wages",
    color = "Model",
    fill = "Confidence Interval",
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


## Finding the 'peak-age' with CI's according by bootstrapping the the model

set.seed(101110)

B <- 1000

estimates_model1<-rep(NA,B)

for(i in 1:B){
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
#takes a sample with replacement of the same size of the original sample 
  
  model1 <- lm(y_ingLab_m_ha  ~ age + age2, db_sample)
  
  beta1<-model1$coefficients[2] # gets the coefficient of interest 
  beta2 <- model1$coefficients[3]
  
  age_maxb <- -beta1 / (2 * beta2)
  
  estimates_model1[i]<- age_maxb #saves it in the above vector
}

length(estimates_model1)

plot(hist(estimates_model1))

meanpeakage <- mean(estimates_model1)

sepeakage <- sqrt(var(estimates_model1))

ci_lower <- quantile(estimates_model1, 0.025)
ci_upper <- quantile(estimates_model1, 0.975)

## Estimation procedure for REAL Hourly Wage -----------------------------------

model3 <- lm(y_salary_m_hu  ~ age + age2, data= db)

# Extract coefficients
beta1r <- coef(model3)["age"]
beta2r <- coef(model3)["age2"]

# Compute the age at which income is maximized
age_maxr <- -beta1r / (2 * beta2r)

age_maxr #Income is maximized at this age 

model4 <- lm(y_salary_m_hu  ~ age, data= db)

stargazer(model3, model4, type="text",
          covariate.labels=c("Age","Squared Age"), 
          dep.var.labels = "Log Real Hourly Wage", 
          title = 'Quadratic vs Linear model')

db <- db  %>% mutate(yhat1r=predict(model3), yhat2r=predict(model4)) 

summ <- db %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_yr = mean(y_salary_m_hu),
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
    aes(x = age, ymin = lower_yhat1r, ymax = upper_yhat1r, fill = "Model 1"),
    alpha = 0.2
  ) +
  
  # Line for Model 1
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg1r, color = "Model 1"),
    linewidth = 1.5
  ) + 
  
  # Confidence interval for Model 2
  geom_ribbon(
    data = summ, 
    aes(x = age, ymin = lower_yhat2r, ymax = upper_yhat2r, fill = "Model 2"),
    alpha = 0.2
  ) +
  
  # Line for Model 2
  geom_line(
    data = summ, 
    aes(x = age, y = yhat_reg2r, color = "Model 2"),
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


## Finding the 'peak-age' with CI's according by bootstrapping the the model

set.seed(101111)

B <- 1000

estimates_model3<-rep(NA,B)

for(i in 1:B){
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
#takes a sample with replacement of the same size of the original sample.
  
  model3 <- lm(y_salary_m_hu  ~ age + age2, db_sample)
  
  beta1r<-model3$coefficients[2] # gets the coefficient of interest 
  beta2r <- model3$coefficients[3]
  
  age_maxbr <- -beta1r / (2 * beta2r)
  
  estimates_model3[i]<- age_maxbr #saves it in the above vector
}

length(estimates_model3)

plot(hist(estimates_model3))

meanpeakager <- mean(estimates_model3)

sepeakager <- sqrt(var(estimates_model3))

ci_lowerr <- quantile(estimates_model3, 0.025)
ci_upperr <- quantile(estimates_model3, 0.975)

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
          title = "Comparison of Nominal and Real Bootstrap Estimates", 
          digits = 2)


# GENDER-EARNINGS GAP ----------------------------------------------------------

#a).

model5 <- lm(y_ingLab_m_ha  ~ female, data= db)

stargazer(model5, type="text", 
          covariate.labels=c("Female"), 
          dep.var.labels = "Log Nominal Hourly Wage", 
          title = 'Unconditional Wage Gap Model')

model6 <- lm(y_salary_m_hu  ~ female, data= db)

stargazer(model6, type="text", 
          covariate.labels=c("Female"), 
          dep.var.labels = "Log Real Hourly Wage", 
          title = 'Unconditional Wage Gap Model')

#b).

model7 <- lm(y_ingLab_m_ha ~ female + age +age2 + relab + Head_Female +
            totalHoursWorked + formal + sizeFirm + maxEducLevel, data=db)


stargazer(model5, model7, type="text", 
          covariate.labels=c("Female", "Age", 'Age2', "Employment Sector",
                             "Female Household Head", "Total Hours Worked", 
                             "Formal", "Size of Firm", "Max. Education Level"), 
          dep.var.labels = "Log Nominal Hourly Wage", 
          title = 'Unconditional vs.Conditional Wage Gap Model')


#FWL ---------------------------------------------------------------------------

db<-db %>% mutate(femaleResidX=lm(female~ age + age2 + relab + Head_Female +
                                    totalHoursWorked + formal + 
                                    sizeFirm + maxEducLevel, db)$residuals) 
#Residuals of regression female ~ X 

db<-db %>% mutate(loghwageResidX=lm(y_ingLab_m_ha~ age + age2  
                                    + relab + Head_Female +
                                    totalHoursWorked + formal + 
                                    sizeFirm + maxEducLevel, db)$residuals) 

#Residuals of regression log nominal hourly wage ~ X 

modelRESFWL <- lm(loghwageResidX~femaleResidX, db)
stargazer(model7, modelRESFWL, type="text",digits=7,
          covariate.labels=c("Female", "Age", "Employment Sector",
          "Female Household Head", "Total Hours Worked", 
          "Formal", "Size of Firm", "Max. Education Level", "FWL estimate"), 
          dep.var.labels = c("Log Nominal Hourly Wage", "Residualized Model"), 
          title = 'OLS vs FWL estimates')

SSROLSmodel7 <-sum(resid(model7)^2)
SSRFWLmodel7 <-sum(resid(modelRESFWL)^2) #Both models have the same SSR.

##Adjusting the degrees of freedom by the 7 residualized predictors:

df_OLSmodel7 = model7$df[1]
df_FWLmodel7 = modelRESFWL$df[1]

sqrt(diag(vcov(modelRESFWL))*(df_FWLmodel7/df_OLSmodel7))[2]

sqrt(diag(vcov(model7)))[2] #Degrees of freedom have been succesfully adjusted.
#Standard error now matches the one of the original regression.



#FWL WITH BOOTSTRAP ------------------------------------------------------------

#Displaying regression and y_hat function by sex:

model7men <- lm(y_ingLab_m_ha ~ female + age +age2 + relab + Head_Female +
               totalHoursWorked + formal + sizeFirm + maxEducLevel, data=db,
             subset = (female == 0))

model7females <- lm(y_ingLab_m_ha ~ female + age +age2 + relab + Head_Female +
                    totalHoursWorked + formal + sizeFirm + maxEducLevel,
                    data=db,
                  subset = (female == 1))

# Create predicted values with NA for the opposite gender
db <- db %>%
  mutate(yhat1men = ifelse(female == 0, predict(model7men, newdata = db), NA),
         yhat2females = ifelse(female == 1, 
                               predict(model7females, newdata = db), NA))


summ2 <- db %>%  
  group_by(
    female, age, age2
  ) %>%  
  summarize(
    mean_hourly_salary= mean(y_salary_m_hu),#Mean hourly salary for  sample
    yhat_reg7men = mean(yhat1men), #Predicted avg. hourly salary for this male
    yhat_reg7females = mean(yhat2females), .groups="drop"
  ) #Predicted avg. hourly salary for this female



summ2_men <- summ2 %>% filter(!is.na(yhat_reg7men)) #Data for males
summ2_females <- summ2 %>% filter(!is.na(yhat_reg7females)) #Data for females


#GRAPHING

# Compute standard errors for confidence intervals
summ2_men <- summ2_men %>%
  mutate(se = sd(yhat_reg7men, na.rm = TRUE) / sqrt(n()),
         lower = yhat_reg7men - 1.96 * se,
         upper = yhat_reg7men + 1.96 * se)

summ2_females <- summ2_females %>%
  mutate(se = sd(yhat_reg7females, na.rm = TRUE) / sqrt(n()),
         lower = yhat_reg7females - 1.96 * se,
         upper = yhat_reg7females + 1.96 * se)

# Graphing with confidence intervals and proper labels
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
    title = "Predicted Age-Wage Profile with Confidence Intervals by Sex",
    subtitle = "Based on regression models estimated separately for
    males and females",
    x = "Age",
    y = "Log Nominal Hourly Wages",
    color = "Sex",
    caption = "Note: Predicted values are 
    from separate regressions for males and females.\nConfidence 
    intervals represent 95% uncertainty bands."
  ) +
  
 #Theme
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))


#BOOTSTRAPPING

set.seed(101112)

B <- 1000

estimates_FWLmodel7<-rep(NA,B)

agesmax_FWLmodel7men<-rep(NA,B)
agesmax_FWLmodel7females<-rep(NA,B)


for(i in 1:B){
  
  
  db_sample<- sample_frac(db,size=1,replace=TRUE) 
  
  #takes a sample with replacement of the same size of the original sample.
  
  db<-db %>% mutate(femaleResidXbs=lm(female~ age + age2+ relab + Head_Female +
                                      totalHoursWorked + formal + 
                                      sizeFirm + maxEducLevel,
                                      data = db_sample)$residuals) 
  #Residuals of regression female ~ X 
  
  db<-db %>% mutate(loghwageResidXbs=lm(y_ingLab_m_ha~ age +
                                        age2 + relab + Head_Female +
                                        totalHoursWorked + formal + 
                                  sizeFirm + maxEducLevel, 
                                  data =db_sample)$residuals) 
  
  #Residuals of regression log nominal hourly wage ~ X 
  
  modelRESFWLbs <- lm(loghwageResidXbs~femaleResidXbs, db_sample)
  
  model7bsmen <- lm(y_ingLab_m_ha ~ female + age +age2 + relab + Head_Female +
                 totalHoursWorked + formal + sizeFirm + maxEducLevel, 
                 subset = (female == 0 ),
                 data=db_sample)
  
  model7bsfemales <- lm(y_ingLab_m_ha ~ female + age +age2 + 
                          relab + Head_Female +
                      totalHoursWorked + formal + sizeFirm + maxEducLevel, 
                    subset = (female == 1 ),
                    data=db_sample)
  
  beta1RES_fwlbs <-modelRESFWLbs$coefficients[2] 
  # gets the coefficient of interest 
  
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

plot(hist(estimates_FWLmodel7))

plot(hist(agesmax_FWLmodel7men))
plot(hist(agesmax_FWLmodel7females))

meancoefFWLbs <- mean(estimates_FWLmodel7)

meanpeakagebsmen <- mean(agesmax_FWLmodel7men)
meanpeakagebsfemales <- mean(agesmax_FWLmodel7females)

secoefFWLbs <- sqrt(var(estimates_FWLmodel7))

sepeakagebsmen <- sqrt(var(agesmax_FWLmodel7men))
sepeakagebsfemales <- sqrt(var(agesmax_FWLmodel7females))

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

# Printing table
stargazer(merged_table2, summary = FALSE, type = "text",
          title = "Bootstrap Peak Age Estimates by Sex", digits = 2)

#Creating FWL Bootstrap 'female' coefficient estimation table:

summary_tableBETA1s <- data.frame(
  Statistic = c("Bootstrap + FWL Mean", "Standard Error", 
                "Lower 95% CI", "Upper 95% CI", 'FWL-only value', 'OLS value'),
  Beta1Coef_Value = c(meancoefFWLbs, secoefFWLbs, 
                   ci_lowercoefFWLbs, ci_uppercoefFWLbs, 
                   modelRESFWL$coefficients[2], model7$coefficients[2])
)

# Printing table
stargazer(summary_tableBETA1s, summary = FALSE, type = "text",
          title = "Bootstrap 
          + FWL Coefficient Stats vs FWL and OLS Coefficients", digits = 4)

# PREDICTING EARNINGS ----------------------------------------------------------

set.seed(10101)

#a). 

inTrain <- createDataPartition(
  y = db$y_ingLab_m_ha,  ## the outcome data are needed
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

reg_1<- y_ingLab_m_ha ~ age + age2

model1 <- lm(reg_1,
               data = training)
 
# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model1, testing)


score1<- RMSE(predictions, testing$y_ingLab_m_ha )
score1
# 0.3489662

# MODEL 2 ----------------------------------------------------------------------

reg_2<- y_ingLab_m_ha ~ age

model2 <- lm(reg_2,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model2, testing)


score2<- RMSE(predictions, testing$y_ingLab_m_ha )
score2
# 0.3510614

# ---------- model 3 & model 4 results for real Hourly Wage -----------------
# MODEL 3 ----------------------------------------------------------------------

reg_3<- y_salary_m_hu  ~ age + age2

model3 <- lm(reg_3,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model3, testing)


score3<- RMSE(predictions, testing$y_salary_m_hu )
score3
# 0.3241643

# MODEL 4 ----------------------------------------------------------------------

reg_4<- y_salary_m_hu  ~ age 

model4 <- lm(reg_4,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model4, testing)


score4<- RMSE(predictions, testing$y_salary_m_hu )
score4
# 0.3254666

# MODEL 5 ----------------------------------------------------------------------

reg_5<- y_ingLab_m_ha  ~ female

model5 <- lm(reg_5,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model5, testing)


score5<- RMSE(predictions, testing$y_ingLab_m_ha )
score5
# 0.3524142

# MODEL 6 ----------------------------------------------------------------------

reg_6 <- y_salary_m_hu  ~ female

model6 <- lm(reg_6,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model6, testing)


score6<- RMSE(predictions, testing$y_salary_m_hu )
score6
# 0.3263867 

# MODEL 7 ----------------------------------------------------------------------

reg_7 <- y_ingLab_m_ha ~ female + age +age2 + relab + Head_Female +
  totalHoursWorked + formal + sizeFirm + maxEducLevel

model7 <- lm(reg_7,
              data = training)

# PERFORMANCE RMSE -------------------------------------------------------------
predictions <- predict(model7, testing)


score7<- RMSE(predictions, testing$y_ingLab_m_ha )
score7
# 0.2709422

# MODELOS CON MAYOR COMPLEJIDAD/NO LINEALES ------------------------------------

# MODELO 8 - polynomial regression (age^3) ---------------------------------------------

reg_8 <- y_ingLab_m_ha ~ age + poly(age, 3) + female

model8 <- lm(reg_8, 
              data = training)

# PERFORMANCE RMSE 

predictions <- predict(model8, testing)

score8 <- RMSE(predictions, testing$y_ingLab_m_ha)
score8
# 0.3476926

# MODELO 9 - polynomial regression on age, education and hours worked-----------
# non-linearity in education and hours worked (posibles rendimientos marginales)

reg_9 <- y_ingLab_m_ha ~ poly(age, 3) + poly(maxEducLevel, 2) + poly(totalHoursWorked, 2) + female + relab + formal + sizeFirm
model9 <- lm(reg_9, 
             data = training)

# PERFORMANCE RMSE 

predictions <- predict(model9, testing)

score9 <- RMSE(predictions, testing$y_ingLab_m_ha)
score9
# 0.2672922

# MODELO 10 - interaction age & gender -----------------------------------------

reg_10 <- y_ingLab_m_ha ~ age * female + age2 + relab + totalHoursWorked + maxEducLevel
model10 <- lm(reg_10, 
             data = training)

# PERFORMANCE RMSE 

predictions <- predict(model10, testing)

score10 <- RMSE(predictions, testing$y_ingLab_m_ha)
score10
# 0.312069

# MODELO 11 - interaction formal work & gender -----------------------------------------

reg_11 <- y_ingLab_m_ha ~ formal*female + age + age2 + relab + totalHoursWorked + maxEducLevel
model11 <- lm(reg_11, 
              data = training)

# PERFORMANCE RMSE 

predictions <- predict(model11, testing)

score11 <- RMSE(predictions, testing$y_ingLab_m_ha)
score11
# 0.2743492

# MODELO 12 - multiple interactions --------------------------------------------

reg_12 <- y_ingLab_m_ha ~ maxEducLevel * female + formal * sizeFirm + age * age2 + relab * totalHoursWorked
model12 <- lm(reg_12, 
              data = training)

# PERFORMANCE RMSE 

predictions <- predict(model12, testing)

score12 <- RMSE(predictions, testing$y_ingLab_m_ha)
score12

# 0.2624852

# PERFORMANCE RESULT TABLE 

results <- data.frame(
  model = c("Model 1", "Model 2", "Model 3", 
             "Model 4", "Model 5", "Model 6", 
             "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12") ,
  RMSE = c(score1, score2, score3, score4, score5, score6, score7, 
           score8, score9, score10, score11, score12)
)

results <- results[order(results$RMSE), ]
print(results)

lowest <- results$model[1]
lowest_RMSE <- results$RMSE[1]

lowest # model 
lowest_RMSE # lowest RMSE

## Prediction errors in test sample --------------------------------------------

model <- c("Model 1", "Model 2", "Model 3", 
          "Model 4", "Model 5", "Model 6", 
          "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12")

# predictions 
pred <- list(score1, score2, score3, score4, score5, score6, score7, 
             score8, score9, score10, score11, score12)

best_pred <- NULL 

for (i in seq_along(model)) {
  if (model[i] == lowest) {
    best_pred <- pred[[i]]
    break 
  }
}

pred_errors <- testing$y_ingLab_m_ha - best_pred
pred_errors

# pred_errors graph -------------------------------------------------------------

library(ggplot2)

dist_errors <- ggplot(d = data.frame(pred_errors), aes(x = pred_errors)) + 
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black") + 
  labs(title = "Prediction errors distribution", x = "predicted errors") + 
  theme_classic()

dist_errors

## outliers 


## LOOVC 

ctrl <- trainControl(
  method = "LOOCV")

## Model 12 ---- first lowest predictive error 

model1_best_loovc <- train(reg_12,
                  data = db,
                  method = 'lm', 
                  trControl= ctrl)

head(model1_best_loovc)

score_best1 <-RMSE(model1_best_loovc$pred$pred, db$y_ingLab_m_ha)

scores_LOOCV<- append(scores_LOOCV, score_best1)


## Model 9 ---- second lowest predictive error 
model1_best_loovc

model2_best_loovc <- train(reg_9,
                           data = db,
                           method = 'lm', 
                           trControl= ctrl)


model2_best_loovc

score_best2 <-RMSE(model2_best_loovc$pred$pred, db$y_ingLab_m_ha)

scores_LOOCV<- append(scores_LOOCV, score_best2)
