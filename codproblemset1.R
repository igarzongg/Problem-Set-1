
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
       MASS)

# IMPORTING DATABASES ----------------------------------------------------------

#OJO - esta es temporal

db <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
db <- as_tibble(db) 

# CLEANING DATA ----------------------------------------------------------------

skim(db) %>% head()

db <- db %>%
  filter(age > 18, ocu == 1)

# DATA TRANSFORMATION  ---------------------------------------------------------

#Aca toca ver que hacemos con los datos missing (ver cuaderno clase complementaria 1)

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
  Proportion = round(as.numeric(prop_table_form), 4)  
)
print(summary_table_form, row.names = FALSE)

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


freq_table_ocu <- table(db$ocu)
prop_table_ocu <- prop.table(table(db$ocu))
summary_table_ocu <- data.frame(
  Status = c("0 (N/Employed)", "1 (Employed)"), #Not employed contains all unemployed, economically inactive population, and population that is not working age.
  Count = as.numeric(freq_table_ocu),     
  Proportion = round(as.numeric(prop_table_ocu), 4)  
)
print(summary_table_ocu, row.names = FALSE)

#What fraction of the economically active population is employed?

pea_data <- subset(db, pea == 1)

freq_table <- table(pea_data$ocu)
prop_table <- prop.table(freq_table)

employment_summary <- data.frame(
  Status = c("N/Employed", "Employed"),  # Labels for employment status
  Count = as.numeric(freq_table),        # Number of observations
  Proportion = as.numeric(prop_table)    # Convert proportions to numeric
)
employment_summary$Label <- paste0(round(employment_summary$Proportion * 100, 1), "%")

ggplot(employment_summary, aes(x = "", y = Proportion, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +  # Create a bar to be transformed into a pie
  coord_polar("y") +  # Convert bar chart into a pie chart
  scale_fill_manual(values = c("blue", "red")) +  
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Add percentage labels
  labs(title = "Employment Status of EAP, GEIH (2018)") +
  theme_void()  

#CONTINUOUS VARIABLES



#Aca nos toca poner el stargazer con la variable de ingreso por hora, edad, y oras variables continuas de interes.
