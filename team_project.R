# Team project

data <- read.csv("./project_data.csv")

# It is perceived that patients with liver transplant have problems with sleep. 
# In addition, it is hypothesized that sleep disturbance has a direct effect on Quality of Life. 

# The objective of this project is the investigation of these hypotheses with 
# the use of an observational dataset of 268 patients with liver transplant. 

mydata_raw <- subset(mydata, , c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease",
                                  "Rejection.graft.dysfunction", "Any.fibrosis",
                                  "Renal.Failure", "Depression", "Corticoid",
                                  "Epworth.Sleepiness.Scale", "Pittsburgh.Sleep.Quality.Index.Score",
                                  "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                                  "SF36.PCS", "SF36.MCS"))

# --------------------------------------------------------------------------------
# EDA 
library(funModeling) 
library(Hmisc)

# Function for checking if any empty values (i.e. just "", and not as an NA)
empty_string <- function(col) {
  any(col == "")
}

# Exploratory Descriptive Analysis from Datasciencehereos (listed in assignment)
basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

str(mydata_raw)

glimpse(mydata_raw)

basic_eda(mydata_raw)
# No zeroes in gender 

empty_string <- function(col) {
  any(col == "")
}

empty_string(mydata_raw)
# No blank strings (makes sense as all data is in numeric form), 
# 268 obs TOTAL 
# Only age, BMI, and time from transplant is considered continoues, rest is categorical 
# NAs in BMI (2), Age (2), BMI(23), Epworth (17), Pitt(85), Athens (6), Berlin (6)
# SF36(21), SF36(21) 
# no extreme outliers in age (only at 70, loqwest is at 18 --> GOOD ONLY ADULTS+)
# some concerns with BMI (40+ is morbid obesity, highest 5 is 45, 43, 42, 42, 41)
# DO WE REMOVE OR NO? 
# (I think it is ok to leave)

# LEGEND FOR CATEGORICAL (FROM DATA DICTIONARY)
#' gender <- 1 = M, 2 = F 
#' Liver.Diagnosis <- Liver Diagnosis (1 = Hep C, 2 = Hep B, 3 = PSC/PBC/AHA, 
#'    4 = alcohol, 5 = other NASH/Hepatoma, Biliary atresia, 
#'    Tyrosenemia/congenital/neonatal/AAT/Wilsons, HCC, FHF, 
#'    cryptogenic, Angiosarcoma/tumor)
#' recurrence <- Recurrence of original disease process (1 = yes, 0 = no)
#' rejection <- Evidence of rejection/graft dysfunction either 
#'    as evidenced by pathology or by clinical judgement based on treating
#'     hepatologist (1 = yes, 0 = no)
#' Any.fibrosis <- Evidence of fibrosis grade A2 and higher (1 = yes, 0 = no)
#' renal.failure <- Renal failure (1 = yes, 0 = no)
#' depression <- Depression (1 = yes, 0 = no)
#' steriod <- Corticosteroid (1 = yes, 0 = no)

# Setting gender as 1 = F, 0 = M, to match the rest of the data 
mydata_raw$Gender <- ifelse(mydata_raw$Gender == "2", 1, 0)

mydata_num <- mydata_raw %>%
    select(gender = Gender, age = Age, BMI, time.transplant = Time.from.transplant, 
           liver.diagnosis = Liver.Diagnosis, disease.recurrence = Recurrence.of.disease, 
           graft.rejection.dys = Rejection.graft.dysfunction, fibrosis = Any.fibrosis, renal.failure = Renal.Failure, 
           depression = Depression, corticoid = Corticoid, epworth.sleep.scale = Epworth.Sleepiness.Scale, 
           pittsburgh.quality.score = Pittsburgh.Sleep.Quality.Index.Score, athens.insomnia.scale = Athens.Insomnia.Scale, 
           berlin.sleep.scale = Berlin.Sleepiness.Scale, SF36.PCS, SF36.MCS)

# --------------------------------------------------------------------------------


mydata_raw$Gender_fctr <- ifelse(mydata_raw$Gender == "1", "F", "M")
mydata_raw$Gender_fctr <- factor(mydata_raw$Gender_fctr) 

mydata_raw$Liver.Diagnosis_fctr <- ifelse(mydata_raw$Liver.Diagnosis == "1", "Hep C", 
                                     ifelse(mydata_raw$Liver.Diagnosis == "2", "Hep B", 
                                            ifelse(mydata_raw$Liver.Diagnosis == "3", "PSC/PBC/AHA", 
                                                   ifelse(mydata_raw$Liver.Diagnosis == "4", "Alcohol", "Other"))))
mydata_raw$Liver.Diagnosis_fctr <- factor(mydata_raw$Liver.Diagnosis_fctr, levels = c("Hep C", "Hep B", "PSC/PBC/AHA", "Alcohol", "Other")) 

mydata_raw$Rejection.graft.dysfunction_fctr <- factor(mydata_raw$Rejection.graft.dysfunction, levels = c(0, 1), labels = c("N", "Y")) 

mydata_raw$Recurrence.of.disease_fctr  <- factor(mydata_raw$Recurrence.of.disease, levels = c(0, 1), labels = c("N", "Y")) 

mydata_raw$Any.fibrosis_fctr  <- factor(mydata_raw$Any.fibrosis, levels = c(0, 1), labels = c("N", "Y")) 

mydata_raw$Renal.Failure_fctr <- factor(mydata_raw$Renal.Failure, levels = c(0, 1), labels = c("N", "Y")) 

mydata_raw$Depression_fctr <- factor(mydata_raw$Depression, levels = c(0, 1), labels = c("N", "Y")) 

mydata_raw$Corticoid_fctr <- factor(mydata_raw$Corticoid, levels = c(0, 1), labels = c("N", "Y")) 

mydata_fct <- mydata_raw %>%
  select(gender.fctr = Gender_fctr, age = Age, BMI, time.transplant = Time.from.transplant, 
         liver.diagnosis.fctr = Liver.Diagnosis_fctr, disease.recurrence.fctr = Recurrence.of.disease_fctr, 
         graft.rejection.dys.fctr = Rejection.graft.dysfunction_fctr, fibrosis.fctr = Any.fibrosis_fctr, renal.failure.fctr = Renal.Failure_fctr, 
         depression.fctr = Depression_fctr, corticoid.fctr = Corticoid_fctr, epworth.sleep.scale = Epworth.Sleepiness.Scale, 
         pittsburgh.quality.score = Pittsburgh.Sleep.Quality.Index.Score, athens.insomnia.scale = Athens.Insomnia.Scale, 
         berlin.sleep.scale = Berlin.Sleepiness.Scale, SF36.PCS, SF36.MCS)





