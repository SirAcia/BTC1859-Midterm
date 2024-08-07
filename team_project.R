# Team project

mydata <- read.csv("./project_data.csv")

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
library(tidyverse)

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



basic_eda(mydata_fct)

# --------------------------------------------------------------------------------

# PSQI estimate

# Calculate the number of patients with poor sleep quality
psqi_poor_sleep <- length(which(mydata_fct$pittsburgh.quality.score > 4))

# Calculate the total number of observations
total_psqi_observations <- length(which(!is.na(mydata_fct$pittsburgh.quality.score)))

# Calculate the prevalence of sleep disturbance
psqi_prevalence_poor_sleep <- psqi_poor_sleep / total_psqi_observations

# Print the prevalence
cat("PSQI Prevalence of Sleep Disturbance:", psqi_prevalence_poor_sleep, "\n")

# ESS estimate

# Calculate the number of patients with poor sleep quality
ess_poor_sleep <- length(which(mydata_fct$epworth.sleep.scale > 10))

# Calculate the total number of observations
total_ess_observations <- length(which(!is.na(mydata_fct$epworth.sleep.scale)))

# Calculate the prevalence of sleep disturbance
ess_prevalence_poor_sleep <- ess_poor_sleep / total_ess_observations

# Print the prevalence
cat("ESS Prevalence of Sleep Disturbance:", ess_prevalence_poor_sleep, "\n")

# BSS estimate

# Calculate the number of patients with poor sleep quality
bss_poor_sleep <- length(which(mydata_fct$berlin.sleep.scale == 1))

# Calculate the total number of observations
total_bss_observations <- length(which(!is.na(mydata_fct$berlin.sleep.scale)))

# Calculate the prevalence of sleep disturbance
bss_prevalence_poor_sleep <- bss_poor_sleep / total_bss_observations

# Print the prevalence
cat("BSS Prevalence of Sleep Disturbance:", bss_prevalence_poor_sleep, "\n")

# AIS estimate

# Calculate the number of patients with poor sleep quality
ais_poor_sleep <- length(which(mydata_fct$athens.insomnia.scale > 5))

# Calculate the total number of observations
total_ais_observations <- length(which(!is.na(mydata_fct$athens.insomnia.scale)))

# Calculate the prevalence of sleep disturbance
ais_prevalence_poor_sleep <- ais_poor_sleep / total_ais_observations

# Print the prevalence
cat("AIS Prevalence of Sleep Disturbance:", ais_prevalence_poor_sleep, "\n")

# --------------------------------------------------------------------------------

# Converting clinical testing scales into binary variables 

# For PSQI <- >4 == DISTURBANCE (1 = TRUE)
# For ESS <- >10 == DISTURBANCE (1 = TRUE)
# For AIS <- >5 == DISTURBANCE (TRUE)
# For BSS <- == 1 == DISTURBANCE (TRUE)

mydata_fct$pittsburgh.quality.score <- ifelse(mydata_fct$pittsburgh.quality.score > 4, 1, 0)

mydata_fct$epworth.sleep.scale <- ifelse(mydata_fct$epworth.sleep.scale > 10, 1, 0)

mydata_fct$athens.insomnia.scale <- ifelse(mydata_fct$athens.insomnia.scale > 5, 1, 0)


# --------------------------------------------------------------------------------

# Making model dataset
mydata_fct3 <- na.omit(mydata_fct)


#libraries for stepwise 
library(ISLR)
library(MASS)
library(dplyr)

mydata_fct2 <- mydata_fct %>%
  dplyr::select(-SF36.PCS, -SF36.MCS)

# Omitting NAs (check) 
mydata_scales <- na.omit(mydata_fct2)

##########################
### PITTS ################
##########################

# Making full model for stepwise 
pitts_model_full <- glm(pittsburgh.quality.score~age+gender.fctr+BMI+time.transplant
                   +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                   +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                   data = mydata_scales, family = "binomial")

summary(pitts_model_full)

# Making null model for stepwise 
pitts_model_null <- glm(pittsburgh.quality.score~1, 
                   data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
pitts_model_full.step.back <- stepAIC(pitts_model_full, direction = "backward", trace = F)

summary(pitts_model_full.step.back)
#'glm(formula = pittsburgh.quality.score ~ age + gender.fctr + 
#'disease.recurrence.fctr + renal.failure.fctr + depression.fctr, 
#'family = "binomial", data = mydata_scales)

# Conducting stepwise both
pitts_model_full.step.both <- stepAIC(pitts_model_full, direction = "both", trace = F)

summary(pitts_model_full.step.both)
#'glm(formula = pittsburgh.quality.score ~ age + gender.fctr + 
#'disease.recurrence.fctr + renal.failure.fctr + depression.fctr, 
#'family = "binomial", data = mydata_scales)
#'
# Conducting stepwise forward 
pitts_model_full.step.for <- stepAIC(pitts_model_null, direction = "forward", trace = F, scope = list(upper=pitts_model_full, lower=pitts_model_null))

summary(pitts_model_full.step.for)
#'  glm(formula = pittsburgh.quality.score ~ depression.fctr + age + 
#'   disease.recurrence.fctr + gender.fctr + renal.failure.fctr, 
#'   family = "binomial", data = mydata_scales)"

# manual/literature models
pitts_model_lit <- glm(pittsburgh.quality.score~time.transplant+BMI+
                         depression.fctr+gender.fctr+disease.recurrence.fctr+
                         graft.rejection.dys.fctr+renal.failure.fctr+
                         fibrosis.fctr+corticoid.fctr, data = mydata_scales, 
                       family = binomial)

summary(pitts_model_lit)

# --------------------------------------------------------------------------------

##########################
### Epworth ##############
##########################

# Making full model for stepwise (Epworth)
epworth_model_full <- glm(epworth.sleep.scale~age+gender.fctr+BMI+time.transplant
                        +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                        +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                        data = mydata_scales, family = "binomial")

summary(epworth_model_full)

# Making null model for stepwise 
epworth_model_null <- glm(epworth.sleep.scale~1, 
                        data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
epworth_model_full.step.back <- stepAIC(epworth_model_full, direction = "backward", trace = F)

summary(epworth_model_full.step.back)
#' glm(formula = epworth.sleep.scale ~ liver.diagnosis.fctr + renal.failure.fctr + 
#' corticoid.fctr, family = "binomial", data = mydata_scales)

# Conducting stepwise both
epworth_model_full.step.both <- stepAIC(epworth_model_full, direction = "both", trace = F)

summary(epworth_model_full.step.both)
#' glm(formula = epworth.sleep.scale ~ liver.diagnosis.fctr + renal.failure.fctr + 
#' corticoid.fctr, family = "binomial", data = mydata_scales)


# Conducting stepwise forward 
epworth_model_full.step.for <- stepAIC(epworth_model_null, direction = "forward", trace = F, scope = list(upper=epworth_model_full, lower=epworth_model_null))

summary(epworth_model_full.step.for)
#'  glm(formula = epworth.sleep.scale ~ corticoid.fctr + liver.diagnosis.fctr + 
#'  renal.failure.fctr, family = "binomial", data = mydata_scales)


# manual/literature models
epworth_model_lit <- glm(epworth.sleep.scale~time.transplant+BMI+
                         depression.fctr+gender.fctr+disease.recurrence.fctr+
                         graft.rejection.dys.fctr+renal.failure.fctr+
                         fibrosis.fctr+corticoid.fctr, data = mydata_scales, 
                       family = binomial)

summary(epworth_model_lit)
# --------------------------------------------------------------------------------


##########################
### Athens ##############
##########################

# Making full model for stepwise (Epworth)
athens_model_full <- glm(athens.insomnia.scale~age+gender.fctr+BMI+time.transplant
                          +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                          +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                          data = mydata_scales, family = "binomial")

summary(athens_model_full)

# Making null model for stepwise 
athens_model_null <- glm(athens.insomnia.scale~1, 
                          data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
athens_model_full.step.back <- stepAIC(athens_model_full, direction = "backward", trace = F)

summary(athens_model_full.step.back)
#' glm(formula = athens.insomnia.scale ~ age + gender.fctr + liver.diagnosis.fctr + 
#' depression.fctr + corticoid.fctr, family = "binomial", data = mydata_scales)

# Conducting stepwise both
athens_model_full.step.both <- stepAIC(athens_model_full, direction = "both", trace = F)

summary(athens_model_full.step.both)
#' glm(formula = athens.insomnia.scale ~ age + gender.fctr + liver.diagnosis.fctr + 
#' depression.fctr + corticoid.fctr, family = "binomial", data = mydata_scales)


# Conducting stepwise forward 
athens_model_full.step.for <- stepAIC(athens_model_null, direction = "forward", trace = F, scope = list(upper=athens_model_full, lower=athens_model_null))

summary(athens_model_full.step.for)
#'  glm(formula = athens.insomnia.scale ~ liver.diagnosis.fctr + 
#'  depression.fctr + corticoid.fctr + age + gender.fctr, family = "binomial", 
#'  data = mydata_scales)

# --------------------------------------------------------------------------------

##########################
### Berlin ###############
##########################

# Making full model for stepwise (Epworth)
berlin_model_full <- glm(berlin.sleep.scale~age+gender.fctr+BMI+time.transplant
                         +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                         +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                         data = mydata_scales, family = "binomial")

summary(berlin_model_full)

# Making null model for stepwise 
berlin_model_null <- glm(berlin.sleep.scale~1, 
                         data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
berlin_model_full.step.back <- stepAIC(berlin_model_full, direction = "backward", trace = F)

summary(berlin_model_full.step.back)
#' glm(formula = berlin.sleep.scale ~ BMI + renal.failure.fctr, 
#' family = "binomial", data = mydata_scales)

# Conducting stepwise both
berlin_model_full.step.both <- stepAIC(berlin_model_full, direction = "both", trace = F)

summary(berlin_model_full.step.both)
#' glm(formula = berlin.sleep.scale ~ BMI + renal.failure.fctr, 
#' family = "binomial", data = mydata_scales)


# Conducting stepwise forward 
berlin_model_full.step.for <- stepAIC(berlin_model_null, direction = "forward", trace = F, scope = list(upper=berlin_model_full, lower=berlin_model_null))

summary(berlin_model_full.step.for)
#'  glm(formula = berlin.sleep.scale ~ BMI + renal.failure.fctr, 
#'  family = "binomial", data = mydata_scales)

# manual/literature models
berlin_model_lit <- glm(berlin.sleep.scale~time.transplant+BMI+
                           depression.fctr+gender.fctr+disease.recurrence.fctr+
                           graft.rejection.dys.fctr+renal.failure.fctr+
                           fibrosis.fctr+corticoid.fctr, data = mydata_scales, 
                         family = binomial)

summary(berlin_model_lit)

anova()


# --------------------------------------------------------------------------------

##########################
### Correlation ##########
##########################

# Creating data frame to calculate corr
corr_data <- mydata_num %>%
  dplyr::select(SF36.PCS, SF36.MCS, berlin.sleep.scale, epworth.sleep.scale, pittsburgh.quality.score, 
                athens.insomnia.scale)

corr_data_2 <- na.omit(corr_data)

cor(corr_data_2)
corrplot(corr_data_2)



# manual/literature models
pitts_model_lit <- glm(pittsburgh.quality.score~time.transplant+BMI+
                         depression.fctr+gender.fctr+disease.recurrence.fctr+
                         graft.rejection.dys.fctr+renal.failure.fctr+
                         fibrosis.fctr+corticoid.fctr, data = mydata_scales, 
                         family = binomial)

# Literature relevant predictors
pitts_model <- glm(pittsburgh.quality.score~time.transplant+BMI+depression.fctr+gender.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr+renal.failure.fctr+fibrosis.fctr+corticoid.fctr, data = mydata_fct3, family = binomial)

# All predictors
all_pitts <- glm(pittsburgh.quality.score~age+liver.diagnosis.fctr+time.transplant+BMI+depression.fctr+gender.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr+renal.failure.fctr+fibrosis.fctr+corticoid.fctr3, data = mydata_fct, family = binomial)

# significant predictors from all_pitts
relevant_pitts <- glm(pittsburgh.quality.score~age+depression.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr, data = mydata_fct3, family = binomial)

# stepAIC model
aic_pitts <- glm(pittsburgh.quality.score~age+depression.fctr+disease.recurrence.fctr+renal.failure.fctr+gender.fctr, data = mydata_fct3, family = binomial)

summary(pitts_model)

summary(all_pitts)

summary(relevant_pitts)

summary(aic_pitts)
