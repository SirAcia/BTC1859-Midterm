# BTC1859 Team project
# Mikael Goutama, Zachery Chan, Mausam Vadakkayil
# Github Repo: https://github.com/SirAcia/BTC1859-Midterm.git
# Code dated to: Aug 9, 2024 

# --------------------------------------------------------------------------------

##########################
##### Libraries ##########
##########################

library(funModeling) 
library(Hmisc)
library(tidyverse)
library(mice)
library(ISLR)
library(MASS)
library(dplyr)
library(car)
library(corrplot)

# --------------------------------------------------------------------------------

##########################
### Functions ############
##########################

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

# Function for calculating predictor limit for regression models
pred_limit <- function(data, column_name) {
  # Get the specific column from the data
  column_data <- data[[column_name]]
  
  # Count the number of cases in each class
  class_1 <- sum(column_data == 1)
  class_2 <- sum(column_data == 0)
  
  # Calculate the prediction limit based on the smaller class, using p < m/15 rule of thumb 
  if (class_1 < class_2) {
    pred_limit_value <- class_1 / 15
  } else {
    pred_limit_value <- class_2 / 15
  }
  
  # Returning prediction limit
  return(pred_limit_value)
}


# --------------------------------------------------------------------------------

##########################
### Reading Data #########
##########################

# Reading raw data into data frame 
mydata <- read.csv("./project_data.csv")

# It is perceived that patients with liver transplant have problems with sleep. 
# In addition, it is hypothesized that sleep disturbance has a direct effect on Quality of Life. 

# The objective of this project is the investigation of these hypotheses with 
# the use of an observational dataset of 268 patients with liver transplant. 

# Subsetting original data to get raw data for variables of interest 
mydata_raw <- subset(mydata, , c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease",
                                  "Rejection.graft.dysfunction", "Any.fibrosis",
                                  "Renal.Failure", "Depression", "Corticoid",
                                  "Epworth.Sleepiness.Scale", "Pittsburgh.Sleep.Quality.Index.Score",
                                  "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                                  "SF36.PCS", "SF36.MCS"))


# --------------------------------------------------------------------------------

###################################################
### Descriptive & Exploratory Analysis ############
###################################################

# Examining structure of raw data 
str(mydata_raw)

glimpse(mydata_raw)

basic_eda(mydata_raw)
# No zeroes in gender, coded as 1s and 2s 

# Checking for any duplicates 
duplicated(mydata_raw) 
# No duplicates 

empty_string(mydata_raw)
# No blank strings (makes sense as all data is in numeric form), 
# 268 obs TOTAL 

# No extreme outliers in age or BMI (only at 70, lowest is at 18 --> ONLY ADULTS)

# Only age, BMI, and time from transplant is considered continous, rest is categorical 
# NAs in Age (2), BMI(23), Epworth (17), Pitt(85), Athens (6), Berlin (6)
# SF36(21), SF36(21) 

# No extreme outliers in age or BMI (only at 70, lowest is at 18 --> ONLY ADULTS)

# Largest concern is NAs in Pittsburgh scale data
Pitts_NA <- mydata_raw %>%
  dplyr::filter(is.na(mydata_raw$Pittsburgh.Sleep.Quality.Index.Score))

# Using summary to compare overall statistics between NAs in Pittsburgh and overall data 
summary(Pitts_NA)

summary(mydata_raw)
# Overall no obvious concerns with NAs, seem to match overall trends and similar IQRs & mean 
# BUT NO renal failure seen in Pittsburgh NAs, this may indicate missing at random (MAR) data
# without further information on the context behind data collection, hard to determine type of
# missingness but the lack of renal failure may suggest missingness due to unseen, observable 
# variables. 

PSQI_NA_renal <- mydata_raw %>%
  filter(is.na(Pittsburgh.Sleep.Quality.Index.Score)) %>%
  count(Renal.Failure)

PSQI_NA_renal

# Seeing if this is the case for other scales as well 
ESS_NA_renal <- mydata_raw %>%
  filter(is.na(Epworth.Sleepiness.Scale)) %>%
  count(Renal.Failure)

ESS_NA_renal

AIS_NA_renal <- mydata_raw %>%
  filter(is.na(Athens.Insomnia.Scale)) %>%
  count(Renal.Failure)

AIS_NA_renal

BSS_NA_renal <- mydata_raw %>%
  filter(is.na(Berlin.Sleepiness.Scale)) %>%
  count(Renal.Failure)

BSS_NA_renal 
# No renal failure for any of the missing values for any of the clinical scales 
# This systematic missingness may be due to unseen variables, unlikely MNAR as values are 0 or 1

# LEGEND FOR CATEGORICAL (FROM DATA DICTIONARY)
#' - Gender: 1 = M, 2 = F 
#' - Liver Diagnosis:  1 = Hep C, 
#'                     2 = Hep B, 
#'                     3 = PSC/PBC/AHA, 
#'                     4 = alcohol, 
#'                     5 = other NASH/Hepatoma, Biliary atresia, 
#'                         Tyrosenemia/congenital/neonatal/AAT/Wilsons, HCC, FHF, 
#'                         cryptogenic, Angiosarcoma/tumor
#' - Recurrence of original disease process: 1 = yes, 0 = no
#' - Evidence of rejection/graft dysfunction either as evidenced by pathology 
#'   or by clinical judgement based on treating hepatologist: 1 = yes, 0 = no
#' - Evidence of fibrosis grade A2 and higher: 1 = yes, 0 = no
#' - Renal failure: 1 = yes, 0 = no
#' - Depression 1 = yes, 0 = no
#' - Corticosteroid: 1 = yes, 0 = no

# Setting gender as 1 = F, 0 = M, to match the rest of the data 
mydata_raw$Gender <- ifelse(mydata_raw$Gender == "2", 1, 0)


# --------------------------------------------------------------------------------

##########################
### Imputation ###########
##########################

#Using stochastic imputation as presumed MAR 

# Defining the imputation methods for each column
imp_methods <- c(Gender = "",
                 Age = "norm.nob",
                 BMI = "norm.nob",
                 Time.from.transplant = "",
                 Liver.Diagnosis = "",
                 Recurrence.of.disease = "",
                 Rejection.graft.dysfunction = "",
                 Any.fibrosis = "",
                 Renal.Failure = "",
                 Depression = "",
                 Corticoid = "",
                 Epworth.Sleepiness.Scale = "norm.nob",
                 Pittsburgh.Sleep.Quality.Index.Score = "norm.nob",
                 Athens.Insomnia.Scale = "norm.nob",
                 Berlin.Sleepiness.Scale = "logreg",
                 SF36.PCS = "norm.nob",
                 SF36.MCS = "norm.nob")

# As berlin is binary, factoring as using "logreg" method for imputation 
mydata_raw$Berlin.Sleepiness.Scale <- factor(mydata_raw$Berlin.Sleepiness.Scale, levels = c(0,1))

# Performing imputation, using seed 32 for random generation 
imputed_data <- mice(mydata_raw, method = imp_methods, seed = 32, m = 1, print = FALSE)

# Extracting complete data set with imputed values
mydata_imp<- complete(imputed_data, 1)

# converting Berlin back to numeric 
mydata_imp$Berlin.Sleepiness.Scale <- as.numeric(mydata_imp$Berlin.Sleepiness.Scale) 

# Recoding Berlin to match original structure (1 = T) 
mydata_imp$Berlin.Sleepiness.Scale <- ifelse(mydata_imp$Berlin.Sleepiness.Scale == 2, 1, 0) 

# Some values in Pittsburgh are negative (previous NAs, now negative with imputation)
which(mydata_imp$Pittsburgh.Sleep.Quality.Index.Score <0)

# Recoding imputed negatives as 0 
mydata_imp$Pittsburgh.Sleep.Quality.Index.Score[mydata_imp$Pittsburgh.Sleep.Quality.Index.Score <0] <- 0

# Rounding to whole numbers for clinical scales
mydata_imp$Epworth.Sleepiness.Scale <- round(mydata_imp$Epworth.Sleepiness.Scale)
mydata_imp$Pittsburgh.Sleep.Quality.Index.Score <- round(mydata_imp$Pittsburgh.Sleep.Quality.Index.Score)
mydata_imp$Athens.Insomnia.Scale <- round(mydata_imp$Athens.Insomnia.Scale)

# Exploratory analysis post-imputation
glimpse(mydata_imp)

anyNA(mydata_imp)
# No NAs anymore


# --------------------------------------------------------------------------------

#################################
### Numerical Dataset ###########
#################################

# Saving dataset with numerical variables for correlation analysis
mydata_num <- mydata_imp %>%
  dplyr::select(gender = Gender, age = Age, BMI, time.transplant = Time.from.transplant, 
                liver.diagnosis = Liver.Diagnosis, disease.recurrence = Recurrence.of.disease, 
                graft.rejection.dys = Rejection.graft.dysfunction, fibrosis = Any.fibrosis, renal.failure = Renal.Failure, 
                depression = Depression, corticoid = Corticoid, epworth.sleep.scale = Epworth.Sleepiness.Scale, 
                pittsburgh.quality.score = Pittsburgh.Sleep.Quality.Index.Score, athens.insomnia.scale = Athens.Insomnia.Scale, 
                berlin.sleep.scale = Berlin.Sleepiness.Scale, SF36.PCS, SF36.MCS)


# --------------------------------------------------------------------------------

#################################
### Categorical Dataset #########
#################################

# Factoring variables for regression models

# Factoring gender, F = 1
mydata_imp$Gender_fctr <- factor(mydata_imp$Gender, levels = c(0, 1), labels = c("Male", "Female")) 

# Factoring liver diagnosis, setting levels to reset order of bins 
mydata_imp$Liver.Diagnosis_fctr <- ifelse(mydata_imp$Liver.Diagnosis == "1", "Hep C", 
                                     ifelse(mydata_imp$Liver.Diagnosis == "2", "Hep B", 
                                            ifelse(mydata_imp$Liver.Diagnosis == "3", "PSC/PBC/AHA", 
                                                   ifelse(mydata_imp$Liver.Diagnosis == "4", "Alcohol", "Other"))))
mydata_imp$Liver.Diagnosis_fctr <- factor(mydata_imp$Liver.Diagnosis_fctr, levels = c("Hep C", "Hep B", "PSC/PBC/AHA", "Alcohol", "Other")) 

# Factoring graft rejection, Y = 1
mydata_imp$Rejection.graft.dysfunction_fctr <- factor(mydata_imp$Rejection.graft.dysfunction, levels = c(0, 1), labels = c("N", "Y")) 

# Factoring graft rejection, Y = 1
mydata_imp$Recurrence.of.disease_fctr  <- factor(mydata_imp$Recurrence.of.disease, levels = c(0, 1), labels = c("N", "Y")) 

# Factoring fibrosis, Y = 1
mydata_imp$Any.fibrosis_fctr  <- factor(mydata_imp$Any.fibrosis, levels = c(0, 1), labels = c("N", "Y")) 

# Factoring renal failure, Y = 1
mydata_imp$Renal.Failure_fctr <- factor(mydata_imp$Renal.Failure, levels = c(0, 1), labels = c("N", "Y")) 

# Factoring depression, Y = 1
mydata_imp$Depression_fctr <- factor(mydata_imp$Depression, levels = c(0, 1), labels = c("N", "Y")) 

# Factoring corticoid, Y = 1
mydata_imp$Corticoid_fctr <- factor(mydata_imp$Corticoid, levels = c(0, 1), labels = c("N", "Y")) 

# Converting clinical testing scales into binary variables 

# For PSQI <- >4 == DISTURBANCE (1 = TRUE)
# For ESS <- >10 == DISTURBANCE (1 = TRUE)
# For AIS <- >5 == DISTURBANCE (1 = TRUE)
# For BSS <- i == 1 == DISTURBANCE (TRUE)

mydata_imp$Pittsburgh.Sleep.Quality.Index.Score <- ifelse(mydata_imp$Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0)

mydata_imp$Epworth.Sleepiness.Scale <- ifelse(mydata_imp$Epworth.Sleepiness.Scale > 10, 1, 0)

mydata_imp$Athens.Insomnia.Scale<- ifelse(mydata_imp$Athens.Insomnia.Scale > 5, 1, 0)

# Saving factored variables as new data frame
mydata_fct <- mydata_imp %>%
  dplyr::select(gender.fctr = Gender_fctr, age = Age, BMI, time.transplant = Time.from.transplant, 
         liver.diagnosis.fctr = Liver.Diagnosis_fctr, disease.recurrence.fctr = Recurrence.of.disease_fctr, 
         graft.rejection.dys.fctr = Rejection.graft.dysfunction_fctr, fibrosis.fctr = Any.fibrosis_fctr, renal.failure.fctr = Renal.Failure_fctr, 
         depression.fctr = Depression_fctr, corticoid.fctr = Corticoid_fctr, epworth.sleep.scale = Epworth.Sleepiness.Scale, 
         pittsburgh.quality.score = Pittsburgh.Sleep.Quality.Index.Score, athens.insomnia.scale = Athens.Insomnia.Scale, 
         berlin.sleep.scale = Berlin.Sleepiness.Scale, SF36.PCS, SF36.MCS)

# Exploratory analysis post-factoring (for report) 
basic_eda(mydata_fct)


# --------------------------------------------------------------------------------

#################################
### Prevalence Estimates ########
#################################

# Calculate the number of patients with poor sleep quality
psqi_poor_sleep <- length(which(mydata_num$pittsburgh.quality.score > 4))

# Calculate the total number of observations
total_psqi_observations <- length(mydata_num$pittsburgh.quality.score)

# Calculate the prevalence of sleep disturbance
psqi_prevalence_poor_sleep <- psqi_poor_sleep / total_psqi_observations

# Print the prevalence
cat("PSQI Prevalence of Sleep Disturbance:", psqi_prevalence_poor_sleep, "\n")

# ESS estimate

# Calculate the number of patients with poor sleep quality
ess_poor_sleep <- length(which(mydata_num$epworth.sleep.scale > 10))

# Calculate the total number of observations
total_ess_observations <- length(mydata_num$epworth.sleep.scale)

# Calculate the prevalence of sleep disturbance
ess_prevalence_poor_sleep <- ess_poor_sleep / total_ess_observations

# Print the prevalence
cat("ESS Prevalence of Sleep Disturbance:", ess_prevalence_poor_sleep, "\n")

# BSS estimate

# Calculate the number of patients with poor sleep quality
bss_poor_sleep <- length(which(mydata_num$berlin.sleep.scale == 1))

# Calculate the total number of observations
total_bss_observations <- length(mydata_num$berlin.sleep.scale)

# Calculate the prevalence of sleep disturbance
bss_prevalence_poor_sleep <- bss_poor_sleep / total_bss_observations

# Print the prevalence
cat("BSS Prevalence of Sleep Disturbance:", bss_prevalence_poor_sleep, "\n")

# AIS estimate

# Calculate the number of patients with poor sleep quality
ais_poor_sleep <- length(which(mydata_num$athens.insomnia.scale > 5))

# Calculate the total number of observations
total_ais_observations <- length(mydata_num$athens.insomnia.scale)

# Calculate the prevalence of sleep disturbance
ais_prevalence_poor_sleep <- ais_poor_sleep / total_ais_observations

# Print the prevalence
cat("AIS Prevalence of Sleep Disturbance:", ais_prevalence_poor_sleep, "\n")


# --------------------------------------------------------------------------------

###########################################
### Predictor Limits for Models ###########
###########################################

mydata_scales <- mydata_fct %>%
  dplyr::select(-SF36.PCS, -SF36.MCS)

# Calculating number of predictors (degrees of freedom) available for each model
pittsburgh_pred <- pred_limit(mydata_scales, "pittsburgh.quality.score")
pittsburgh_pred
# Pittsburgh model has a limit of 5.8 degrees of freedom, rounding to 6

epworth_pred <- pred_limit(mydata_scales, "epworth.sleep.scale")
epworth_pred
# Epworth model has a limit of 4.6 degrees of freedom, rounding to 5

athens_pred <- pred_limit(mydata_scales, "athens.insomnia.scale")
athens_pred 
# Athens model has a limit of 7.8 degrees of freedom, rounding to 8

berlin_pred<- pred_limit(mydata_scales, "berlin.sleep.scale")
berlin_pred
# Berlin model has a limit of 7 degrees of freedom

# Compressing liver diagnosis to 1 degree of freedom (previously 4), 
# According to literature findings, Hep C diagnosis is of specific interest 
# when it comes to sleep impacts. 

# Converting back liver diagnosis to numeric to reset factor 
mydata_scales$liver.diagnosis.fctr <- as.numeric(mydata_scales$liver.diagnosis.fctr)

# Converting back to compressed, factored variable
mydata_scales$liver.diagnosis.fctr <- ifelse(mydata_scales$liver.diagnosis.fctr == 1, 1, 0)


# --------------------------------------------------------------------------------

#####################################
### Pittsburgh Model ################
#####################################

# Making full model for Pittsburgh
pitts_model_full <- glm(pittsburgh.quality.score~age+gender.fctr+BMI+time.transplant
                   +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                   +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                   data = mydata_scales, family = "binomial")

summary(pitts_model_full)
# AIC: 309.49

# Making null model for stepwise 
pitts_model_null <- glm(pittsburgh.quality.score~1, 
                   data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
pitts_model_full.step.back <- stepAIC(pitts_model_full, direction = "backward", trace = F)

summary(pitts_model_full.step.back)
# AIC: 302.93

# Conducting stepwise forward 
pitts_model_full.step.for <- stepAIC(pitts_model_null, direction = "forward", trace = F, scope = list(upper=pitts_model_full, lower=pitts_model_null))

summary(pitts_model_full.step.for)
# Model has same predictors as backwards model
# AIC: 302.93 <- same AIC as backwards stepwise model 

# Measuring colinearity for stepwise model
vif(pitts_model_full.step.for)
# Nothing above 5, so no strong evidence for colinearity

# Literature model based on predictors of interest from literature review
pitts_model_lit <- glm(pittsburgh.quality.score~time.transplant+BMI+
                         depression.fctr+gender.fctr+liver.diagnosis.fctr,
                       data = mydata_scales, family = binomial)

summary(pitts_model_lit)
# AIC: 315.38
# Currently at 5 degrees of freedom, m/15 gives limit of 6 degrees

# Measuring colinearity for literature model
vif(pitts_model_lit)
# Nothing above 5, so no strong evidence for colinearity

# Hybridizing model with sample-specific predictors
# Including significant variables from stepwise approach, doing so because 
# we have available degrees of freedom and can reduce AIC. Adding disease recurrence as predictor
pitts_model_hybrid <- glm(pittsburgh.quality.score~time.transplant+BMI+
                         depression.fctr+gender.fctr+liver.diagnosis.fctr+disease.recurrence.fctr,
                       data = mydata_scales, family = binomial)

summary(pitts_model_hybrid)
#AIC: 306.05

# Using ANOVA to determine if hybrid model is significantly better 
anova(pitts_model_lit, pitts_model_hybrid, test = "Chisq")

# Measuring colinearity for hybrid model
vif(pitts_model_hybrid)
# Nothing above 5, so no strong evidence for colinearity

# --------------------------------------------------------------------------------

################################
### Epworth Model ##############
################################

# Making full model for Epworth
epworth_model_full <- glm(epworth.sleep.scale~age+gender.fctr+BMI+time.transplant
                        +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                        +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                        data = mydata_scales, family = "binomial")

summary(epworth_model_full)
# AIC: 307.1

# Making null model for stepwise 
epworth_model_null <- glm(epworth.sleep.scale~1, 
                        data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
epworth_model_full.step.back <- stepAIC(epworth_model_full, direction = "backward", trace = F)

summary(epworth_model_full.step.back)
#' AIC: 296.99

# Conducting stepwise forward 
epworth_model_full.step.for <- stepAIC(epworth_model_null, direction = "forward", trace = F, scope = list(upper=epworth_model_full, lower=epworth_model_null))

summary(epworth_model_full.step.for)
# Model has same predictors as backwards model
# AIC: 296.99 <- same AIC as backwards stepwise model 

# Measuring colinearity for stepwise model
vif(epworth_model_full.step.for)
# Nothing above 5, so no strong evidence for colinearity

# Literature model based on predictors of interest from literature review
epworth_model_lit <- glm(epworth.sleep.scale~time.transplant+BMI+
                           depression.fctr+gender.fctr+liver.diagnosis.fctr,
                         data = mydata_scales, family = binomial)

summary(epworth_model_lit)
# AIC: 304.28
# Currently at 5 degrees of freedom, m/15 gives limit of 5 degrees

# Measuring colinearity for stepwise model
vif(epworth_model_lit)
# Nothing above 5, so no strong evidence for colinearity

# Hybridizing model with sample-specific predictors
# Switching high p-value predictor, transplant time, with corticoid as we have hit limit
epworth_model_hybrid <- glm(epworth.sleep.scale~corticoid.fctr+BMI+
                           depression.fctr+gender.fctr+liver.diagnosis.fctr,
                         data = mydata_scales, family = binomial)

summary(epworth_model_hybrid)
# AIC: 299.74

# Measuring colinearity for stepwise model
vif(epworth_model_hybrid)
# Nothing above 5, so no strong evidence for colinearity


# --------------------------------------------------------------------------------

###############################
### Athens Model ##############
###############################

# Making full model for Athens
athens_model_full <- glm(athens.insomnia.scale~age+gender.fctr+BMI+time.transplant
                          +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                          +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                          data = mydata_scales, family = "binomial")

summary(athens_model_full)
# AIC: 366.67

# Making null model for stepwise 
athens_model_null <- glm(athens.insomnia.scale~1, 
                          data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
athens_model_full.step.back <- stepAIC(athens_model_full, direction = "backward", trace = F)

summary(athens_model_full.step.back)
# AIC: 358.9

# Conducting stepwise forward 
athens_model_full.step.for <- stepAIC(athens_model_null, direction = "forward", trace = F, scope = list(upper=athens_model_full, lower=athens_model_null))

summary(athens_model_full.step.for)
# Model has same predictors as backwards model
# AIC: 358.9 <- same AIC as backwards stepwise model 

# Measuring colinearity for stepwise model
vif(athens_model_full.step.for)
# Nothing above 5, so no strong evidence for colinearity

# Literature model based on predictors of interest from literature review
athens_model_lit <- glm(athens.insomnia.scale~time.transplant+BMI+
                           depression.fctr+gender.fctr+liver.diagnosis.fctr,
                         data = mydata_scales, family = binomial)

summary(athens_model_lit)
# AIC: 366.51
# Currently at 5 degrees of freedom, m/15 gives limit of 8 degrees

# Measuring colinearity for literature model
vif(athens_model_lit)
# Nothing above 5, so no strong evidence for colinearity

# Hybridizing model with sample-specific predictors
# Including significant variables from stepwise approach, doing so because 
# we have available degrees of freedom and can reduce AIC. 
# Adding disease recurrence and corticoid as predictor
athens_model_hybrid <- glm(athens.insomnia.scale~time.transplant+BMI+
                          depression.fctr+gender.fctr+liver.diagnosis.fctr+corticoid.fctr+
                          disease.recurrence.fctr,
                          data = mydata_scales, family = binomial)


summary(athens_model_hybrid)
# AIC: 362.92

# Using ANOVA to determine if hybrid model is significantly better 
anova(athens_model_lit, athens_model_hybrid, test = "Chisq")

# Measuring colinearity for hybrid model
vif(athens_model_hybrid)
# Nothing above 5, so no strong evidence for colinearity

# --------------------------------------------------------------------------------

################################
### Berlin Model ###############
################################

# Making full model for stepwise 
berlin_model_full <- glm(berlin.sleep.scale~age+gender.fctr+BMI+time.transplant
                         +liver.diagnosis.fctr+disease.recurrence.fctr+graft.rejection.dys.fctr
                         +fibrosis.fctr+renal.failure.fctr+depression.fctr+corticoid.fctr, 
                         data = mydata_scales, family = "binomial")

summary(berlin_model_full)
# AIC: 331.34

# Making null model for stepwise 
berlin_model_null <- glm(berlin.sleep.scale~1, 
                         data = mydata_scales, family = "binomial")

# Conducting stepwise backwards 
berlin_model_full.step.back <- stepAIC(berlin_model_full, direction = "backward", trace = F)

summary(berlin_model_full.step.back)
#' AIC: 322.43

# Conducting stepwise forward 
berlin_model_full.step.for <- stepAIC(berlin_model_null, direction = "forward", trace = F, scope = list(upper=berlin_model_full, lower=berlin_model_null))

summary(berlin_model_full.step.for)
#'  AIC: 321.41
#'  Forward stepwise has lower AIC, will prefer using forward stepwise model as compared to backwards 

# Measuring colinearity for hybrid model
vif(berlin_model_full.step.for)
# Nothing above 5, so no strong evidence for colinearity

# Literature model based on predictors of interest from literature review
berlin_model_lit <- glm(berlin.sleep.scale~time.transplant+BMI+
                             depression.fctr+gender.fctr+liver.diagnosis.fctr,
                           data = mydata_scales, family = binomial)

summary(berlin_model_lit)
# AIC: 330.01
# Currently at 5 degrees of freedom, m/15 gives limit of 7 degrees

# Measuring colinearity for hybrid model
vif(berlin_model_lit)
# Nothing above 5, so no strong evidence for colinearity

# Hybridizing model with sample-specific predictors
# Including significant variables from stepwise approach, doing so because 
# we have available degrees of freedom and can reduce AIC. 
# Adding age and renal failure as predictor
berlin_model_hybrid <- glm(berlin.sleep.scale~time.transplant+BMI+
                          depression.fctr+gender.fctr+liver.diagnosis.fctr+age+renal.failure.fctr,
                        data = mydata_scales, family = binomial)

summary(berlin_model_hybrid)
#AIC: 327.28

# Using ANOVA to determine if hybrid model is significantly better 
anova(berlin_model_lit, berlin_model_hybrid, test = "Chisq")

# Measuring colinearity for hybrid model
vif(berlin_model_hybrid)
# Nothing above 5, so no strong evidence for colinearity


# --------------------------------------------------------------------------------

##########################
### Correlation ##########
##########################

# Creating data frame to calculate corr
corr_data <- mydata_num %>%
  dplyr::select(SF36.PCS, SF36.MCS, berlin.sleep.scale, epworth.sleep.scale, pittsburgh.quality.score, 
                athens.insomnia.scale)

cor_mat <- cor(corr_data)

#Plot sleep disturbance and QOL correlation
corrplot(cor_mat, type = "upper", method = "number", order = "alphabet", 
         tl.offset = 2, diag = FALSE)
mtext("Correlation of Sleep Disturbance with Quality of Life (Mental and Physical)", at=1, line=-0.1, cex=1.5)


#average SF36.PCS and SF36.MCS
mydata_num$SF36_Avg <- rowMeans(mydata_num[ ,c("SF36.PCS", "SF36.MCS")])

# Calculate correlations of SF36_avg and sleep disturbance using cor.test
cor_psqi <- cor.test(mydata_num$pittsburgh.quality.score, mydata_num$SF36_Avg, method = "pearson")
cor_ess <- cor.test(mydata_num$epworth.sleep.scale, mydata_num$SF36_Avg, method = "pearson")
cor_ais <- cor.test(mydata_num$athens.insomnia.scale, mydata_num$SF36_Avg, method = "pearson")
cor_bss <- cor.test(mydata_num$berlin.sleep.scale, mydata_num$SF36_Avg, method = "spearman")

cor_psqi

cor_ess

cor_ais

cor_bss

#plot correlation of SF36_average and sleep disturbance
corr_data_avg <- mydata_num %>%
  dplyr::select(SF36_Avg, berlin.sleep.scale, epworth.sleep.scale, pittsburgh.quality.score, 
                athens.insomnia.scale)

corr_avg <- cor(corr_data_avg)
corrplot(corr_avg, type = "upper", method = "number", order = "alphabet", 
         tl.offset = 2, diag = FALSE)
mtext("Correlation of Sleep Disturbance with Quality of Life (Average)", at=1, line=-0.1, cex=1.5)


### Plot PSQI against SF36 average, physical and mental

# Plot PSQI against SF36_Avg 
plot(mydata_num$pittsburgh.quality.score, mydata_num$SF36_Avg,
     main = "PSQI vs SF36 Average",
     xlab = "PSQI Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ pittsburgh.quality.score, data = mydata_num), col = "red")
summary(lm(SF36_Avg ~ pittsburgh.quality.score, data = mydata_num))

# PSQI against SF36.PCS
plot(mydata_num$pittsburgh.quality.score, mydata_num$SF36.PCS,
     main = "PSQI vs SF36_PCS",
     xlab = "PSQI Score",
     ylab = "SF36 PCS",
     pch = 19)
abline(lm(SF36.PCS ~ pittsburgh.quality.score, data = mydata_num), col = "red")
summary(lm(SF36.PCS ~ pittsburgh.quality.score, data = mydata_num))

# PSQI against SF36.MCS
plot(mydata_num$pittsburgh.quality.score, mydata_num$SF36.MCS,
     main = "PSQI vs SF36_MCS",
     xlab = "PSQI Score",
     ylab = "SF36 MCS",
     pch = 19)
abline(lm(SF36.MCS ~ pittsburgh.quality.score, data = mydata_num), col = "red")
summary(lm(SF36.MCS ~ pittsburgh.quality.score, data = mydata_num))


### Plot ESS against quality of life average, physical and mental

# Plot ESS against SF36_Avg 
plot(mydata_num$epworth.sleep.scale, mydata_num$SF36_Avg,
     main = "ESS vs SF36_Average",
     xlab = "ESS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ epworth.sleep.scale, data = mydata_num), col = "blue")
summary(lm(SF36_Avg ~ epworth.sleep.scale, data = mydata_num))

# Plot ESS against SF36.PCS
plot(mydata_num$epworth.sleep.scale, mydata_num$SF36.PCS,
     main = "ESS vs SF36_PCS",
     xlab = "ESS Score",
     ylab = "SF36.PCS",
     pch = 19)
abline(lm(SF36.PCS ~ epworth.sleep.scale, data = mydata_num), col = "blue")
summary(lm(SF36.PCS ~ epworth.sleep.scale, data = mydata_num))

# Plot ESS against SF36.MCS 
plot(mydata_num$epworth.sleep.scale, mydata_num$SF36.MCS,
     main = "ESS vs SF36_MCS",
     xlab = "ESS Score",
     ylab = "SF36.MCS",
     pch = 19)
abline(lm(SF36.MCS ~ epworth.sleep.scale, data = mydata_num), col = "blue")
summary(lm(SF36.MCS ~ epworth.sleep.scale, data = mydata_num))

### Plot AIS against quality of life average, physical and mental

# Plot AIS against SF36_Avg 
plot(mydata_num$athens.insomnia.scale, mydata_num$SF36_Avg,
     main = "AIS vs SF36_Average",
     xlab = "AIS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ athens.insomnia.scale, data = mydata_num), col = "green")
summary(lm(SF36_Avg ~ athens.insomnia.scale, data = mydata_num))

# Plot AIS against SF36.PCS 
plot(mydata_num$athens.insomnia.scale, mydata_num$SF36.PCS,
     main = "AIS vs SF36_PCS",
     xlab = "AIS Score",
     ylab = "SF36.PCS",
     pch = 19)
abline(lm(SF36.PCS ~ athens.insomnia.scale, data = mydata_num), col = "green")
summary(lm(SF36.PCS ~ athens.insomnia.scale, data = mydata_num))

# Plot AIS against SF36.MCS 
plot(mydata_num$athens.insomnia.scale, mydata_num$SF36.MCS,
     main = "AIS vs SF36_MCS",
     xlab = "AIS Score",
     ylab = "SF36.MCS",
     pch = 19)
abline(lm(SF36.MCS ~ athens.insomnia.scale, data = mydata_num), col = "green")
summary(lm(SF36.MCS ~ athens.insomnia.scale, data = mydata_num))

### Plot BSS against quality of life average.  

# Plot BSS against SF36_Avg 
plot(mydata_num$berlin.sleep.scale, mydata_num$SF36_Avg,
     main = "BSS vs SF36_Average",
     xlab = "BSS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ berlin.sleep.scale, data = mydata_num), col = "purple")
summary(lm(SF36_Avg ~ berlin.sleep.scale, data = mydata_num))

# Get the linear regression summary for the PCS and MCS quality of life
summary(lm(SF36.PCS ~ berlin.sleep.scale, data = mydata_num))
summary(lm(SF36.MCS ~ berlin.sleep.scale, data = mydata_num))

# Box plot for BSS vs SF36 Average
boxplot(SF36_Avg ~ berlin.sleep.scale, data = mydata_num,
        main = "BSS vs SF36_Average",
        xlab = "BSS (0 = No Sleep Disturbance, 1 = Sleep Disturbance)",
        ylab = "SF36 Average",
        col = c("lightblue", "lightgreen"))

# Box plot for BSS vs SF36 PCS
boxplot(SF36.PCS ~ berlin.sleep.scale, data = mydata_num,
        main = "BSS vs SF36.PCS",
        xlab = "BSS (0 = No Sleep Disturbance, 1 = Sleep Disturbance)",
        ylab = "SF36 PCS",
        col = c("lightblue", "lightgreen"))

# Box plot for BSS vs SF36 MCS
boxplot(SF36.MCS ~ berlin.sleep.scale, data = mydata_num,
        main = "BSS vs SF36.MCS",
        xlab = "BSS (0 = No Sleep Disturbance, 1 = Sleep Disturbance)",
        ylab = "SF36 MCS",
        col = c("lightblue", "lightgreen"))


# Measuring differences in mean for PSQI 
# Storing observations which are clinically defined as sleep disturbed by PSQI 
PSQI_yes <- mydata_num %>%
  dplyr::filter(mydata_num$pittsburgh.quality.score > 4) 

# Number of sleep disturbed according to PSQI
PSQI_yes_count <- length(PSQI_yes$SF36_Avg)

# Storing observations which are NOT clinically defined as sleep disturbed by PSQI 
PSQI_no <- mydata_num %>%
  dplyr::filter(mydata_num$pittsburgh.quality.score <= 4)

# Number of NOT sleep disturbed according to PSQI
PSQI_no_count <- length(PSQI_no$SF36_Avg)

# Calculating means and st devs for each scale (SF36.PCS, SF36.MCS, SF36.AVG) for sub-group (PSQI-yes / PSQI-no)
PSQI_yes_avg_PCS <- mean(PSQI_yes$SF36.PCS)
PSQI_yes_stdev_PCS <- sd(PSQI_yes$SF36.PCS)

PSQI_no_avg_PCS <- mean(PSQI_no$SF36.PCS)
PSQI_no_stdev_PCS <- sd(PSQI_no$SF36.PCS)


PSQI_yes_avg_MCS <- mean(PSQI_yes$SF36.MCS)
PSQI_yes_stdev_MCS <- sd(PSQI_yes$SF36.MCS)

PSQI_no_avg_MCS <- mean(PSQI_no$SF36.MCS)
PSQI_no_stdev_MCS <- sd(PSQI_no$SF36.MCS)

PSQI_avg_avg_yes <- mean(PSQI_yes$SF36_Avg)
PSQI_avg_yes_stdev <- sd(PSQI_yes$SF36_Avg)

PSQI_avg_avg_no <- mean(PSQI_no$SF36_Avg)
PSQI_avg_no_stdev  <- sd(PSQI_no$SF36_Avg)

# Conducting t-tests to determine if there is a statistically significant 
# difference in the mean quality of life for each sub-group
t.test(PSQI_yes$SF36.PCS, PSQI_no$SF36.PCS, alternative = "two.sided")

t.test(PSQI_yes$SF36.MCS, PSQI_no$SF36.MCS, alternative = "two.sided")

t.test(PSQI_yes$SF36_Avg, PSQI_no$SF36_Avg, alternative = "two.sided")

# Measuring differences in mean for ESS 
# Storing observations which are clinically defined as sleep disturbed by ESS
ESS_yes <- mydata_num %>%
  dplyr::filter(mydata_num$epworth.sleep.scale > 10)

# Number of sleep disturbed according to ESS
ESS_yes_count <- length(ESS_yes$SF36_Avg)

# Storing observations which are NOT clinically defined as sleep disturbed by ESS
ESS_no <- mydata_num %>%
  dplyr::filter(mydata_num$epworth.sleep.scale <= 10)

# Number of NOT sleep disturbed according to ESS
ESS_no_count <- length(ESS_no$SF36_Avg)

# Calculating means for each scale (SF36.PCS, SF36.MCS, SF36.AVG) for sub-group (ESS-yes / ESS-no)
ESS_yes_avg_PCS <- mean(ESS_yes$SF36.PCS)
ESS_yes_stdev_PCS <- sd(ESS_yes$SF36.PCS)

ESS_no_avg_PCS <- mean(ESS_no$SF36.PCS)
ESS_no_stdev_PCS <- sd(ESS_no$SF36.PCS)

ESS_yes_avg_MCS <- mean(ESS_yes$SF36.MCS)
ESS_yes_stdev_MCS <- sd(ESS_yes$SF36.MCS)

ESS_no_avg_MCS <- mean(ESS_no$SF36.MCS)
ESS_no_stdev_MCS <- sd(ESS_no$SF36.MCS)

ESS_avg_avg_yes <- mean(ESS_yes$SF36_Avg)
ESS_avg_stdev_yes <- sd(ESS_yes$SF36_Avg)

ESS_avg_avg_no <- mean(ESS_no$SF36_Avg)
ESS_avg_stdev_no <- sd(ESS_no$SF36_Avg)

# Conducting t-tests to determine if there is a statistically significant 
# difference in the mean quality of life for each sub-group
t.test(ESS_yes$SF36.PCS, ESS_no$SF36.PCS, alternative = "two.sided")

t.test(ESS_yes$SF36.MCS, ESS_no$SF36.MCS, alternative = "two.sided")

t.test(ESS_yes$SF36_Avg, ESS_no$SF36_Avg, alternative = "two.sided")

# Measuring differences in mean for AIS
# Storing observations which are clinically defined as sleep disturbed by AIS
AIS_yes <- mydata_num %>%
  dplyr::filter(mydata_num$athens.insomnia.scale > 5)

# Number of sleep disturbed according to AIS
AIS_yes_count <- length(AIS_yes$SF36_Avg)

# Storing observations which are NOT clinically defined as sleep disturbed by AIS
AIS_no <- mydata_num %>%
  dplyr::filter(mydata_num$athens.insomnia.scale <= 5)

# Number of NOT sleep disturbed according to AIS
AIS_no_count <- length(AIS_no$SF36_Avg)

# Calculating means for each scale (SF36.PCS, SF36.MCS, SF36.AVG) for sub-group (AIS-yes / AIS-no)
AIS_yes_avg_PCS <- mean(AIS_yes$SF36.PCS)
AIS_yes_stdev_PCS <- sd(AIS_yes$SF36.PCS)

AIS_no_avg_PCS <- mean(AIS_no$SF36.PCS)
AIS_no_stdev_PCS <- sd(AIS_no$SF36.PCS)

AIS_yes_avg_MCS <- mean(AIS_yes$SF36.MCS)
AIS_yes_stdev_MCS <- sd(AIS_yes$SF36.MCS)

AIS_no_avg_MCS <- mean(AIS_no$SF36.MCS)
AIS_no_stdev_MCS <- sd(AIS_no$SF36.MCS)

AIS_avg_avg_yes <- mean(AIS_yes$SF36_Avg)
AIS_avg_stdev_yes <- sd(AIS_yes$SF36_Avg)

AIS_avg_avg_no <- mean(AIS_no$SF36_Avg)
AIS_avg_stdev_no <- sd(AIS_no$SF36_Avg)

# Conducting t-tests to determine if there is a statistically significant 
# difference in the mean quality of life for each sub-group
t.test(AIS_yes$SF36.PCS, AIS_no$SF36.PCS, alternative = "two.sided")

t.test(AIS_yes$SF36.MCS, AIS_no$SF36.MCS, alternative = "two.sided")

t.test(AIS_yes$SF36_Avg, AIS_no$SF36_Avg, alternative = "two.sided")

# Measuring differences in mean for BSS
# Storing observations which are clinically defined as sleep disturbed by BSS
BSS_yes <- mydata_num %>%
  dplyr::filter(mydata_num$berlin.sleep.scale == 1)

# Number of sleep disturbed according to BSS
BSS_yes_count <- length(BSS_yes$SF36_Avg)

# Storing observations which are NOT clinically defined as sleep disturbed by BSS
BSS_no <- mydata_num %>%
  dplyr::filter(mydata_num$berlin.sleep.scale == 0)

# Number of NOT sleep disturbed according to AIS
BSS_no_count <- length(BSS_no$SF36_Avg)

# Calculating means for each scale (SF36.PCS, SF36.MCS, SF36.AVG) for sub-group (BSS-yes / BSS-no)
BSS_yes_avg_PCS <- mean(BSS_yes$SF36.PCS)
BSS_yes_stdev_PCS <- sd(BSS_yes$SF36.PCS)

AIS_no_avg_PCS <- mean(BSS_no$SF36.PCS)
AIS_no_stdev_PCS <- sd(BSS_no$SF36.PCS)

BSS_yes_avg_MCS <- mean(BSS_yes$SF36.MCS)
BSS_yes_stdev_MCS <- sd(BSS_yes$SF36.MCS)

BSS_no_avg_MCS <- mean(BSS_no$SF36.MCS)
BSS_no_stdev_MCS <- sd(BSS_no$SF36.MCS)

BSS_avg_avg_yes <- mean(BSS_yes$SF36_Avg)
BSS_avg_stdev_yes <- sd(BSS_yes$SF36_Avg)

BSS_avg_avg_no <- mean(BSS_no$SF36_Avg)
BSS_avg_stdev_no <- sd(BSS_no$SF36_Avg)

# Conducting t-tests to determine if there is a statistically significant 
# difference in the mean quality of life for each sub-group
t.test(BSS_yes$SF36.PCS, BSS_no$SF36.PCS, alternative = "two.sided")

t.test(BSS_yes$SF36.MCS, BSS_no$SF36.MCS, alternative = "two.sided")

t.test(BSS_yes$SF36_Avg, BSS_no$SF36_Avg, alternative = "two.sided")




