# BTC1859 Team project
# Mikael Goutama, Zachery Chan, Mausam Vadakkayil

library(funModeling) 
library(Hmisc)
library(tidyverse)
library(mice)
library(ISLR)
library(MASS)
library(dplyr)

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

# Examining structure of raw data 
str(mydata_raw)

glimpse(mydata_raw)

basic_eda(mydata_raw)
# No zeroes in gender 

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

# Define the imputation methods for each column
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

# Perform imputation
imputed_data <- mice(mydata_raw, method = imp_methods, seed = 32, m = 1, print = FALSE)

# Extract the complete data set with imputed values
mydata_raw<- complete(imputed_data, 1)

# Rounding to whole numbers for clinical scales
mydata_raw$Epworth.Sleepiness.Scale <- round(mydata_raw$Epworth.Sleepiness.Scale)
mydata_raw$Pittsburgh.Sleep.Quality.Index.Score <- round(mydata_raw$Pittsburgh.Sleep.Quality.Index.Score)
mydata_raw$Athens.Insomnia.Scale <- round(mydata_raw$Athens.Insomnia.Scale)

# Binary values for Berlin.Sleepiness.Scale
mydata_raw$Berlin.Sleepiness.Scale <- ifelse(mydata_raw$Berlin.Sleepiness.Scale >= 0.5, 1, 0)

view(mydata_raw)

basic_eda(mydata_raw)

mydata_num <- mydata_raw %>%
    dplyr::select(gender = Gender, age = Age, BMI, time.transplant = Time.from.transplant, 
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
  dplyr::select(gender.fctr = Gender_fctr, age = Age, BMI, time.transplant = Time.from.transplant, 
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

#' the stepAIC methods have lower AIC compared to the literature

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

#' the stepAIC methods have lower AIC compared to the literature

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

# manual/literature models
athens_model_lit <- glm(athens.insomnia.scale~time.transplant+BMI+
                           depression.fctr+gender.fctr+disease.recurrence.fctr+
                           graft.rejection.dys.fctr+renal.failure.fctr+
                           fibrosis.fctr+corticoid.fctr, data = mydata_scales, 
                         family = binomial)

summary(athens_model_lit)

#' the stepAIC methods have lower AIC compared to the literature

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

#' the stepAIC methods have lower AIC compared to the literature

# --------------------------------------------------------------------------------

##########################
### Correlation ##########
##########################

library(corrplot)

# Creating data frame to calculate corr
corr_data <- mydata_num %>%
  dplyr::select(SF36.PCS, SF36.MCS, berlin.sleep.scale, epworth.sleep.scale, pittsburgh.quality.score, 
                athens.insomnia.scale)

corr_data_2 <- na.omit(corr_data)

cor_mat <- cor(corr_data_2)
corrplot(cor_mat)



mydata_num$SF36_Avg <- rowMeans(mydata_num[ ,c("SF36.PCS", "SF36.MCS")])

mydata_clean <- mydata_num %>%
  filter(!is.na(epworth.sleep.scale) &
           !is.na(pittsburgh.quality.score) &
           !is.na(athens.insomnia.scale) &
           !is.na(berlin.sleep.scale) &
           !is.na(SF36_Avg))


# Plotting the PSQI and SFF

#mydata_psqi_sf36 <- subset(mydata_num, !is.na(pittsburgh.quality.score) & !is.na(SF36_Avg), 
#select = c(pittsburgh.quality.score, SF36_Avg))

# Plot PSQI against quality of life

# Plot PSQI against SF36_Avg 
plot(mydata_clean$pittsburgh.quality.score, mydata_clean$SF36_Avg,
     main = "PSQI vs SF36 Average",
     xlab = "PSQI Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ pittsburgh.quality.score, data = mydata_clean), col = "red")

# PSQI against SF36.PCS
plot(mydata_clean$pittsburgh.quality.score, mydata_clean$SF36.PCS,
     main = "PSQI vs SF36 PCS",
     xlab = "PSQI Score",
     ylab = "SF36 PCS",
     pch = 19)
abline(lm(SF36.PCS ~ pittsburgh.quality.score, data = mydata_clean), col = "red")

# PSQI against SF36.MCS
plot(mydata_clean$pittsburgh.quality.score, mydata_clean$SF36.MCS,
     main = "PSQI vs SF36 MCS",
     xlab = "PSQI Score",
     ylab = "SF36 MCS",
     pch = 19)
abline(lm(SF36.MCS ~ pittsburgh.quality.score, data = mydata_clean), col = "red")


# Plot ESS against quality of life

# Plot ESS against SF36_Avg 
plot(mydata_clean$epworth.sleep.scale, mydata_clean$SF36_Avg,
     main = "ESS vs SF36 Average",
     xlab = "ESS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ epworth.sleep.scale, data = mydata_clean), col = "blue")

# Plot ESS against SF36.PCS
plot(mydata_clean$epworth.sleep.scale, mydata_clean$SF36.PCS,
     main = "ESS vs SF36.PCS",
     xlab = "ESS Score",
     ylab = "SF36.PCS",
     pch = 19)
abline(lm(SF36.PCS ~ epworth.sleep.scale, data = mydata_clean), col = "blue")

# Plot ESS against SF36.MCS 
plot(mydata_clean$epworth.sleep.scale, mydata_clean$SF36.MCS,
     main = "ESS vs SF36.MCS",
     xlab = "ESS Score",
     ylab = "SF36.MCS",
     pch = 19)
abline(lm(SF36.MCS ~ epworth.sleep.scale, data = mydata_clean), col = "blue")

# Plot AIS against quality of life

# Plot AIS against SF36_Avg 
plot(mydata_clean$athens.insomnia.scale, mydata_clean$SF36_Avg,
     main = "AIS vs SF36 Average",
     xlab = "AIS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ athens.insomnia.scale, data = mydata_clean), col = "green")

# Plot AIS against SF36.PCS 
plot(mydata_clean$athens.insomnia.scale, mydata_clean$SF36.PCS,
     main = "AIS vs SF36.PCS",
     xlab = "AIS Score",
     ylab = "SF36.PCS",
     pch = 19)
abline(lm(SF36.PCS ~ athens.insomnia.scale, data = mydata_clean), col = "green")

# Plot AIS against SF36.MCS 
plot(mydata_clean$athens.insomnia.scale, mydata_clean$SF36.MCS,
     main = "AIS vs SF36.MCS",
     xlab = "AIS Score",
     ylab = "SF36.MCS",
     pch = 19)
abline(lm(SF36.MCS ~ athens.insomnia.scale, data = mydata_clean), col = "green")

# Plot BSS against quality of life

# Plot BSS against SF36_Avg 
plot(mydata_clean$berlin.sleep.scale, mydata_clean$SF36_Avg,
     main = "BSS vs SF36 Average",
     xlab = "BSS Score",
     ylab = "SF36 Average",
     pch = 19)
abline(lm(SF36_Avg ~ berlin.sleep.scale, data = mydata_clean), col = "purple")

# Box plot for BSS vs SF36 Average
boxplot(SF36_Avg ~ berlin.sleep.scale, data = mydata_clean,
        main = "BSS vs SF36 Average",
        xlab = "BSS (0 = No Disturbance, 1 = Disturbance)",
        ylab = "SF36 Average",
        col = c("lightblue", "lightgreen"))

lm(SF36_Avg ~ berlin.sleep.scale+epworth.sleep.scale+pittsburgh.quality.score+athens.insomnia.scale, data = mydata_clean)


# Calculate correlations using cor.test
cor_psqi <- cor.test(mydata_clean$pittsburgh.quality.score, mydata_clean$SF36_Avg, method = "pearson")
cor_ess <- cor.test(mydata_clean$epworth.sleep.scale, mydata_clean$SF36_Avg, method = "pearson")
cor_ais <- cor.test(mydata_clean$athens.insomnia.scale, mydata_clean$SF36_Avg, method = "pearson")
cor_bss <- cor.test(mydata_clean$berlin.sleep.scale, mydata_clean$SF36_Avg, method = "pearson")

cor_psqi

cor_ess

cor_ais

cor_bss