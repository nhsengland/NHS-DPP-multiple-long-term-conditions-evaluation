# NDPP Evaluation.  Code author: Paul Chappell

#                            #
#                            #
#   Explore missing data and #
#   prepare dataset for      #
#     modelling              #

##### Load packages:

library(odbc)
library(DBI)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(cowplot)

rm(list=ls()) #clear environment

#Load outcome data from sql:

con <- DBI::dbConnect(odbc(), dsn = "NCDR", bigint = "num")

outcomes_6 <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_19_April]")
outcomes_12 <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_R_19_April]")
outcomes_18 <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_R_19_April]")
outcomes_24 <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_R_19_April]")

odbc::dbDisconnect(con)

##### Load matched sample datasets to merge back in variables derived before matching:


NDH_6 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_6mth.RDS")
NDH_12 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_12mth.RDS")
NDH_18 <- readRDS( "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_18mth.RDS")
NDH_24 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_24mth.RDS")


#### 6 Months #####
###################

#Merge sql dataset with existing dataset, sort out dates, and make Died flag:

df_1 <- outcomes_6 %>%
  mutate(NHS_Number= as.numeric(NHS_Number)) %>%
  select(-Date_Id, -new_f_date) %>%
  mutate(across(.cols = c(f_date_end_month, outcome_6_date_end_month, f_date_plus_6), .fns = as_date)) %>%
  left_join(NDH_6, by = c("NHS_Number" = "NHS_Number_Flag")) %>%
  mutate(across(.cols = c(h_date, f_date, ndh_date_valid, h_date_end_month, 
                           DATE_OF_BIRTH, DATE_OF_DEATH), .fns = as_date)) %>%
  mutate(died_in_study =ifelse(DATE_OF_DEATH < f_date_plus_6, 1, 0)) %>%
  distinct(NHS_Number, .keep_all = TRUE) %>%
  arrange(NHS_Number)
 
# testing death variable:
  
hist(df_1$DATE_OF_DEATH, breaks = 10)
hist(df_1$new_f_date, breaks = 20)
hist(df_1$f_date_plus_6, breaks=20)
table(df_1$died_in_study)


# count cases where missing data

missing_percent <- function(x) {(sum(is.na(x))/ (length(x)) *100)}
missing_n <- function(x) {sum(is.na(x))}

options(scipen=999)

apply(df_1, 2, missing_percent)
apply(df_1, 2, missing_n)

dim(df_1)

table(df_1$pop_group)


#Removing missing data on LTC combination_ID,people who died in follow-up and people who went from frail to not frail:
df_2 <- df_1 %>%
  filter(!is.na(LTC_6_Combination_Code)) %>%
  filter(!is.na(LTC_f_Combination_Code)) %>%
  filter(is.na(died_in_study) | died_in_study == 0)

dim(df_2)

table(df_2$pop_group)

#Check missings:

apply(df_2, 2, missing_percent)
apply(df_2, 2, missing_n)

#Don't allow people to leave frailty risk LTCs:

table(df_2$LTC_6_Intermediate_Frailty_Risk_HFRS)
table(df_2$LTC_6_High_Frailty_Risk_HFRS)


df_3 <- df_2 %>%
  select(-LTC_6_Combination_Code, -LTC_6_Combination_Name, -LTC_f_Combination_Code, -LTC_f_Combination_Name) %>%
  mutate(across(starts_with("LTC_6_"), ~replace_na(., replace = 0))) %>%
  mutate(across(starts_with("LTC_f_"), ~replace_na(., replace = 0))) %>%
  mutate(left_inter = LTC_6_Intermediate_Frailty_Risk_HFRS - LTC_f_Intermediate_Frailty_Risk_HFRS, #subtracting the earlier indicator variable from the later one will give a score of -1 if they moved out of frailty
         left_high = LTC_6_High_Frailty_Risk_HFRS - LTC_f_High_Frailty_Risk_HFRS, 
   LTC_6_Intermediate_Frailty_Risk_HFRS = as.numeric(LTC_6_Intermediate_Frailty_Risk_HFRS),
  LTC_6_Intermediate_Frailty_Risk_HFRS = if_else(left_inter == -1, 1, LTC_6_Intermediate_Frailty_Risk_HFRS), # if it's -1 then calssify tehm as still havign frailty 
  LTC_6_High_Frailty_Risk_HFRS = as.numeric(LTC_6_High_Frailty_Risk_HFRS),
  LTC_6_High_Frailty_Risk_HFRS = if_else(left_high == -1, 1, LTC_6_High_Frailty_Risk_HFRS))
  

table(df_3$LTC_6_Intermediate_Frailty_Risk_HFRS)
table(df_3$left_inter)
table(df_3$LTC_6_High_Frailty_Risk_HFRS)
table(df_3$left_high)
  
#calculate if people have LTCs / clusters of LTCs at f date and 6 month date:

df_4 <- df_3 %>%
  mutate(Diabetes_f = LTC_f_Diabetes,
         Diabetes_6 = LTC_6_Diabetes,
         Diabetes_New = ifelse((Diabetes_6- Diabetes_f) == 1, 1, 0),
         Cancer_f = LTC_f_Cancer,
         Cancer_6 = LTC_6_Cancer,
         Cancer_new = ifelse((Cancer_6- Cancer_f) == 1, 1, 0),
         Cardiovascular_f = LTC_f_Heart_Failure + LTC_f_Severe_Heart_Failure +
                                LTC_f_Atrial_Fibrillation + LTC_f_Cerebrovascular_Disease +
                                LTC_f_Coronary_Heart_Disease + LTC_f_Hypertension +
                                LTC_f_Peripheral_Vascular_Disease,
         Cardiovascular_6 = LTC_6_Heart_Failure + LTC_6_Severe_Heart_Failure + 
                                LTC_6_Atrial_Fibrillation + LTC_6_Cerebrovascular_Disease +
                                LTC_6_Coronary_Heart_Disease + LTC_6_Hypertension +
                                LTC_6_Peripheral_Vascular_Disease,
         Cardiovascular_new = ifelse((Cardiovascular_6 - Cardiovascular_f) >= 1, 1, 0),
         Other_concordant_f = LTC_f_Osteoarthritis +  LTC_f_Physical_Disability +  LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + LTC_f_Chronic_Liver_Disease + LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Other_concordant_6 = LTC_6_Osteoarthritis +  LTC_6_Physical_Disability +  LTC_6_Dementia + 
           LTC_6_Intermediate_Frailty_Risk_HFRS + LTC_6_High_Frailty_Risk_HFRS +
           LTC_6_Chronic_Kidney_Disease + 
           LTC_6_End_Stage_Renal_Failure + LTC_6_Chronic_Liver_Disease + LTC_6_Liver_Failure +
           LTC_6_Chronic_Pain,
         Other_concordant_new = ifelse((Other_concordant_6 - Other_concordant_f) >= 1, 1, 0),
         Discordant_f =  LTC_f_Pulmonary_Heart_Disease + LTC_f_Epilepsy + LTC_f_Serious_Mental_Illness
         + LTC_f_Osteoporosis +  LTC_f_Rheumatoid_Arthritis + LTC_f_Asthma + LTC_f_COPD +
           LTC_f_Severe_COPD + LTC_f_Bronchiectasis + LTC_f_Autism + LTC_f_Cystic_Fibrosis +
           LTC_f_Learning_Disability + LTC_f_Parkinsons_Disease + LTC_f_Neurological_Organ_Failure
         +  LTC_f_Alcohol_Dependence +  LTC_f_Inflammatory_Bowel_Disease + LTC_f_Multiple_Sclerosis
         +LTC_f_Sarcoidosis  + LTC_f_Severe_Interstitial_Lung_Disease + LTC_f_Sickle_Cell_Disease,
         Discordant_6 =  LTC_6_Pulmonary_Heart_Disease + LTC_6_Epilepsy + LTC_6_Serious_Mental_Illness
         + LTC_6_Osteoporosis +  LTC_6_Rheumatoid_Arthritis + LTC_6_Asthma + LTC_6_COPD +
           LTC_6_Severe_COPD + LTC_6_Bronchiectasis + LTC_6_Autism + LTC_6_Cystic_Fibrosis +
           LTC_6_Learning_Disability + LTC_6_Parkinsons_Disease + LTC_6_Neurological_Organ_Failure
         +  LTC_6_Alcohol_Dependence +  LTC_6_Inflammatory_Bowel_Disease + LTC_6_Multiple_Sclerosis
         +LTC_6_Sarcoidosis  + LTC_6_Severe_Interstitial_Lung_Disease + LTC_6_Sickle_Cell_Disease,
         Discordant_new_binary = ifelse((Discordant_6 - Discordant_f) >= 1, 1, 0),
         Discordant_new_count = Discordant_6 - Discordant_f,
         Concordant_f =  LTC_f_Cancer + 
           LTC_f_Heart_Failure + 
           LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation +
           LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + 
           LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease + 
           LTC_f_Osteoarthritis +  
           LTC_f_Physical_Disability +
           LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + 
           LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + 
           LTC_f_Chronic_Liver_Disease + 
           LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Concordant_6 =  LTC_6_Cancer + 
           LTC_6_Heart_Failure + 
           LTC_6_Severe_Heart_Failure +
           LTC_6_Atrial_Fibrillation +
           LTC_6_Cerebrovascular_Disease +
           LTC_6_Coronary_Heart_Disease + 
           LTC_6_Hypertension +
           LTC_6_Peripheral_Vascular_Disease + 
           LTC_6_Osteoarthritis +  
           LTC_6_Physical_Disability +
           LTC_6_Dementia + 
           LTC_6_Intermediate_Frailty_Risk_HFRS + 
           LTC_6_High_Frailty_Risk_HFRS +
           LTC_6_Chronic_Kidney_Disease + 
           LTC_6_End_Stage_Renal_Failure + 
           LTC_6_Chronic_Liver_Disease + 
           LTC_6_Liver_Failure +
           LTC_6_Chronic_Pain,
         Concordant_new_binary = ifelse((Concordant_6 - Concordant_f) >= 1, 1, 0),
         Concordant_new_count = Concordant_6 - Concordant_f)
          

#Testing:

table(df_4$Diabetes_f, df_4$Diabetes_6)
table(df_4$Diabetes_f)
table(df_4$Diabetes_6)
table(df_4$Diabetes_New)
table(df_4$Cancer_f, df_4$Cancer_6)
table(df_4$Cancer_f)
table(df_4$Cancer_6)
table(df_4$Cancer_new)
table(df_4$Cardiovascular_f)
table(df_4$Cardiovascular_6)
table(df_4$Cardiovascular_new)
table(df_4$Cardiovascular_f, df_4$Cardiovascular_6)
table(df_4$Other_concordant_f)
table(df_4$Other_concordant_6)
table(df_4$Other_concordant_new)
table(df_4$Discordant_f, df_4$Discordant_6)
table(df_4$Discordant_f)
table(df_4$Discordant_6)
table(df_4$Discordant_new_binary)
table(df_4$Discordant_new_count)
table(df_4$Concordant_f, df_4$Concordant_6)
table(df_4$Concordant_f)
table(df_4$Concordant_6)
table(df_4$Concordant_new_binary)
table(df_4$Concordant_new_count)
table(df_4$pop_group)

#Save dataset

saveRDS(df_4, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_6mth_outcomes_for_modelling.RDS")     

###################
#### 12 Months #####
###################

#Merge sql dataset with existing dataset, sort out dates, and make Died flag:

df_1 <- outcomes_12 %>%
  mutate(NHS_Number= as.numeric(NHS_Number)) %>%
  select(-Date_Id, -new_f_date) %>%
  mutate(across(.cols = c(f_date_end_month, outcome_12_date_end_month, f_date_plus_12), .fns = as_date)) %>%
  left_join(NDH_12, by = c("NHS_Number" = "NHS_Number_Flag")) %>%
  mutate(across(.cols = c(h_date, f_date, ndh_date_valid, h_date_end_month, 
                          DATE_OF_BIRTH, DATE_OF_DEATH), .fns = as_date)) %>%
  mutate(died_in_study =ifelse(DATE_OF_DEATH < f_date_plus_12, 1, 0)) %>%
  distinct(NHS_Number, .keep_all = TRUE) %>%
  arrange(NHS_Number)

#testing death variable

hist(df_1$DATE_OF_DEATH, breaks = 10)
hist(df_1$new_f_date, breaks = 20)
hist(df_1$f_date_plus_12, breaks=20)
table(df_1$died_in_study)

# count cases where missing data

missing_percent <- function(x) {(sum(is.na(x))/ (length(x)) *100)}
missing_n <- function(x) {sum(is.na(x))}

options(scipen=999)

apply(df_1, 2, missing_percent)
apply(df_1, 2, missing_n)

dim(df_1)

#Removing missing data on LTC combination_ID:
df_2 <- df_1 %>%
  filter(!is.na(LTC_12_Combination_Code)) %>%
  filter(!is.na(LTC_f_Combination_Code)) %>%
  filter(is.na(died_in_study) | died_in_study == 0)

dim(df_2)

#Check missings:

apply(df_2, 2, missing_percent)
apply(df_2, 2, missing_n)

#Don't allow people to leave frailty risk LTCs:

table(df_2$LTC_12_Intermediate_Frailty_Risk_HFRS)
table(df_2$LTC_12_High_Frailty_Risk_HFRS)

df_3 <- df_2 %>%
  select(-LTC_12_Combination_Code, -LTC_12_Combination_Name, -LTC_f_Combination_Code, -LTC_f_Combination_Name) %>%
  mutate(across(starts_with("LTC_12_"), ~replace_na(., replace = 0))) %>%
  mutate(across(starts_with("LTC_f_"), ~replace_na(., replace = 0))) %>%
  mutate(left_inter = LTC_12_Intermediate_Frailty_Risk_HFRS - LTC_f_Intermediate_Frailty_Risk_HFRS,
         left_high = LTC_12_High_Frailty_Risk_HFRS - LTC_f_High_Frailty_Risk_HFRS,
         LTC_12_Intermediate_Frailty_Risk_HFRS = as.numeric(LTC_12_Intermediate_Frailty_Risk_HFRS),
         LTC_12_Intermediate_Frailty_Risk_HFRS = if_else(left_inter == -1, 1, LTC_12_Intermediate_Frailty_Risk_HFRS),
         LTC_12_High_Frailty_Risk_HFRS = as.numeric(LTC_12_High_Frailty_Risk_HFRS),
         LTC_12_High_Frailty_Risk_HFRS = if_else(left_high == -1, 1, LTC_12_High_Frailty_Risk_HFRS))

table(df_3$LTC_12_Intermediate_Frailty_Risk_HFRS)
table(df_3$left_inter)
table(df_3$LTC_12_High_Frailty_Risk_HFRS)
table(df_3$left_high)

#calculate if people have LTCs / clusters of LTCs at f date and 12 month date:

df_4 <- df_3 %>%
  mutate(Diabetes_f = LTC_f_Diabetes,
         Diabetes_12 = LTC_12_Diabetes,
         Diabetes_New = ifelse((Diabetes_12- Diabetes_f) == 1, 1, 0),
         Cancer_f = LTC_f_Cancer,
         Cancer_12 = LTC_12_Cancer,
         Cancer_new = ifelse((Cancer_12- Cancer_f) == 1, 1, 0),
         Cardiovascular_f = LTC_f_Heart_Failure + LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation + LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease,
         Cardiovascular_12 = LTC_12_Heart_Failure + LTC_12_Severe_Heart_Failure + 
           LTC_12_Atrial_Fibrillation + LTC_12_Cerebrovascular_Disease +
           LTC_12_Coronary_Heart_Disease + LTC_12_Hypertension +
           LTC_12_Peripheral_Vascular_Disease,
         Cardiovascular_new = ifelse((Cardiovascular_12 - Cardiovascular_f) >= 1, 1, 0),
         Other_concordant_f = LTC_f_Osteoarthritis +  LTC_f_Physical_Disability +  LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + LTC_f_Chronic_Liver_Disease + LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Other_concordant_12 = LTC_12_Osteoarthritis +  LTC_12_Physical_Disability +  LTC_12_Dementia + 
           LTC_12_Intermediate_Frailty_Risk_HFRS + LTC_12_High_Frailty_Risk_HFRS +
           LTC_12_Chronic_Kidney_Disease + 
           LTC_12_End_Stage_Renal_Failure + LTC_12_Chronic_Liver_Disease + LTC_12_Liver_Failure +
           LTC_12_Chronic_Pain,
         Other_concordant_new = ifelse((Other_concordant_12 - Other_concordant_f) >= 1, 1, 0),
         Discordant_f =  LTC_f_Pulmonary_Heart_Disease + LTC_f_Epilepsy + LTC_f_Serious_Mental_Illness
         + LTC_f_Osteoporosis +  LTC_f_Rheumatoid_Arthritis + LTC_f_Asthma + LTC_f_COPD +
           LTC_f_Severe_COPD + LTC_f_Bronchiectasis + LTC_f_Autism + LTC_f_Cystic_Fibrosis +
           LTC_f_Learning_Disability + LTC_f_Parkinsons_Disease + LTC_f_Neurological_Organ_Failure
         +  LTC_f_Alcohol_Dependence +  LTC_f_Inflammatory_Bowel_Disease + LTC_f_Multiple_Sclerosis
         +LTC_f_Sarcoidosis  + LTC_f_Severe_Interstitial_Lung_Disease + LTC_f_Sickle_Cell_Disease,
         Discordant_12 =  LTC_12_Pulmonary_Heart_Disease + LTC_12_Epilepsy + LTC_12_Serious_Mental_Illness
         + LTC_12_Osteoporosis +  LTC_12_Rheumatoid_Arthritis + LTC_12_Asthma + LTC_12_COPD +
           LTC_12_Severe_COPD + LTC_12_Bronchiectasis + LTC_12_Autism + LTC_12_Cystic_Fibrosis +
           LTC_12_Learning_Disability + LTC_12_Parkinsons_Disease + LTC_12_Neurological_Organ_Failure
         +  LTC_12_Alcohol_Dependence +  LTC_12_Inflammatory_Bowel_Disease + LTC_12_Multiple_Sclerosis
         +LTC_12_Sarcoidosis  + LTC_12_Severe_Interstitial_Lung_Disease + LTC_12_Sickle_Cell_Disease,
         Discordant_new_binary = ifelse((Discordant_12 - Discordant_f) >= 1, 1, 0),
         Discordant_new_count = Discordant_12 - Discordant_f,
         Concordant_f =  LTC_f_Cancer + 
           LTC_f_Heart_Failure + 
           LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation +
           LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + 
           LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease + 
           LTC_f_Osteoarthritis +  
           LTC_f_Physical_Disability +
           LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + 
           LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + 
           LTC_f_Chronic_Liver_Disease + 
           LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Concordant_12 =  LTC_12_Cancer + 
           LTC_12_Heart_Failure + 
           LTC_12_Severe_Heart_Failure +
           LTC_12_Atrial_Fibrillation +
           LTC_12_Cerebrovascular_Disease +
           LTC_12_Coronary_Heart_Disease + 
           LTC_12_Hypertension +
           LTC_12_Peripheral_Vascular_Disease + 
           LTC_12_Osteoarthritis +  
           LTC_12_Physical_Disability +
           LTC_12_Dementia + 
           LTC_12_Intermediate_Frailty_Risk_HFRS + 
           LTC_12_High_Frailty_Risk_HFRS +
           LTC_12_Chronic_Kidney_Disease + 
           LTC_12_End_Stage_Renal_Failure + 
           LTC_12_Chronic_Liver_Disease + 
           LTC_12_Liver_Failure +
           LTC_12_Chronic_Pain,
         Concordant_new_binary = ifelse((Concordant_12 - Concordant_f) >= 1, 1, 0),
         Concordant_new_count = Concordant_12 - Concordant_f)


#Testing:

table(df_4$Diabetes_f, df_4$Diabetes_12)
table(df_4$Diabetes_f)
table(df_4$Diabetes_12)
table(df_4$Diabetes_New)
table(df_4$Cancer_f, df_4$Cancer_12)
table(df_4$Cancer_f)
table(df_4$Cancer_12)
table(df_4$Cancer_new)
table(df_4$Cardiovascular_f)
table(df_4$Cardiovascular_12)
table(df_4$Cardiovascular_new)
table(df_4$Cardiovascular_f, df_4$Cardiovascular_12)
table(df_4$Other_concordant_f)
table(df_4$Other_concordant_12)
table(df_4$Other_concordant_new)
table(df_4$Discordant_f, df_4$Discordant_12)
table(df_4$Discordant_f)
table(df_4$Discordant_12)
table(df_4$Discordant_new_binary)
table(df_4$Discordant_new_count)
table(df_4$Concordant_f, df_4$Concordant_12)
table(df_4$Concordant_f)
table(df_4$Concordant_12)
table(df_4$Concordant_new_binary)
table(df_4$Concordant_new_count)

#Save dataset:

saveRDS(df_4, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_12mth_outcomes_for_modelling.RDS")     

###################
#### 18 Months #####
###################

#Merge sql dataset with existing dataset, sort out dates, and make Died flag:

df_1 <- outcomes_18 %>%
  mutate(NHS_Number= as.numeric(NHS_Number)) %>%
  select(-Date_Id, -new_f_date) %>%
  mutate(across(.cols = c(f_date_end_month, outcome_18_date_end_month, f_date_plus_18), .fns = as_date)) %>%
  left_join(NDH_18, by = c("NHS_Number" = "NHS_Number_Flag")) %>%
  mutate(across(.cols = c(h_date, f_date, ndh_date_valid, h_date_end_month, 
                          DATE_OF_BIRTH, DATE_OF_DEATH), .fns = as_date)) %>%
  mutate(died_in_study =ifelse(DATE_OF_DEATH < f_date_plus_18, 1, 0)) %>%
  distinct(NHS_Number, .keep_all = TRUE) %>%
  arrange(NHS_Number)


#testing death variable:

hist(df_1$DATE_OF_DEATH, breaks = 10)
hist(df_1$new_f_date, breaks = 20)
hist(df_1$f_date_plus_18, breaks=20)
table(df_1$died_in_study)


# count cases where missing data:

missing_percent <- function(x) {(sum(is.na(x))/ (length(x)) *100)}
missing_n <- function(x) {sum(is.na(x))}

options(scipen=999)

apply(df_1, 2, missing_percent)
apply(df_1, 2, missing_n) 

dim(df_1)

#Removing missing data on LTC combination_ID:

df_2 <- df_1 %>%
  filter(!is.na(LTC_18_Combination_Code)) %>%
  filter(!is.na(LTC_f_Combination_Code)) %>%
  filter(is.na(died_in_study) | died_in_study == 0)

dim(df_2)

#Check missings:

apply(df_2, 2, missing_percent)
apply(df_2, 2, missing_n)

#Don't allow people to leave frailty risk LTCs:

table(df_2$LTC_18_Intermediate_Frailty_Risk_HFRS)
table(df_2$LTC_18_High_Frailty_Risk_HFRS)


df_3 <- df_2 %>%
  select(-LTC_18_Combination_Code, -LTC_18_Combination_Name, -LTC_f_Combination_Code, -LTC_f_Combination_Name) %>%
  mutate(across(starts_with("LTC_18_"), ~replace_na(., replace = 0))) %>%
  mutate(across(starts_with("LTC_f_"), ~replace_na(., replace = 0))) %>%
  mutate(left_inter = LTC_18_Intermediate_Frailty_Risk_HFRS - LTC_f_Intermediate_Frailty_Risk_HFRS,
         left_high = LTC_18_High_Frailty_Risk_HFRS - LTC_f_High_Frailty_Risk_HFRS,
         LTC_18_Intermediate_Frailty_Risk_HFRS = as.numeric(LTC_18_Intermediate_Frailty_Risk_HFRS),
         LTC_18_Intermediate_Frailty_Risk_HFRS = if_else(left_inter == -1, 1, LTC_18_Intermediate_Frailty_Risk_HFRS),
         LTC_18_High_Frailty_Risk_HFRS = as.numeric(LTC_18_High_Frailty_Risk_HFRS),
         LTC_18_High_Frailty_Risk_HFRS = if_else(left_high == -1, 1, LTC_18_High_Frailty_Risk_HFRS))


table(df_3$LTC_18_Intermediate_Frailty_Risk_HFRS)
table(df_3$left_inter)
table(df_3$LTC_18_High_Frailty_Risk_HFRS)
table(df_3$left_high)

#calculate if people have LTCs / clusters of LTCs at f date and 18 month date:

df_4 <- df_3 %>%
  mutate(Diabetes_f = LTC_f_Diabetes,
         Diabetes_18 = LTC_18_Diabetes,
         Diabetes_New = ifelse((Diabetes_18- Diabetes_f) == 1, 1, 0),
         Cancer_f = LTC_f_Cancer,
         Cancer_18 = LTC_18_Cancer,
         Cancer_new = ifelse((Cancer_18- Cancer_f) == 1, 1, 0),
         Cardiovascular_f = LTC_f_Heart_Failure + LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation + LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease,
         Cardiovascular_18 = LTC_18_Heart_Failure + LTC_18_Severe_Heart_Failure + 
           LTC_18_Atrial_Fibrillation + LTC_18_Cerebrovascular_Disease +
           LTC_18_Coronary_Heart_Disease + LTC_18_Hypertension +
           LTC_18_Peripheral_Vascular_Disease,
         Cardiovascular_new = ifelse((Cardiovascular_18 - Cardiovascular_f) >= 1, 1, 0),
         Other_concordant_f = LTC_f_Osteoarthritis +  LTC_f_Physical_Disability +  LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + LTC_f_Chronic_Liver_Disease + LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Other_concordant_18 = LTC_18_Osteoarthritis +  LTC_18_Physical_Disability +  LTC_18_Dementia + 
           LTC_18_Intermediate_Frailty_Risk_HFRS + LTC_18_High_Frailty_Risk_HFRS +
           LTC_18_Chronic_Kidney_Disease + 
           LTC_18_End_Stage_Renal_Failure + LTC_18_Chronic_Liver_Disease + LTC_18_Liver_Failure +
           LTC_18_Chronic_Pain,
         Other_concordant_new = ifelse((Other_concordant_18 - Other_concordant_f) >= 1, 1, 0),
         Discordant_f =  LTC_f_Pulmonary_Heart_Disease + LTC_f_Epilepsy + LTC_f_Serious_Mental_Illness
         + LTC_f_Osteoporosis +  LTC_f_Rheumatoid_Arthritis + LTC_f_Asthma + LTC_f_COPD +
           LTC_f_Severe_COPD + LTC_f_Bronchiectasis + LTC_f_Autism + LTC_f_Cystic_Fibrosis +
           LTC_f_Learning_Disability + LTC_f_Parkinsons_Disease + LTC_f_Neurological_Organ_Failure
         +  LTC_f_Alcohol_Dependence +  LTC_f_Inflammatory_Bowel_Disease + LTC_f_Multiple_Sclerosis
         +LTC_f_Sarcoidosis  + LTC_f_Severe_Interstitial_Lung_Disease + LTC_f_Sickle_Cell_Disease,
         Discordant_18 =  LTC_18_Pulmonary_Heart_Disease + LTC_18_Epilepsy + LTC_18_Serious_Mental_Illness
         + LTC_18_Osteoporosis +  LTC_18_Rheumatoid_Arthritis + LTC_18_Asthma + LTC_18_COPD +
           LTC_18_Severe_COPD + LTC_18_Bronchiectasis + LTC_18_Autism + LTC_18_Cystic_Fibrosis +
           LTC_18_Learning_Disability + LTC_18_Parkinsons_Disease + LTC_18_Neurological_Organ_Failure
         +  LTC_18_Alcohol_Dependence +  LTC_18_Inflammatory_Bowel_Disease + LTC_18_Multiple_Sclerosis
         +LTC_18_Sarcoidosis  + LTC_18_Severe_Interstitial_Lung_Disease + LTC_18_Sickle_Cell_Disease,
         Discordant_new_binary = ifelse((Discordant_18 - Discordant_f) >= 1, 1, 0),
         Discordant_new_count = Discordant_18 - Discordant_f,
         Concordant_f =  LTC_f_Cancer + 
           LTC_f_Heart_Failure + 
           LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation +
           LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + 
           LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease + 
           LTC_f_Osteoarthritis +  
           LTC_f_Physical_Disability +
           LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + 
           LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + 
           LTC_f_Chronic_Liver_Disease + 
           LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Concordant_18 =  LTC_18_Cancer + 
           LTC_18_Heart_Failure + 
           LTC_18_Severe_Heart_Failure +
           LTC_18_Atrial_Fibrillation +
           LTC_18_Cerebrovascular_Disease +
           LTC_18_Coronary_Heart_Disease + 
           LTC_18_Hypertension +
           LTC_18_Peripheral_Vascular_Disease + 
           LTC_18_Osteoarthritis +  
           LTC_18_Physical_Disability +
           LTC_18_Dementia + 
           LTC_18_Intermediate_Frailty_Risk_HFRS + 
           LTC_18_High_Frailty_Risk_HFRS +
           LTC_18_Chronic_Kidney_Disease + 
           LTC_18_End_Stage_Renal_Failure + 
           LTC_18_Chronic_Liver_Disease + 
           LTC_18_Liver_Failure +
           LTC_18_Chronic_Pain,
         Concordant_new_binary = ifelse((Concordant_18 - Concordant_f) >= 1, 1, 0),
         Concordant_new_count = Concordant_18 - Concordant_f)


#Testing:

table(df_4$Diabetes_f, df_4$Diabetes_18)
table(df_4$Diabetes_f)
table(df_4$Diabetes_18)
table(df_4$Diabetes_New)
table(df_4$Cancer_f, df_4$Cancer_18)
table(df_4$Cancer_f)
table(df_4$Cancer_18)
table(df_4$Cancer_new)
table(df_4$Cardiovascular_f)
table(df_4$Cardiovascular_18)
table(df_4$Cardiovascular_new)
table(df_4$Cardiovascular_f, df_4$Cardiovascular_18)
table(df_4$Other_concordant_f)
table(df_4$Other_concordant_18)
table(df_4$Other_concordant_new)
table(df_4$Discordant_f, df_4$Discordant_18)
table(df_4$Discordant_f)
table(df_4$Discordant_18)
table(df_4$Discordant_new_binary)
table(df_4$Discordant_new_count)
table(df_4$Concordant_f, df_4$Concordant_18)
table(df_4$Concordant_f)
table(df_4$Concordant_18)
table(df_4$Concordant_new_binary)
table(df_4$Concordant_new_count)


#Save dataset:

saveRDS(df_4, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_18mth_outcomes_for_modelling.RDS")     

###################
#### 24 Months #####
###################

#Merge sql dataset with existing dataset, sort out dates, and make Died flag:

df_1 <- outcomes_24 %>%
  mutate(NHS_Number= as.numeric(NHS_Number)) %>%
  select(-Date_Id, -new_f_date) %>%
  mutate(across(.cols = c(f_date_end_month, outcome_24_date_end_month, f_date_plus_24), .fns = as_date)) %>%
  left_join(NDH_24, by = c("NHS_Number" = "NHS_Number_Flag")) %>%
  mutate(across(.cols = c(h_date, f_date, ndh_date_valid, h_date_end_month, 
                          DATE_OF_BIRTH, DATE_OF_DEATH), .fns = as_date)) %>%
  mutate(died_in_study =ifelse(DATE_OF_DEATH < f_date_plus_24, 1, 0)) %>%
  distinct(NHS_Number, .keep_all = TRUE) %>%
  arrange(NHS_Number)

#testing death variable:

hist(df_1$DATE_OF_DEATH, breaks = 10)
hist(df_1$new_f_date, breaks = 20)
hist(df_1$f_date_plus_24, breaks=20)
table(df_1$died_in_study)

# Count cases where missing data:

missing_percent <- function(x) {(sum(is.na(x))/ (length(x)) *100)}
missing_n <- function(x) {sum(is.na(x))}

options(scipen=999)

apply(df_1, 2, missing_percent)
apply(df_1, 2, missing_n)

dim(df_1)

#Removing missing data on LTC combination_ID:

df_2 <- df_1 %>%
  filter(!is.na(LTC_24_Combination_Code)) %>%
  filter(!is.na(LTC_f_Combination_Code)) %>%
  filter(is.na(died_in_study) | died_in_study == 0)

dim(df_2)

#Check missings:

apply(df_2, 2, missing_percent)
apply(df_2, 2, missing_n)

#Don't allow people to leave frailty risk LTCs:

table(df_2$LTC_24_Intermediate_Frailty_Risk_HFRS)
table(df_2$LTC_24_High_Frailty_Risk_HFRS)

df_3 <- df_2 %>%
  select(-LTC_24_Combination_Code, -LTC_24_Combination_Name, -LTC_f_Combination_Code, -LTC_f_Combination_Name) %>%
  mutate(across(starts_with("LTC_24_"), ~replace_na(., replace = 0))) %>%
  mutate(across(starts_with("LTC_f_"), ~replace_na(., replace = 0))) %>%
  mutate(left_inter = LTC_24_Intermediate_Frailty_Risk_HFRS - LTC_f_Intermediate_Frailty_Risk_HFRS,
         left_high = LTC_24_High_Frailty_Risk_HFRS - LTC_f_High_Frailty_Risk_HFRS,
         LTC_24_Intermediate_Frailty_Risk_HFRS = as.numeric(LTC_24_Intermediate_Frailty_Risk_HFRS),
         LTC_24_Intermediate_Frailty_Risk_HFRS = if_else(left_inter == -1, 1, LTC_24_Intermediate_Frailty_Risk_HFRS),
         LTC_24_High_Frailty_Risk_HFRS = as.numeric(LTC_24_High_Frailty_Risk_HFRS),
         LTC_24_High_Frailty_Risk_HFRS = if_else(left_high == -1, 1, LTC_24_High_Frailty_Risk_HFRS))

table(df_3$LTC_24_Intermediate_Frailty_Risk_HFRS)
table(df_3$left_inter)
table(df_3$LTC_24_High_Frailty_Risk_HFRS)
table(df_3$left_high)

#calculate if people have LTCs / clusters of LTCs at f date and 24 month date:

df_4 <- df_3 %>%
  mutate(Diabetes_f = LTC_f_Diabetes,
         Diabetes_24 = LTC_24_Diabetes,
         Diabetes_New = ifelse((Diabetes_24- Diabetes_f) == 1, 1, 0),
         Cancer_f = LTC_f_Cancer,
         Cancer_24 = LTC_24_Cancer,
         Cancer_new = ifelse((Cancer_24- Cancer_f) == 1, 1, 0),
         Cardiovascular_f = LTC_f_Heart_Failure + LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation + LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease,
         Cardiovascular_24 = LTC_24_Heart_Failure + LTC_24_Severe_Heart_Failure + 
           LTC_24_Atrial_Fibrillation + LTC_24_Cerebrovascular_Disease +
           LTC_24_Coronary_Heart_Disease + LTC_24_Hypertension +
           LTC_24_Peripheral_Vascular_Disease,
         Cardiovascular_new = ifelse((Cardiovascular_24 - Cardiovascular_f) >= 1, 1, 0),
         Other_concordant_f = LTC_f_Osteoarthritis +  LTC_f_Physical_Disability +  LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + LTC_f_Chronic_Liver_Disease + LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Other_concordant_24 = LTC_24_Osteoarthritis +  LTC_24_Physical_Disability +  LTC_24_Dementia + 
           LTC_24_Intermediate_Frailty_Risk_HFRS + LTC_24_High_Frailty_Risk_HFRS +
           LTC_24_Chronic_Kidney_Disease + 
           LTC_24_End_Stage_Renal_Failure + LTC_24_Chronic_Liver_Disease + LTC_24_Liver_Failure +
           LTC_24_Chronic_Pain,
         Other_concordant_new = ifelse((Other_concordant_24 - Other_concordant_f) >= 1, 1, 0),
         Discordant_f =  LTC_f_Pulmonary_Heart_Disease + LTC_f_Epilepsy + LTC_f_Serious_Mental_Illness
         + LTC_f_Osteoporosis +  LTC_f_Rheumatoid_Arthritis + LTC_f_Asthma + LTC_f_COPD +
           LTC_f_Severe_COPD + LTC_f_Bronchiectasis + LTC_f_Autism + LTC_f_Cystic_Fibrosis +
           LTC_f_Learning_Disability + LTC_f_Parkinsons_Disease + LTC_f_Neurological_Organ_Failure
         +  LTC_f_Alcohol_Dependence +  LTC_f_Inflammatory_Bowel_Disease + LTC_f_Multiple_Sclerosis
         +LTC_f_Sarcoidosis  + LTC_f_Severe_Interstitial_Lung_Disease + LTC_f_Sickle_Cell_Disease,
         Discordant_24 =  LTC_24_Pulmonary_Heart_Disease + LTC_24_Epilepsy + LTC_24_Serious_Mental_Illness
         + LTC_24_Osteoporosis +  LTC_24_Rheumatoid_Arthritis + LTC_24_Asthma + LTC_24_COPD +
           LTC_24_Severe_COPD + LTC_24_Bronchiectasis + LTC_24_Autism + LTC_24_Cystic_Fibrosis +
           LTC_24_Learning_Disability + LTC_24_Parkinsons_Disease + LTC_24_Neurological_Organ_Failure
         +  LTC_24_Alcohol_Dependence +  LTC_24_Inflammatory_Bowel_Disease + LTC_24_Multiple_Sclerosis
         +LTC_24_Sarcoidosis  + LTC_24_Severe_Interstitial_Lung_Disease + LTC_24_Sickle_Cell_Disease,
         Discordant_new_binary = ifelse((Discordant_24 - Discordant_f) >= 1, 1, 0),
         Discordant_new_count = Discordant_24 - Discordant_f,
         Concordant_f =  LTC_f_Cancer + 
           LTC_f_Heart_Failure + 
           LTC_f_Severe_Heart_Failure +
           LTC_f_Atrial_Fibrillation +
           LTC_f_Cerebrovascular_Disease +
           LTC_f_Coronary_Heart_Disease + 
           LTC_f_Hypertension +
           LTC_f_Peripheral_Vascular_Disease + 
           LTC_f_Osteoarthritis +  
           LTC_f_Physical_Disability +
           LTC_f_Dementia + 
           LTC_f_Intermediate_Frailty_Risk_HFRS + 
           LTC_f_High_Frailty_Risk_HFRS +
           LTC_f_Chronic_Kidney_Disease + 
           LTC_f_End_Stage_Renal_Failure + 
           LTC_f_Chronic_Liver_Disease + 
           LTC_f_Liver_Failure +
           LTC_f_Chronic_Pain,
         Concordant_24 =  LTC_24_Cancer + 
           LTC_24_Heart_Failure + 
           LTC_24_Severe_Heart_Failure +
           LTC_24_Atrial_Fibrillation +
           LTC_24_Cerebrovascular_Disease +
           LTC_24_Coronary_Heart_Disease + 
           LTC_24_Hypertension +
           LTC_24_Peripheral_Vascular_Disease + 
           LTC_24_Osteoarthritis +  
           LTC_24_Physical_Disability +
           LTC_24_Dementia + 
           LTC_24_Intermediate_Frailty_Risk_HFRS + 
           LTC_24_High_Frailty_Risk_HFRS +
           LTC_24_Chronic_Kidney_Disease + 
           LTC_24_End_Stage_Renal_Failure + 
           LTC_24_Chronic_Liver_Disease + 
           LTC_24_Liver_Failure +
           LTC_24_Chronic_Pain,
         Concordant_new_binary = ifelse((Concordant_24 - Concordant_f) >= 1, 1, 0),
         Concordant_new_count = Concordant_24 - Concordant_f)


#Testing:

table(df_4$Diabetes_f, df_4$Diabetes_24)
table(df_4$Diabetes_f)
table(df_4$Diabetes_24)
table(df_4$Diabetes_New)
table(df_4$Cancer_f, df_4$Cancer_24)
table(df_4$Cancer_f)
table(df_4$Cancer_24)
table(df_4$Cancer_new)
table(df_4$Cardiovascular_f)
table(df_4$Cardiovascular_24)
table(df_4$Cardiovascular_new)
table(df_4$Cardiovascular_f, df_4$Cardiovascular_24)
table(df_4$Other_concordant_f)
table(df_4$Other_concordant_24)
table(df_4$Other_concordant_new)
table(df_4$Discordant_f, df_4$Discordant_24)
table(df_4$Discordant_f)
table(df_4$Discordant_24)
table(df_4$Discordant_new_binary)
table(df_4$Discordant_new_count)
table(df_4$Concordant_f, df_4$Concordant_24)
table(df_4$Concordant_f)
table(df_4$Concordant_24)
table(df_4$Concordant_new_binary)
table(df_4$Concordant_new_count)

#Save dataset:

saveRDS(df_4, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_24mth_outcomes_for_modelling.RDS")     