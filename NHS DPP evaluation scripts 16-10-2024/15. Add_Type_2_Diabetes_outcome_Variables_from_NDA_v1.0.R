#NDPP Evaluation

#T2D variable from NDA core data #

rm(list=ls()) #clear environment

##### Load packages
library(odbc) 
library(DBI)
library(tidyverse)
library(scales)
library(lubridate)
library(dplyr)

##### pulling data from analyst machine NDA and saving NDA core:

#NOTE: This section is hashed out because reading from database only done once then saved to folder

#con <- DBI::dbConnect(odbc(), dsn = "NCDR")
#NDAcore <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_NDA_Core_Latest_Audit]")

#saveRDS(NDAcore,"N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/Datasets from analyst machine/NDA_core.RDS")

#### Read NDACore data:

NDAcore<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/Datasets from analyst machine/NDA_core.RDS")

NDAcore<-NDAcore %>% mutate(Pseudo_NHS_Number=as.numeric(Pseudo_NHS_Number)) #change variable format to numeric to faciliate join

#### join to outcomes for modelling dataset:

dat_6M<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/NDH_6mth_outcomes_for_modelling.RDS") %>%
  rename(intervention=treat,Pseudo_NHS_Number=NHS_Number)

dat_6m_nda_core <- dat_6M %>% 
  left_join(NDAcore,by="Pseudo_NHS_Number")

dat_12M<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/NDH_12mth_outcomes_for_modelling.RDS") %>%
  rename(intervention=treat,Pseudo_NHS_Number=NHS_Number)

dat_12m_nda_core <- dat_12M %>% 
  left_join(NDAcore,by="Pseudo_NHS_Number")

dat_18M<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/NDH_18mth_outcomes_for_modelling.RDS") %>%
  rename(intervention=treat,Pseudo_NHS_Number=NHS_Number)

dat_18m_nda_core <- dat_18M %>% 
  left_join(NDAcore,by="Pseudo_NHS_Number")

dat_24M<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/NDH_24mth_outcomes_for_modelling.RDS") %>%
  rename(intervention=treat,Pseudo_NHS_Number=NHS_Number)

dat_24m_nda_core <- dat_24M %>% 
  left_join(NDAcore,by="Pseudo_NHS_Number")


#### Read in all T2D outcome datasets:

files <- list.files(path="N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/Outcomes_forT2Dvar/", pattern=".RDS", all.files=T, full.names=T) #create list of all files in T2D folder
data<-lapply(files, readRDS) #read all the files T2D files in. The result is a list

##### Take each element from the list 'data' to make four separate dataframes:

dat_12m<-data[[1]]  
dat_18m<-data[[2]]
dat_24<-data[[3]]
dat_6m<-data[[4]]

#Make a variable for type 2 diagnoses occuring within f -date and f-date + 6 mths:

dat_6m_nda_core<-dat_6m_nda_core %>% 
  mutate(Diabetes2_6=case_when((DERIVED_CLEAN_DIABETES_TYPE==2)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_6_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes1_6=case_when((DERIVED_CLEAN_DIABETES_TYPE==1)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_6_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes99_6=case_when((DERIVED_CLEAN_DIABETES_TYPE==99)&
                                  (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                  (CLEAN_DIAGNOSIS_DATE<=(outcome_6_date_end_month))
                                ~1,
                                TRUE~0))

#+12 mths:

dat_12m_nda_core<-dat_12m_nda_core %>% 
  mutate(Diabetes2_12=case_when((DERIVED_CLEAN_DIABETES_TYPE==2)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_12_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes1_12=case_when((DERIVED_CLEAN_DIABETES_TYPE==1)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_12_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes99_12=case_when((DERIVED_CLEAN_DIABETES_TYPE==99)&
                                  (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                  (CLEAN_DIAGNOSIS_DATE<=(outcome_12_date_end_month))
                                ~1,
                                TRUE~0))

#+18 mths:

dat_18m_nda_core<-dat_18m_nda_core %>% 
  mutate(Diabetes2_18=case_when((DERIVED_CLEAN_DIABETES_TYPE==2)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_18_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes1_18=case_when((DERIVED_CLEAN_DIABETES_TYPE==1)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_18_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes99_18=case_when((DERIVED_CLEAN_DIABETES_TYPE==99)&
                                  (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                  (CLEAN_DIAGNOSIS_DATE<=(outcome_18_date_end_month))
                                ~1,
                                TRUE~0))

#+24 months:

dat_24m_nda_core<-dat_24m_nda_core %>% 
  mutate(Diabetes2_24=case_when((DERIVED_CLEAN_DIABETES_TYPE==2)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_24_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes1_24=case_when((DERIVED_CLEAN_DIABETES_TYPE==1)&
                                 (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                 (CLEAN_DIAGNOSIS_DATE<=(outcome_24_date_end_month))
                               ~1,
                               TRUE~0)) %>% 
  mutate(Diabetes99_24=case_when((DERIVED_CLEAN_DIABETES_TYPE==99)&
                                  (CLEAN_DIAGNOSIS_DATE>f_date_end_month)&
                                  (CLEAN_DIAGNOSIS_DATE<=(outcome_24_date_end_month))
                                ~1,
                                TRUE~0))

#Count checks:

dat_6m_nda_core %>% count(Diabetes2_6)
dat_6m_nda_core %>% count(Diabetes1_6)
dat_6m_nda_core %>% count(Diabetes_New)
dat_12m_nda_core %>% count(Diabetes2_12)
dat_12m_nda_core %>% count(Diabetes1_12)
dat_12m_nda_core %>% count(Diabetes_New)
dat_18m_nda_core %>% count(Diabetes2_18)
dat_18m_nda_core %>% count(Diabetes1_18)
dat_18m_nda_core %>% count(Diabetes_New)
dat_24m_nda_core %>% count(Diabetes2_24)
dat_24m_nda_core %>% count(Diabetes1_24)
dat_24m_nda_core %>% count(Diabetes_New)

#Cross-tab checks:

table(dat_6m_nda_core$Diabetes2_6,dat_6m_nda_core$Diabetes_New)
table(dat_6m_nda_core$Diabetes1_6,dat_6m_nda_core$Diabetes_New)
table(dat_12m_nda_core$Diabetes2_12,dat_12m_nda_core$Diabetes_New)
table(dat_12m_nda_core$Diabetes1_12,dat_12m_nda_core$Diabetes_New)
table(dat_18m_nda_core$Diabetes2_18,dat_18m_nda_core$Diabetes_New)
table(dat_18m_nda_core$Diabetes1_18,dat_18m_nda_core$Diabetes_New)
table(dat_24m_nda_core$Diabetes2_24,dat_24m_nda_core$Diabetes_New)
table(dat_24m_nda_core$Diabetes1_24,dat_24m_nda_core$Diabetes_New)


# Saving outcomes for modelling datasets where Type2 var == Diabetes2_6,Diabetes2_12,Diabetes2_18,Diabetes2_24:

NDH_6mth_outcomes_for_modelling_withType2var<-dat_6m_nda_core %>% select(,-c(NCDR_Core_IDENT,AUDIT_YEAR,DERIVED_CLEAN_SEX,DERIVED_CLEAN_ETHNICITY,DERIVED_CLEAN_DIAGNOSIS_YEAR,SMOKING_DATE,Frailty,AGE,DERIVED_CLEAN_BIRTH_YEAR,SMOKING,Frailty_Date))
NDH_12mth_outcomes_for_modelling_withType2var<-dat_12m_nda_core %>% select(,-c(NCDR_Core_IDENT,AUDIT_YEAR,DERIVED_CLEAN_SEX,DERIVED_CLEAN_ETHNICITY,DERIVED_CLEAN_DIAGNOSIS_YEAR,SMOKING_DATE,Frailty,AGE,DERIVED_CLEAN_BIRTH_YEAR,SMOKING,Frailty_Date))
NDH_18mth_outcomes_for_modelling_withType2var<-dat_18m_nda_core %>% select(,-c(NCDR_Core_IDENT,AUDIT_YEAR,DERIVED_CLEAN_SEX,DERIVED_CLEAN_ETHNICITY,DERIVED_CLEAN_DIAGNOSIS_YEAR,SMOKING_DATE,Frailty,AGE,DERIVED_CLEAN_BIRTH_YEAR,SMOKING,Frailty_Date))
NDH_24mth_outcomes_for_modelling_withType2var<-dat_24m_nda_core %>% select(,-c(NCDR_Core_IDENT,AUDIT_YEAR,DERIVED_CLEAN_SEX,DERIVED_CLEAN_ETHNICITY,DERIVED_CLEAN_DIAGNOSIS_YEAR,SMOKING_DATE,Frailty,AGE,DERIVED_CLEAN_BIRTH_YEAR,SMOKING,Frailty_Date))

saveRDS(NDH_6mth_outcomes_for_modelling_withType2var,"N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS")
saveRDS(NDH_12mth_outcomes_for_modelling_withType2var,"N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_12mth_outcomes_for_modelling_withType2var.RDS")
saveRDS(NDH_18mth_outcomes_for_modelling_withType2var,"N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_18mth_outcomes_for_modelling_withType2var.RDS")
saveRDS(NDH_24mth_outcomes_for_modelling_withType2var,"N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_24mth_outcomes_for_modelling_withType2var.RDS")
