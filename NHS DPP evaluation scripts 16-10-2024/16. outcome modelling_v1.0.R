# NDPP Evaluation            #
#Code written by Xiaochen Ge # 
#                            #
#                            #
#   Outcome modelling        #
# diabetes (two types of outcomes)# 
# concordant- count outcome  #
# discordant -count outcome  #


# Install and load 
# required libraries:

library(jtools)
library(tidyverse)
library(lubridate)


# Preamble:

rm(list=ls())  # Clear work-space

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/"  # Set project folder


#Predictor variables for modelling:


source(file.path(project_dir, "R/scripts/XG/fitGLM_ndpp_SC.r"))

load(file.path(project_dir, "R/data/19_April/df_2.RData"))  # Load analysis data-frame)

df_1 <-  df_2 %>%
  mutate(IMD_quintile = as.integer(IMD_quintile)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_condition_count)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  mutate(h_date_quarter = as.character(h_date_quarter))

#Select variables for modelling:

not_for_matching <- c("NHS_Number_Flag", "h_date", "f_date", "provider_name", "ndh_date_valid","IMD_DECILE_PERSON", "GP_ORG_CODE",
                      "GP_NAME", "GP_POST_CODE", "pop_group", "h_date_end_month", "Person_Id", "DATE_OF_DEATH", "DATE_OF_BIRTH", 
                      "LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions") #Name is confusing: these variables are for modelling, not matching

all_names <- colnames(df_1) 

names_for_matching <- setdiff(all_names, not_for_matching) # create vector that includes only variables used to be used in the modelling, Name is confusing - this is for modelling,  not matching
write_rds(names_for_matching, file.path(project_dir, "R/data/07_Aug/names_for_matching.RDS")) #save vector of variables to be used in modelling
names_for_matching<-readRDS(file.path(project_dir, "R/data/07_Aug/names_for_matching.RDS"))

outcome_variable_6= c('Diabetes_New', 'Concordant_new_count', 'Discordant_new_count', 'Diabetes2_6' ) # 4 different outcome variables at each follow-up
outcome_variable_12= c('Diabetes_New', 'Concordant_new_count', 'Discordant_new_count', 'Diabetes2_12' )
outcome_variable_18= c('Diabetes_New', 'Concordant_new_count', 'Discordant_new_count', 'Diabetes2_18' )
outcome_variable_24= c('Diabetes_New', 'Concordant_new_count', 'Discordant_new_count', 'Diabetes2_24' )

#6M----
dat_6M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS")) %>%
  mutate(studylength=6) %>%
  as.data.frame()

dat_6M_1 <- dat_6M %>%
  filter(pop_group == 1 | pop_group == 4) %>% # pop group 1 is not referred. In final publication Chappell, Hatfield et al., this is the supplementary anlaysis
  as.data.frame()

dat_6M_2 <- dat_6M %>%
  filter(pop_group ==2 | pop_group ==4) %>% # pop group 2 is referred but didnt start. In final publication Chappell, Hatfield et al., this is the main anlaysis
  as.data.frame()

for (outcome in outcome_variable_6 )  
{

  fitGLM_fn(endpointName =outcome, ##fitGLM_ndpp_SC.r function / script author: Stefano Conti. Function automatically selects appropriate model depending on level of measurement of outcome variable (i.e. count or binary)
            predictorNames_vec=names_for_matching,
            design_dat = dat_6M_1,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/6M_1"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

for (outcome in outcome_variable_6 )  
{
  
  fitGLM_fn(endpointName =outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_6M_2,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/6M_2"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

#12M----
dat_12M <- readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_12mth_outcomes_for_modelling_withType2var.RDS")) %>%
  mutate(studylength=12) %>%
  as.data.frame()

dat_12M_1 <- dat_12M %>%
  filter(pop_group == 1 | pop_group == 4) %>%
  as.data.frame()

dat_12M_2 <- dat_12M %>%
  filter(pop_group ==2 | pop_group ==4) %>%
  as.data.frame()

for (outcome in outcome_variable_12 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_12M_1,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/12M_1"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

for (outcome in outcome_variable_12 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_12M_2,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/12M_2"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}



#18M----
dat_18M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_18mth_outcomes_for_modelling_withType2var.RDS")) %>%
  mutate(studylength=18) %>%
  as.data.frame()

dat_18M_1 <- dat_18M %>%
  filter(pop_group == 1 | pop_group == 4) %>%
  as.data.frame()

dat_18M_2 <- dat_18M %>%
  filter(pop_group ==2 | pop_group ==4) %>%
  as.data.frame()

for (outcome in outcome_variable_18 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_18M_1,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/18M_1"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

for (outcome in outcome_variable_18 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_18M_2,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/18M_2"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

#24M----
dat_24M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_24mth_outcomes_for_modelling_withType2var.RDS")) %>%
  mutate(studylength=24) %>%
  as.data.frame()

dat_24M_1 <- dat_24M %>%
  filter(pop_group == 1 | pop_group == 4) %>%
  as.data.frame()

dat_24M_2 <- dat_24M %>%
  filter(pop_group ==2 | pop_group ==4) %>%
  as.data.frame()

for (outcome in outcome_variable_24 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_24M_1,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/24M_1"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

for (outcome in outcome_variable_24 )  
{
  
  fitGLM_fn(endpointName = outcome, 
            predictorNames_vec=names_for_matching,
            design_dat = dat_24M_2,
            analysisLabel='full', 
            subgroupLabel='full',
            outputPath=file.path(project_dir, "Output/07_Aug/outcome_modelling/regression/24M_2"), 
            interactionLabel=NULL, 
            interactionName=NULL)
  
}

