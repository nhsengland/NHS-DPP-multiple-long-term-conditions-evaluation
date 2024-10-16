#### NDPP Evaluation ####
#Code written by Xiaochen Ge, edits by Paul Chappell         #
# Get characteristics of              #
#matched and unmatched groups(table1) #

#### Load packages:
library(jtools)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(flextable)

rm(list=ls()) #clear environment

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/"  # Set project folder


names_for_matching<-readRDS(file.path(project_dir, "R/data/19_April/names_for_matching.RDS")) #Read vector that includes only variables used to be used in the modelling, Name is confusing - this is for modelling,  not matching

name_char<-names_for_matching[! names_for_matching %in% c('h_date_quarter','pred_prob_completion_given_1','ccg_GP')] # Remove ccg, pred_prob_completion_given_1, h_date_quarter


load(file.path(project_dir, "R/data/19_April/df_2.RData"))

data_for_matching_main <- df_2 %>% # df2_for_matching is unmatched dataset
  filter(pop_group == 2 | pop_group == 4) %>% #  pop group 2 is referred but didnt start.In final publication Chappell, Hatfield et al., this is the supplementary anlaysis
  as.data.frame()

data_for_matching_supp <- df_2 %>%
  filter(pop_group ==1 | pop_group ==4) %>% # pop group 1 is not referred.. In final publication Chappell, Hatfield et al., this is the supp anlaysis
  as.data.frame()

matched_data<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS"))  
# NDH_6mth_outcomes_for_modelling_withType2var.RDS is the matched dataset, and only need to use 6 month dataset

matched_data_main <- matched_data %>%
  filter(pop_group == 2 | pop_group == 4) %>%
  as.data.frame()

matched_data_supp <- matched_data %>%
  filter(pop_group == 1 | pop_group ==4) %>%
  as.data.frame()

#### Main analysis.  ####

# unmacthed dataset

unmatched_chara <- data_for_matching_main %>%
  select(pop_group, all_of(name_char)) %>%
  tbl_summary(by = pop_group,
              statistic = all_continuous() ~ "{mean} � ({sd})",  # Removed list()
              digits = all_continuous() ~ c(1, 1)) %>%           # Removed list()
  as_gt() %>% # uses gt table package to construct the descriptive stats tables
  gt::gtsave(file.path(project_dir, "Output/14_03_24/charac/main/characteristics_unmatched.rtf"))

  
# matched data
matched_data2<-matched_data_main %>%
  mutate(rural_urban_GP=as.factor(rural_urban_GP))

matched_charac<-matched_data2 %>%
  select(intervention, all_of(name_char))%>%
  tbl_summary(by = intervention,
              statistic = all_continuous() ~ "{mean} � ({sd})",  # Removed list()
              digits = all_continuous() ~ c(1, 1)) %>%           # Removed list()
  as_gt() %>% # uses gt table package to construct the descriptive stats tables
  gt::gtsave(file.path(project_dir, "Output/14_03_24/charac/main/characteristics_matched_6M.rtf")) 
  


#### Supplementary analysis. NOTE: In published paper, main and supp are swapped. ####

# unmatched dataset
unmatched_chara<-data_for_matching_supp %>%
    select(pop_group, all_of(name_char)) %>%
    tbl_summary(by = pop_group,
                statistic = all_continuous() ~ "{mean} � ({sd})",  
                digits = all_continuous() ~ c(1, 1)) %>%           
    as_gt() %>%
    gt::gtsave(file.path(project_dir, "Output/14_03_24/charac/supp/characteristics_unmatched.rtf")) 

# matched data
matched_data<-matched_data_supp %>%
  mutate(rural_urban_GP=as.factor(rural_urban_GP))

matched_charac<-matched_data %>%
  select(intervention, all_of(name_char))%>%
  tbl_summary(by = intervention,
              statistic = all_continuous() ~ "{mean} � ({sd})",  
              digits = all_continuous() ~ c(1, 1)) %>%    
  as_gt() %>%
  gt::gtsave(file.path(project_dir, "Output/14_03_24/charac/supp/characteristics_matched_6M.rtf")) 


