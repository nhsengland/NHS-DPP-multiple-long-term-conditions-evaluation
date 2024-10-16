### NDPP Evaluation ###
### Code written by Paul Chappell ###
### Multimorbidity incidence analysis ###

#Load packages:

library(jtools)
library(tidyverse)
library(lubridate)

rm(list=ls()) #clear environment:

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/"  # Set project folder

source(file.path(project_dir, "R/scripts/XG/prv_inc.r")) #load prevalence -incidence function

names_for_matching<-readRDS(file.path(project_dir, "R/data/19_April/names_for_matching.RDS")) 
LTC_var<-names_for_matching[str_detect(names_for_matching,'LTC')] #select only var names that are LTCs
LTC_var2<-as.data.frame(LTC_var) 

#### Consturcting dataset that outlines all the variables needed (presence of condition at h date, base('f date') plus 4 follow-up prevalence, whether LTCs develped after h date, and whether LTCs developed after f-date )
LTC_var3<- LTC_var2 %>%
  separate(LTC_var, into=c('A','cond'), sep='LTC_') %>%
  mutate(base=paste0('LTC','_','f','_',cond)) %>% 
  mutate(LTC_6=paste0('LTC','_','6','_',cond)) %>%
  mutate(LTC_12=paste0('LTC','_','12','_',cond)) %>%
  mutate(LTC_18=paste0('LTC','_','18','_',cond)) %>%
  mutate(LTC_24=paste0('LTC','_','24','_',cond)) %>%
  mutate(LTC_h=paste0('LTC','_',cond)) %>%
  mutate(LTC_6_new_after_h = paste0('LTC','_',cond, ' - ','LTC','_','6','_',cond )) %>%
  mutate(LTC_6_new_after_f = paste0('LTC','_','f','_',cond,' - ','LTC','_','6','_',cond)) %>%
  mutate(LTC_12_new_after_h = paste0('LTC','_',cond, ' - ','LTC','_','12','_',cond )) %>%
  mutate(LTC_12_new_after_f = paste0('LTC','_','f','_',cond,' - ','LTC','_','12','_',cond)) %>%
  mutate(LTC_18_new_after_h = paste0('LTC','_',cond, ' - ','LTC','_','18','_',cond )) %>%
  mutate(LTC_18_new_after_f = paste0('LTC','_','f','_',cond,' - ','LTC','_','18','_',cond)) %>%
  mutate(LTC_24_new_after_h = paste0('LTC','_',cond, ' - ','LTC','_','24','_',cond )) %>%
  mutate(LTC_24_new_after_f = paste0('LTC','_','f','_',cond,' - ','LTC','_','24','_',cond)) %>%
  as.data.frame()

#####    Presence and incidence of LTCs:

#### 6M:

exp_var_6M=paste(LTC_var3$LTC_h, collapse = '+') # select relevant var names from LTC_var3

exp_var_6M=paste(LTC_var3$LTC_6, collapse = '+')# select relevant var names from LTC_var3

dat_6M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(n_cond_h = LTC_Alcohol_Dependence+ # compute number of conditions at h date variable
           LTC_Asthma+
           LTC_Atrial_Fibrillation+
           LTC_Bronchiectasis+
           LTC_Cancer+
           LTC_Cerebrovascular_Disease+
           LTC_Chronic_Kidney_Disease+
           LTC_Chronic_Liver_Disease+
           LTC_Chronic_Pain+
           LTC_COPD+
           LTC_Coronary_Heart_Disease+
           LTC_Cystic_Fibrosis+
           LTC_Depression+
           LTC_Diabetes+
           LTC_Epilepsy+
           LTC_Heart_Failure+
           LTC_Hypertension+
           LTC_Inflammatory_Bowel_Disease+
           LTC_Multiple_Sclerosis+
           LTC_Osteoarthritis+
           LTC_Osteoporosis+
           LTC_Parkinsons_Disease+
           LTC_Peripheral_Vascular_Disease+
           LTC_Pulmonary_Heart_Disease+
           LTC_Rheumatoid_Arthritis+
           LTC_Sarcoidosis+
           LTC_Serious_Mental_Illness+
           LTC_Sickle_Cell_Disease+
           LTC_Autism+
           LTC_Learning_Disability+
           LTC_Physical_Disability+
           LTC_Dementia+
           LTC_Intermediate_Frailty_Risk_HFRS+
           LTC_High_Frailty_Risk_HFRS+
           LTC_Severe_Interstitial_Lung_Disease)  %>%
  mutate(n_cond_f = LTC_Alcohol_Dependence+ # compute number of conditions at h date variable
           LTC_f_Asthma+
           LTC_f_Atrial_Fibrillation+
           LTC_f_Bronchiectasis+
           LTC_f_Cancer+
           LTC_f_Cerebrovascular_Disease+
           LTC_f_Chronic_Kidney_Disease+
           LTC_f_Chronic_Liver_Disease+
           LTC_f_Chronic_Pain+
           LTC_f_COPD+
           LTC_f_Coronary_Heart_Disease+
           LTC_f_Cystic_Fibrosis+
           LTC_f_Depression+
           LTC_f_Diabetes+
           LTC_f_Epilepsy+
           LTC_f_Heart_Failure+
           LTC_f_Hypertension+
           LTC_f_Inflammatory_Bowel_Disease+
           LTC_f_Multiple_Sclerosis+
           LTC_f_Osteoarthritis+
           LTC_f_Osteoporosis+
           LTC_f_Parkinsons_Disease+
           LTC_f_Peripheral_Vascular_Disease+
           LTC_f_Pulmonary_Heart_Disease+
           LTC_f_Rheumatoid_Arthritis+
           LTC_f_Sarcoidosis+
           LTC_f_Serious_Mental_Illness+
           LTC_f_Sickle_Cell_Disease+
           LTC_f_Autism+
           LTC_f_Learning_Disability+
           LTC_f_Physical_Disability+
           LTC_f_Dementia+
           LTC_f_Intermediate_Frailty_Risk_HFRS+
           LTC_f_High_Frailty_Risk_HFRS+
           LTC_f_Severe_Interstitial_Lung_Disease)  %>%
  mutate(n_cond_6 = LTC_6_Alcohol_Dependence+ # compute number of conditions at 6 month variable
           LTC_6_Asthma+
           LTC_6_Atrial_Fibrillation+
           LTC_6_Bronchiectasis+
           LTC_6_Cancer+
           LTC_6_Cerebrovascular_Disease+
           LTC_6_Chronic_Kidney_Disease+
           LTC_6_Chronic_Liver_Disease+
           LTC_6_Chronic_Pain+
           LTC_6_COPD+
           LTC_6_Coronary_Heart_Disease+
           LTC_6_Cystic_Fibrosis+
           LTC_6_Depression+
           LTC_6_Diabetes+
           LTC_6_Epilepsy+
           LTC_6_Heart_Failure+
           LTC_6_Hypertension+
           LTC_6_Inflammatory_Bowel_Disease+
           LTC_6_Multiple_Sclerosis+
           LTC_6_Osteoarthritis+
           LTC_6_Osteoporosis+
           LTC_6_Parkinsons_Disease+
           LTC_6_Peripheral_Vascular_Disease+
           LTC_6_Pulmonary_Heart_Disease+
           LTC_6_Rheumatoid_Arthritis+
           LTC_6_Sarcoidosis+
           LTC_6_Serious_Mental_Illness+
           LTC_6_Sickle_Cell_Disease+
           LTC_6_Autism+
           LTC_6_Learning_Disability+
           LTC_6_Physical_Disability+
           LTC_6_Dementia+
           LTC_6_Intermediate_Frailty_Risk_HFRS+
           LTC_6_High_Frailty_Risk_HFRS+
           LTC_6_Severe_Interstitial_Lung_Disease) %>%
  mutate(Any_new = Concordant_new_count + Discordant_new_count) %>% #compute number of new conditions variable
  mutate(Multi_h_prev=case_when(n_cond_h>=2~2, # Classify as multimorbid (2+ conditions), single LTC or no LTC at h date
                              n_cond_h==1~1,
                              n_cond_h==0~0 )) %>%
 mutate(Multi_prev=case_when(n_cond_6>=2~2, # Classify as multimorbid (2+ conditions), single LTC or no LTC at f date + 6M
                      n_cond_6==1~1,
                       n_cond_6==0~0 )) %>%
  mutate(Multi_inc=case_when(Any_new>=2~2, # Classify as developed 2+ conditions, single LTC or no news LTC between f date and f date + 6
                             Any_new==1~1,
                             Any_new==0~0 )) %>%
  mutate(multi_status= case_when(Multi_prev <2 ~ "Not multimorbid", # classify as not multimorbid at 6M, already multimorbid at H, or became multimorbid between h and follow-up,
                                 Multi_h_prev == 2 ~ "Already multimorbid",
                                 Multi_h_prev==0 & Multi_prev==2 ~ "Became multimorbid",
                                Multi_h_prev== 1 & Multi_prev==2 ~ "Became multimorbid")) %>%
  mutate(n_pre_n_post = case_when(n_cond_h==0 & n_cond_6 == 0 ~ "Zero_pre_zero_post", # classify as one of all combinations of 0,1 or 2 LTCs at H and 6 monht follow-up
                                  n_cond_h==0 & n_cond_6 == 1 ~ "Zero_pre_one_post",
                                  n_cond_h==0 & n_cond_6 > 1 ~ "Zero_pre_two_plus_post",
                                  n_cond_h==1 & n_cond_6 == 1 ~ "One_pre_one_post",
                                  n_cond_h==1 & n_cond_6 >= 1 ~ "One_pre_two_plus_post",
                                  n_cond_h>1 & n_cond_6 > 1 ~ "Two_plus_pre_two_plus_post")) %>%
select(intervention, n_cond_h, n_cond_f, n_cond_6,Concordant_new_count, Discordant_new_count, Any_new, Multi_h_prev, Multi_prev, Multi_inc, multi_status, n_pre_n_post) #select out all variables counting LTCs at different time points or classiying people according t number of LTCs / number of new LTCs sdeveloped between time points

table_6_h_prev<-table(dat_6M$Multi_h_prev, dat_6M$intervention) # Compare control and intervention on multimorbidity at h date
table_6_prev<- table(dat_6M$Multi_prev, dat_6M$intervention) # Compare control and intervention on multimorbidity at 6 months
table_6_inc<-table(dat_6M$Multi_inc, dat_6M$intervention) # Compare control and intervention on new morbidity
row_names <- c("Zero", "One", "Two or more")
table_6 <- row_names %>%
  bind_cols(table_6_h_prev,table_6_prev, table_6_inc) #bind tables above together

table_6_pre_post <- table(dat_6M$n_pre_n_post, dat_6M$intervention)

multi_test_6 <- dat_6M %>% 
  filter(multi_status == "Not multimorbid" | multi_status ==  "Became multimorbid" ) #select only those who were not multimorbid to compare against those who became multimorbid (i.e. ignroing those who were already)

test_table_6 <- table(multi_test_6$multi_status, multi_test_6$intervention) #Compare control and intervention on new multi-morbidity

chisq.test(test_table_6) #chi square test

table_for_paper_6 <- as.data.frame.matrix(test_table_6) %>%
  bind_rows(as.data.frame.matrix(table_6_pre_post)) #make table for paper

table_for_paper_6$Multimorbidity_status <- rownames(table_for_paper_6)
rownames(table_for_paper_6) <- NULL  # Remove row names and make values in a variable instead

table_for_paper_6 <- table_for_paper_6  %>%
  select(Multimorbidity_status, `0`, `1`) # select appropriate cols


desired_order <- c("Not multimorbid", "Became multimorbid",
                   "Zero_pre_zero_post", "Zero_pre_one_post", "Zero_pre_two_plus_post", "One_pre_one_post",
                   "One_pre_two_plus_post", "Two_plus_pre_two_plus_post") # prepare to re-order

index_order <- match(table_for_paper_6$Multimorbidity_status, desired_order) #re-order prep

table_for_paper_6 <- arrange(table_for_paper_6, index_order) # re-order

colnames(table_for_paper_6)[2:3] <- c("Control_6", "Intervention_6") #label cols

##### 12M:

exp_var_12M=paste(LTC_var3$LTC_h, collapse = '+')

exp_var_12M=paste(LTC_var3$LTC_12, collapse = '+')

dat_12M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_12mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
mutate(n_cond_h = LTC_Alcohol_Dependence+
         LTC_Asthma+
         LTC_Atrial_Fibrillation+
         LTC_Bronchiectasis+
         LTC_Cancer+
         LTC_Cerebrovascular_Disease+
         LTC_Chronic_Kidney_Disease+
         LTC_Chronic_Liver_Disease+
         LTC_Chronic_Pain+
         LTC_COPD+
         LTC_Coronary_Heart_Disease+
         LTC_Cystic_Fibrosis+
         LTC_Depression+
         LTC_Diabetes+
         LTC_Epilepsy+
         LTC_Heart_Failure+
         LTC_Hypertension+
         LTC_Inflammatory_Bowel_Disease+
         LTC_Multiple_Sclerosis+
         LTC_Osteoarthritis+
         LTC_Osteoporosis+
         LTC_Parkinsons_Disease+
         LTC_Peripheral_Vascular_Disease+
         LTC_Pulmonary_Heart_Disease+
         LTC_Rheumatoid_Arthritis+
         LTC_Sarcoidosis+
         LTC_Serious_Mental_Illness+
         LTC_Sickle_Cell_Disease+
         LTC_Autism+
         LTC_Learning_Disability+
         LTC_Physical_Disability+
         LTC_Dementia+
         LTC_Intermediate_Frailty_Risk_HFRS+
         LTC_High_Frailty_Risk_HFRS+
         LTC_Severe_Interstitial_Lung_Disease)  %>%
  mutate(n_cond_12 = LTC_12_Alcohol_Dependence+
           LTC_12_Asthma+
           LTC_12_Atrial_Fibrillation+
           LTC_12_Bronchiectasis+
           LTC_12_Cancer+
           LTC_12_Cerebrovascular_Disease+
           LTC_12_Chronic_Kidney_Disease+
           LTC_12_Chronic_Liver_Disease+
           LTC_12_Chronic_Pain+
           LTC_12_COPD+
           LTC_12_Coronary_Heart_Disease+
           LTC_12_Cystic_Fibrosis+
           LTC_12_Depression+
           LTC_12_Diabetes+
           LTC_12_Epilepsy+
           LTC_12_Heart_Failure+
           LTC_12_Hypertension+
           LTC_12_Inflammatory_Bowel_Disease+
           LTC_12_Multiple_Sclerosis+
           LTC_12_Osteoarthritis+
           LTC_12_Osteoporosis+
           LTC_12_Parkinsons_Disease+
           LTC_12_Peripheral_Vascular_Disease+
           LTC_12_Pulmonary_Heart_Disease+
           LTC_12_Rheumatoid_Arthritis+
           LTC_12_Sarcoidosis+
           LTC_12_Serious_Mental_Illness+
           LTC_12_Sickle_Cell_Disease+
           LTC_12_Autism+
           LTC_12_Learning_Disability+
           LTC_12_Physical_Disability+
           LTC_12_Dementia+
           LTC_12_Intermediate_Frailty_Risk_HFRS+
           LTC_12_High_Frailty_Risk_HFRS+
           LTC_12_Severe_Interstitial_Lung_Disease) %>%
  mutate(Any_new = Concordant_new_count + Discordant_new_count) %>%
  mutate(Multi_h_prev=case_when(n_cond_h>=2~2,
                                n_cond_h==1~1,
                                n_cond_h==0~0 )) %>%
  mutate(Multi_prev=case_when(n_cond_12>=2~2,
                              n_cond_12==1~1,
                              n_cond_12==0~0 )) %>%
  mutate(Multi_inc=case_when(Any_new>=2~2,
                             Any_new==1~1,
                             Any_new==0~0 )) %>%
  mutate(multi_status= case_when(Multi_prev <2 ~ "Not multimorbid",
                                 Multi_h_prev == 2 ~ "Already multimorbid",
                                 Multi_h_prev==0 & Multi_prev==2 ~ "Became multimorbid",
                                 Multi_h_prev== 1 & Multi_prev==2 ~ "Became multimorbid")) %>%
  mutate(n_pre_n_post = case_when(n_cond_h==0 & n_cond_12 == 0 ~ "Zero_pre_zero_post",
                                  n_cond_h==0 & n_cond_12 == 1 ~ "Zero_pre_one_post",
                                  n_cond_h==0 & n_cond_12 > 1 ~ "Zero_pre_two_plus_post",
                                  n_cond_h==1 & n_cond_12 == 1 ~ "One_pre_one_post",
                                  n_cond_h==1 & n_cond_12 >= 1 ~ "One_pre_two_plus_post",
                                  n_cond_h>1 & n_cond_12 > 1 ~ "Two_plus_pre_two_plus_post")) %>%
  select(intervention, Multi_h_prev, Multi_prev, Multi_inc, multi_status, n_pre_n_post) 



table_12_h_prev<-table(dat_12M$Multi_h_prev, dat_12M$intervention) 
table_12_prev<- table(dat_12M$Multi_prev, dat_12M$intervention)
table_12_inc<-table(dat_12M$Multi_inc, dat_12M$intervention) 
row_names <- c("Zero", "One", "Two or more")
table_12 <- row_names %>%
  bind_cols(table_12_h_prev,table_12_prev, table_12_inc)

table_12_pre_post <- table(dat_12M$n_pre_n_post, dat_12M$intervention)

multi_test_12 <- dat_12M %>% 
  filter(multi_status == "Not multimorbid" | multi_status ==  "Became multimorbid" ) 

test_table_12 <- table(multi_test_12$multi_status, multi_test_12$intervention)

chisq.test(test_table_12) 

table_for_paper_12 <- as.data.frame.matrix(test_table_12) %>%
  bind_rows(as.data.frame.matrix(table_12_pre_post)) #make table for paper

table_for_paper_12$Multimorbidity_status <- rownames(table_for_paper_12)
rownames(table_for_paper_12) <- NULL  # Remove row names and make values in a variable instead

table_for_paper_12 <- table_for_paper_12  %>%
  select(Multimorbidity_status, `0`, `1`)


desired_order <- c("Not multimorbid", "Became multimorbid",
                   "Zero_pre_zero_post", "Zero_pre_one_post", "Zero_pre_two_plus_post", "One_pre_one_post",
                   "One_pre_two_plus_post", "Two_plus_pre_two_plus_post")

index_order <- match(table_for_paper_12$Multimorbidity_status, desired_order)

table_for_paper_12 <- arrange(table_for_paper_12, index_order)

colnames(table_for_paper_12)[2:3] <- c("Control_12", "Intervention_12")


##### 18M:

exp_var_18M=paste(LTC_var3$LTC_h, collapse = '+')

exp_var_18M=paste(LTC_var3$LTC_18, collapse = '+')

dat_18M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_18mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
mutate(n_cond_h = LTC_Alcohol_Dependence+
         LTC_Asthma+
         LTC_Atrial_Fibrillation+
         LTC_Bronchiectasis+
         LTC_Cancer+
         LTC_Cerebrovascular_Disease+
         LTC_Chronic_Kidney_Disease+
         LTC_Chronic_Liver_Disease+
         LTC_Chronic_Pain+
         LTC_COPD+
         LTC_Coronary_Heart_Disease+
         LTC_Cystic_Fibrosis+
         LTC_Depression+
         LTC_Diabetes+
         LTC_Epilepsy+
         LTC_Heart_Failure+
         LTC_Hypertension+
         LTC_Inflammatory_Bowel_Disease+
         LTC_Multiple_Sclerosis+
         LTC_Osteoarthritis+
         LTC_Osteoporosis+
         LTC_Parkinsons_Disease+
         LTC_Peripheral_Vascular_Disease+
         LTC_Pulmonary_Heart_Disease+
         LTC_Rheumatoid_Arthritis+
         LTC_Sarcoidosis+
         LTC_Serious_Mental_Illness+
         LTC_Sickle_Cell_Disease+
         LTC_Autism+
         LTC_Learning_Disability+
         LTC_Physical_Disability+
         LTC_Dementia+
         LTC_Intermediate_Frailty_Risk_HFRS+
         LTC_High_Frailty_Risk_HFRS+
         LTC_Severe_Interstitial_Lung_Disease)  %>%
  mutate(n_cond_18 = LTC_18_Alcohol_Dependence+
           LTC_18_Asthma+
           LTC_18_Atrial_Fibrillation+
           LTC_18_Bronchiectasis+
           LTC_18_Cancer+
           LTC_18_Cerebrovascular_Disease+
           LTC_18_Chronic_Kidney_Disease+
           LTC_18_Chronic_Liver_Disease+
           LTC_18_Chronic_Pain+
           LTC_18_COPD+
           LTC_18_Coronary_Heart_Disease+
           LTC_18_Cystic_Fibrosis+
           LTC_18_Depression+
           LTC_18_Diabetes+
           LTC_18_Epilepsy+
           LTC_18_Heart_Failure+
           LTC_18_Hypertension+
           LTC_18_Inflammatory_Bowel_Disease+
           LTC_18_Multiple_Sclerosis+
           LTC_18_Osteoarthritis+
           LTC_18_Osteoporosis+
           LTC_18_Parkinsons_Disease+
           LTC_18_Peripheral_Vascular_Disease+
           LTC_18_Pulmonary_Heart_Disease+
           LTC_18_Rheumatoid_Arthritis+
           LTC_18_Sarcoidosis+
           LTC_18_Serious_Mental_Illness+
           LTC_18_Sickle_Cell_Disease+
           LTC_18_Autism+
           LTC_18_Learning_Disability+
           LTC_18_Physical_Disability+
           LTC_18_Dementia+
           LTC_18_Intermediate_Frailty_Risk_HFRS+
           LTC_18_High_Frailty_Risk_HFRS+
           LTC_18_Severe_Interstitial_Lung_Disease) %>%
  mutate(Any_new = Concordant_new_count + Discordant_new_count) %>%
  mutate(Multi_h_prev=case_when(n_cond_h>=2~2,
                                n_cond_h==1~1,
                                n_cond_h==0~0 )) %>%
  mutate(Multi_prev=case_when(n_cond_18>=2~2,
                              n_cond_18==1~1,
                              n_cond_18==0~0 )) %>%
  mutate(Multi_inc=case_when(Any_new>=2~2,
                             Any_new==1~1,
                             Any_new==0~0 ))  %>%
  
  mutate(multi_status= case_when(Multi_prev <2 ~ "Not multimorbid",
                                 Multi_h_prev == 2 ~ "Already multimorbid",
                                 Multi_h_prev==0 & Multi_prev==2 ~ "Became multimorbid",
                                 Multi_h_prev== 1 & Multi_prev==2 ~ "Became multimorbid")) %>%
  mutate(n_pre_n_post = case_when(n_cond_h==0 & n_cond_18 == 0 ~ "Zero_pre_zero_post",
                                  n_cond_h==0 & n_cond_18 == 1 ~ "Zero_pre_one_post",
                                  n_cond_h==0 & n_cond_18 > 1 ~ "Zero_pre_two_plus_post",
                                  n_cond_h==1 & n_cond_18 == 1 ~ "One_pre_one_post",
                                  n_cond_h==1 & n_cond_18 >= 1 ~ "One_pre_two_plus_post",
                                  n_cond_h>1 & n_cond_18 > 1 ~ "Two_plus_pre_two_plus_post")) %>%
  select(intervention, Multi_h_prev, Multi_prev, Multi_inc, multi_status, n_pre_n_post) 

table_18_h_prev<-table(dat_18M$Multi_h_prev, dat_18M$intervention) 
table_18_prev<- table(dat_18M$Multi_prev, dat_18M$intervention)
table_18_inc<-table(dat_18M$Multi_inc, dat_18M$intervention) 
row_names <- c("Zero", "One", "Two or more")
table_18 <- row_names %>%
  bind_cols(table_18_h_prev,table_18_prev, table_18_inc)

table_18_pre_post <- table(dat_18M$n_pre_n_post, dat_18M$intervention)

multi_test_18 <- dat_18M %>% 
  filter(multi_status == "Not multimorbid" | multi_status ==  "Became multimorbid" ) 

test_table_18 <- table(multi_test_18$multi_status, multi_test_18$intervention)

chisq.test(test_table_18) 

table_for_paper_18 <- as.data.frame.matrix(test_table_18) %>%
  bind_rows(as.data.frame.matrix(table_18_pre_post)) #make table for paper

table_for_paper_18$Multimorbidity_status <- rownames(table_for_paper_18)
rownames(table_for_paper_18) <- NULL  # Remove row names and make values in a variable instead

table_for_paper_18 <- table_for_paper_18  %>%
  select(Multimorbidity_status, `0`, `1`)

desired_order <- c("Not multimorbid", "Became multimorbid",
                   "Zero_pre_zero_post", "Zero_pre_one_post", "Zero_pre_two_plus_post", "One_pre_one_post",
                   "One_pre_two_plus_post", "Two_plus_pre_two_plus_post")

index_order <- match(table_for_paper_18$Multimorbidity_status, desired_order)

table_for_paper_18 <- arrange(table_for_paper_18, index_order)

colnames(table_for_paper_18)[2:3] <- c("Control_18", "Intervention_18")

##### 24M:

exp_var_24M=paste(LTC_var3$LTC_h, collapse = '+')

exp_var_24M=paste(LTC_var3$LTC_24, collapse = '+')

dat_24M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_24mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(n_cond_h = LTC_Alcohol_Dependence+
           LTC_Asthma+
           LTC_Atrial_Fibrillation+
           LTC_Bronchiectasis+
           LTC_Cancer+
           LTC_Cerebrovascular_Disease+
           LTC_Chronic_Kidney_Disease+
           LTC_Chronic_Liver_Disease+
           LTC_Chronic_Pain+
           LTC_COPD+
           LTC_Coronary_Heart_Disease+
           LTC_Cystic_Fibrosis+
           LTC_Depression+
           LTC_Diabetes+
           LTC_Epilepsy+
           LTC_Heart_Failure+
           LTC_Hypertension+
           LTC_Inflammatory_Bowel_Disease+
           LTC_Multiple_Sclerosis+
           LTC_Osteoarthritis+
           LTC_Osteoporosis+
           LTC_Parkinsons_Disease+
           LTC_Peripheral_Vascular_Disease+
           LTC_Pulmonary_Heart_Disease+
           LTC_Rheumatoid_Arthritis+
           LTC_Sarcoidosis+
           LTC_Serious_Mental_Illness+
           LTC_Sickle_Cell_Disease+
           LTC_Autism+
           LTC_Learning_Disability+
           LTC_Physical_Disability+
           LTC_Dementia+
           LTC_Intermediate_Frailty_Risk_HFRS+
           LTC_High_Frailty_Risk_HFRS+
           LTC_Severe_Interstitial_Lung_Disease)  %>%
  mutate(n_cond_24 = LTC_24_Alcohol_Dependence+
           LTC_24_Asthma+
           LTC_24_Atrial_Fibrillation+
           LTC_24_Bronchiectasis+
           LTC_24_Cancer+
           LTC_24_Cerebrovascular_Disease+
           LTC_24_Chronic_Kidney_Disease+
           LTC_24_Chronic_Liver_Disease+
           LTC_24_Chronic_Pain+
           LTC_24_COPD+
           LTC_24_Coronary_Heart_Disease+
           LTC_24_Cystic_Fibrosis+
           LTC_24_Depression+
           LTC_24_Diabetes+
           LTC_24_Epilepsy+
           LTC_24_Heart_Failure+
           LTC_24_Hypertension+
           LTC_24_Inflammatory_Bowel_Disease+
           LTC_24_Multiple_Sclerosis+
           LTC_24_Osteoarthritis+
           LTC_24_Osteoporosis+
           LTC_24_Parkinsons_Disease+
           LTC_24_Peripheral_Vascular_Disease+
           LTC_24_Pulmonary_Heart_Disease+
           LTC_24_Rheumatoid_Arthritis+
           LTC_24_Sarcoidosis+
           LTC_24_Serious_Mental_Illness+
           LTC_24_Sickle_Cell_Disease+
           LTC_24_Autism+
           LTC_24_Learning_Disability+
           LTC_24_Physical_Disability+
           LTC_24_Dementia+
           LTC_24_Intermediate_Frailty_Risk_HFRS+
           LTC_24_High_Frailty_Risk_HFRS+
           LTC_24_Severe_Interstitial_Lung_Disease) %>%
  mutate(Any_new = Concordant_new_count + Discordant_new_count) %>%
  mutate(Multi_h_prev=case_when(n_cond_h>=2~2,
                                n_cond_h==1~1,
                                n_cond_h==0~0 )) %>%
  mutate(Multi_prev=case_when(n_cond_24>=2~2,
                              n_cond_24==1~1,
                              n_cond_24==0~0 )) %>%
  mutate(Multi_inc=case_when(Any_new>=2~2,
                             Any_new==1~1,
                             Any_new==0~0 ))  %>%
  
  mutate(multi_status= case_when(Multi_prev <2 ~ "Not multimorbid",
                                 Multi_h_prev == 2 ~ "Already multimorbid",
                                 Multi_h_prev==0 & Multi_prev==2 ~ "Became multimorbid",
                                 Multi_h_prev== 1 & Multi_prev==2 ~ "Became multimorbid")) %>%
  mutate(n_pre_n_post = case_when(n_cond_h==0 & n_cond_24 == 0 ~ "Zero_pre_zero_post",
                                  n_cond_h==0 & n_cond_24 == 1 ~ "Zero_pre_one_post",
                                  n_cond_h==0 & n_cond_24 > 1 ~ "Zero_pre_two_plus_post",
                                  n_cond_h==1 & n_cond_24 == 1 ~ "One_pre_one_post",
                                  n_cond_h==1 & n_cond_24 >= 1 ~ "One_pre_two_plus_post",
                                  n_cond_h>1 & n_cond_24 > 1 ~ "Two_plus_pre_two_plus_post")) %>%
  select(intervention, Multi_h_prev, Multi_prev, Multi_inc, multi_status, n_pre_n_post) 

table_24_h_prev<-table(dat_24M$Multi_h_prev, dat_24M$intervention) 
table_24_prev<- table(dat_24M$Multi_prev, dat_24M$intervention)
table_24_inc<-table(dat_24M$Multi_inc, dat_24M$intervention) 
row_names <- c("Zero", "One", "Two or more")
table_24 <- row_names %>%
  bind_cols(table_24_h_prev,table_24_prev, table_24_inc)

table_24_pre_post <- table(dat_24M$n_pre_n_post, dat_24M$intervention)

multi_test_24 <- dat_24M %>% 
  filter(multi_status == "Not multimorbid" | multi_status ==  "Became multimorbid" ) 

test_table_24 <- table(multi_test_24$multi_status, multi_test_24$intervention)

chisq.test(test_table_24) 

table_for_paper_24 <- as.data.frame.matrix(test_table_24) %>%
  bind_rows(as.data.frame.matrix(table_24_pre_post)) #make table for paper

table_for_paper_24$Multimorbidity_status <- rownames(table_for_paper_24)
rownames(table_for_paper_24) <- NULL  # Remove row names and make values in a variable instead

table_for_paper_24 <- table_for_paper_24  %>%
  select(Multimorbidity_status, `0`, `1`)

desired_order <- c("Not multimorbid", "Became multimorbid",
                   "Zero_pre_zero_post", "Zero_pre_one_post", "Zero_pre_two_plus_post", "One_pre_one_post",
                   "One_pre_two_plus_post", "Two_plus_pre_two_plus_post")

index_order <- match(table_for_paper_24$Multimorbidity_status, desired_order)

table_for_paper_24 <- arrange(table_for_paper_24, index_order)

colnames(table_for_paper_24)[2:3] <- c("Control_24", "Intervention_24")

#Combine 6, 12, 18, 24 month tables together

table_for_paper_all <- table_for_paper_6 %>%
  left_join(table_for_paper_12) %>%
  left_join(table_for_paper_18) %>%
  left_join(table_for_paper_24)

test_all <- test_table_6 %>%
  bind_cols(test_table_12, test_table_18, test_table_24)


write.csv(table_for_paper_all,'N:/Analytics - Improvement Analytics Unit/NDPP evaluation/Output/14_03_24/multi_table/table_for_paper.csv')
write.csv(test_all,'N:/Analytics - Improvement Analytics Unit/NDPP evaluation/Output/14_03_24/multi_table/test_all.csv')

