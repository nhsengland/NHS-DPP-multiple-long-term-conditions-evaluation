#### NDPP Evaluation ####
#Code written by Xiaochen Ge, edits by Paul Chappell #
# LTC incidence analysis #

# Load libraries:

library(jtools)
library(tidyverse)
library(lubridate)

rm(list=ls()) #clear environment

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/"  # Set project folder

source(file.path(project_dir, "R/scripts/XG/prv_inc.r")) #load prevalence -incidence function

names_for_matching<-readRDS(file.path(project_dir, "R/data/19_April/names_for_matching.RDS")) 

#### Only select 35 LTc not including severe versions:
LTC_var<-names_for_matching[str_detect(names_for_matching,'LTC')] #select only var names that are LTCs


strings_to_remove <- c("LTC_Diabetes", "LTC_High_Frailty_Risk_HFRS", "LTC_End_Stage_Renal_Failure" , "LTC_Liver_Failure", 
                       "LTC_Severe_COPD",                    
                      "LTC_Severe_Heart_Failure", "LTC_Incurable_Cancer") #generate vector of var names to remove (because they are diabetes / severe conditions)

LTC_var <- str_remove_all(LTC_var, paste(strings_to_remove, collapse = "|")) #remove the var names from that vector
LTC_var  <- LTC_var [LTC_var != ""] # get rid of empty slots in vector
print(LTC_var) # check have selected correct variables


LTC_var2<-as.data.frame(LTC_var) # change vector to DF

LTC_var3<- LTC_var2 %>%
  separate(LTC_var, into=c('A','cond'), sep='LTC_') %>%
  mutate(base=paste0('LTC','_','f','_',cond)) %>%
  mutate(LTC_6=paste0('LTC','_','6','_',cond)) %>%
  mutate(LTC_12=paste0('LTC','_','12','_',cond)) %>%
  mutate(LTC_18=paste0('LTC','_','18','_',cond)) %>%
  mutate(LTC_24=paste0('LTC','_','24','_',cond)) %>%
  as.data.frame() #create columns for follow-up varaible names
  

#####  Main:

#6M----
dat_6M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(studylength=6) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) 


New_6M <- as.data.frame(ifelse((dat_6M[LTC_var3$LTC_6] - dat_6M[LTC_var3$base])== 1, 1, 0))
colnames(New_6M)<-paste0(colnames(New_6M),'_','new') 
dat_6M2<-cbind(dat_6M,New_6M)


# colnames(New_6M): new LTC

### 6M main ###

#### Descriptive stats for incidence:

dat_6M_con<-dat_6M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3)) #calculates means and sds for classically associated ('concordant') conditions


dat_6M_discon<-dat_6M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3)) # calculates means and sds for not classically associated ('discordant') conditions


mean(dat_6M$Concordant_new_count) 
sd(dat_6M$Concordant_new_count)   
table(dat_6M$Concordant_new_count)

table(dat_6M$Discordant_new_count)

outcome_var_6=c( 'Diabetes_New','Diabetes2_6',
                 'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                 'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                 'Discordant_new_4_plus',
                 colnames(New_6M))

for (outcome in outcome_var_6 )  
{
   prv.inc_iau_fn(endpointName=outcome,
                 design=dat_6M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/main/6M"))
                 
}

#12M----

dat_12M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_12mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(studylength=12) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) 

New_12M <- as.data.frame(ifelse((dat_12M[LTC_var3$LTC_12] - dat_12M[LTC_var3$base])== 1, 1, 0))

New_12M <- as.data.frame(ifelse((dat_12M[LTC_var3$LTC_12] - dat_12M[LTC_var3$base])== 1, 1, 0))
colnames(New_12M)<-paste0(colnames(New_12M),'_','new') 
dat_12M2<-cbind(dat_12M,New_12M)

mean(dat_12M$Concordant_new_count) 
sd(dat_12M$Concordant_new_count)   

dat_12M_con<-dat_12M %>%
  filter(pop_group==2 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_12M_discon<-dat_12M %>%
  filter(pop_group==2 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))


outcome_var_12=c( 'Diabetes_New','Diabetes2_12',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_12M))


for (outcome in outcome_var_12 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_12M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/main/12M"))
  
}

table(dat_12M$Concordant_new_count)

table(dat_12M$Discordant_new_count)

#18M----


dat_18M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_18mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(studylength=18) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) %>%
as.data.frame()   

New_18M <- as.data.frame(ifelse((dat_18M[LTC_var3$LTC_18] - dat_18M[LTC_var3$base])== 1, 1, 0))
colnames(New_18M)<-paste0(colnames(New_18M),'_','new') 
dat_18M2<-cbind(dat_18M,New_18M)


mean(dat_18M$Concordant_new_count) 
sd(dat_18M$Concordant_new_count)   

dat_18M_con<-dat_18M %>%
  filter(pop_group==2 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_18M_discon<-dat_18M %>%
  filter(pop_group==2 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))


outcome_var_18=c( 'Diabetes_New','Diabetes2_18',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_18M))


for (outcome in outcome_var_18 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_18M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/main/18M"))
  
}

table(dat_18M$Concordant_new_count)

table(dat_18M$Discordant_new_count)

#24M----

dat_24M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_24mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==2 | pop_group ==4) %>%
  mutate(studylength=24) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) %>%
as.data.frame()   

New_24M <- as.data.frame(ifelse((dat_24M[LTC_var3$LTC_24] - dat_24M[LTC_var3$base])== 1, 1, 0))
colnames(New_24M)<-paste0(colnames(New_24M),'_','new') 
dat_24M2<-cbind(dat_24M,New_24M)

mean(dat_24M$Concordant_new_count) 
sd(dat_24M$Concordant_new_count)

dat_24M_con<-dat_24M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_24M_discon<-dat_24M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))


outcome_var_24=c( 'Diabetes_New','Diabetes2_24',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_24M))

for (outcome in outcome_var_24 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_24M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/main/24M"))

}

table(dat_24M$Concordant_new_count)

table(dat_24M$Discordant_new_count)

#### Write out lists of variables:
write.csv(outcome_var_6, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_var_6.csv"))
write.csv(outcome_var_12, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_var_12.csv"))
write.csv(outcome_var_18, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_var_18.csv"))
write.csv(outcome_var_24, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_var_24.csv"))

#### Combine count of number of classically associated and not classically associated descriptive stats tables and write out:
all_classically_associated <- dat_6M_con %>%
  bind_rows(dat_12M_con, dat_18M_con, dat_24M_con)

all_not_classically_associated <- dat_6M_discon %>%
  bind_rows(dat_12M_discon, dat_18M_discon, dat_24M_discon)

write.csv(all_classically_associated, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_count_classically_associated.csv"))
write.csv(all_not_classically_associated, file.path(project_dir, "Output/14_03_24/prevalence/main/LTC_count_not_classically_associated.csv"))

#####  Supplementary analysis:

rm(list=ls())
project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/"  # Set project folder

source(file.path(project_dir, "R/scripts/XG/prv_inc.r"))

names_for_matching<-readRDS(file.path(project_dir, "R/data/19_April/names_for_matching.RDS")) 
LTC_var<-names_for_matching[str_detect(names_for_matching,'LTC')]

# Character strings to remove:
strings_to_remove <- c("LTC_Diabetes", "LTC_High_Frailty_Risk_HFRS", "LTC_End_Stage_Renal_Failure" , "LTC_Liver_Failure", 
                       "LTC_Severe_COPD",                    
                       "LTC_Severe_Heart_Failure", "LTC_Incurable_Cancer")

LTC_var <- str_remove_all(LTC_var, paste(strings_to_remove, collapse = "|"))
LTC_var  <- LTC_var [LTC_var != ""]
print(LTC_var)

LTC_var2<-as.data.frame(LTC_var)

LTC_var3<- LTC_var2 %>%
  separate(LTC_var, into=c('A','cond'), sep='LTC_') %>%
  mutate(base=paste0('LTC','_','f','_',cond)) %>%
  mutate(LTC_6=paste0('LTC','_','6','_',cond)) %>%
  mutate(LTC_12=paste0('LTC','_','12','_',cond)) %>%
  mutate(LTC_18=paste0('LTC','_','18','_',cond)) %>%
  mutate(LTC_24=paste0('LTC','_','24','_',cond)) %>%
  as.data.frame()

#6M----
dat_6M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_6mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==1 | pop_group ==4) %>%
  mutate(studylength=6) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) 


New_6M <- as.data.frame(ifelse((dat_6M[LTC_var3$LTC_6] - dat_6M[LTC_var3$base])== 1, 1, 0))
colnames(New_6M)<-paste0(colnames(New_6M),'_','new') 
dat_6M2<-cbind(dat_6M,New_6M)

# colnames(New_6M): new LTC

### 6M:
dat_6M_con<-dat_6M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_6M_discon<-dat_6M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))

mean(dat_6M$Concordant_new_count) 
sd(dat_6M$Concordant_new_count)   
table(dat_6M$Concordant_new_count)

table(dat_6M$Discordant_new_count)

outcome_var_6=c( 'Diabetes_New','Diabetes2_6',
                 'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                 'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                 'Discordant_new_4_plus',
                 colnames(New_6M))

for (outcome in outcome_var_6 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_6M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/supp/6M"))
  
}

#### 12M:

dat_12M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_12mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==1 | pop_group ==4) %>%
  mutate(studylength=12) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) %>%
as.data.frame()   

New_12M <- as.data.frame(ifelse((dat_12M[LTC_var3$LTC_12] - dat_12M[LTC_var3$base])== 1, 1, 0))
colnames(New_12M)<-paste0(colnames(New_12M),'_','new') 
dat_12M2<-cbind(dat_12M,New_12M)


mean(dat_12M$Concordant_new_count) 
sd(dat_12M$Concordant_new_count)  

dat_12M_con<-dat_12M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_12M_discon<-dat_12M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))

outcome_var_12=c( 'Diabetes_New','Diabetes2_12',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_12M))

for (outcome in outcome_var_12 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_12M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/supp/12M"))
  
}


table(dat_12M$Concordant_new_count)
 
table(dat_12M$Discordant_new_count)

#18M:

dat_18M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_18mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==1 | pop_group ==4) %>%
  mutate(studylength=18) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) %>%
as.data.frame()   

New_18M <- as.data.frame(ifelse((dat_18M[LTC_var3$LTC_18] - dat_18M[LTC_var3$base])== 1, 1, 0))
colnames(New_18M)<-paste0(colnames(New_18M),'_','new') 
dat_18M2<-cbind(dat_18M,New_18M)


mean(dat_18M$Concordant_new_count) 
sd(dat_18M$Concordant_new_count)  

dat_18M_con<-dat_18M %>%
  filter(pop_group==1 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))

dat_18M_discon<-dat_18M %>%
  filter(pop_group==1 | pop_group ==4) %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))


outcome_var_18=c( 'Diabetes_New','Diabetes2_18',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_18M))


for (outcome in outcome_var_18 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_18M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/supp/18M"))
  
}


table(dat_18M$Concordant_new_count)

table(dat_18M$Discordant_new_count)

#24M----

dat_24M<-readRDS(file.path(project_dir, "R/data/07_Aug/Outcomes_for_modelling_NDAtype2var/NDH_24mth_outcomes_for_modelling_withType2var.RDS")) %>%
  filter(pop_group==1 | pop_group ==4) %>%
  mutate(studylength=24) %>%
  rename(pid=PERSON_ID) %>%
  mutate(Concordant_new_0=if_else( Concordant_new_count==0,1,0)) %>%
  mutate(Concordant_new_1=if_else( Concordant_new_count==1,1,0)) %>%
  mutate(Concordant_new_2=if_else( Concordant_new_count==2,1,0)) %>%
  mutate(Concordant_new_3=if_else( Concordant_new_count==3,1,0)) %>%
  mutate(Concordant_new_4_plus=if_else( Concordant_new_count>=4,1,0)) %>%
  mutate(Discordant_new_0=if_else( Discordant_new_count==0,1,0)) %>%
  mutate(Discordant_new_1=if_else( Discordant_new_count==1,1,0)) %>%
  mutate(Discordant_new_2=if_else( Discordant_new_count==2,1,0)) %>%
  mutate(Discordant_new_3=if_else( Discordant_new_count==3,1,0)) %>%
  mutate(Discordant_new_4_plus=if_else( Discordant_new_count>=4,1,0)) %>%
as.data.frame()   

New_24M <- as.data.frame(ifelse((dat_24M[LTC_var3$LTC_24] - dat_24M[LTC_var3$base])== 1, 1, 0))
colnames(New_24M)<-paste0(colnames(New_24M),'_','new') 
dat_24M2<-cbind(dat_24M,New_24M)



mean(dat_24M$Concordant_new_count) 
sd(dat_24M$Concordant_new_count)  

dat_24M_con<-dat_24M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Concordant_new_count),3),
            sd=round(sd(Concordant_new_count),3))


dat_24M_discon<-dat_24M %>%
  group_by(intervention) %>%
  summarise(mean=round(mean(Discordant_new_count),3),
            sd=round(sd(Discordant_new_count),3))


outcome_var_24=c( 'Diabetes_New','Diabetes2_24',
                  'Concordant_new_0','Concordant_new_1','Concordant_new_2','Concordant_new_3',
                  'Concordant_new_4_plus', 'Discordant_new_0','Discordant_new_1','Discordant_new_2','Discordant_new_3',
                  'Discordant_new_4_plus',
                  colnames(New_24M))

for (outcome in outcome_var_24 )  
{
  prv.inc_iau_fn(endpointName=outcome,
                 design=dat_24M2,
                 outputPath=file.path(project_dir, "Output/14_03_24/prevalence/supp/24M"))
  
}

table(dat_24M$Concordant_new_count)

table(dat_24M$Discordant_new_count)

# list of variables ----
write.csv(outcome_var_6, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_var_6.csv"))
write.csv(outcome_var_12, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_var_12.csv"))
write.csv(outcome_var_18, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_var_18.csv"))
write.csv(outcome_var_24, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_var_24.csv"))


#### Combine count of number of classically associated and not classically associated descriptive stats tables and write out:
all_classically_associated <- dat_6M_con %>%
  bind_rows(dat_12M_con, dat_18M_con, dat_24M_con)

all_not_classically_associated <- dat_6M_discon %>%
  bind_rows(dat_12M_discon, dat_18M_discon, dat_24M_discon)

write.csv(all_classically_associated, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_count_classically_associated.csv"))
write.csv(all_not_classically_associated, file.path(project_dir, "Output/14_03_24/prevalence/supp/LTC_count_not_classically_associated.csv"))

