# NDPP Evaluation            #
# Code author: Paul Chappell using Stefano Conti's matching and forest plot code
#                            #
#                            #
#   Matching                 #
#   Nearest Neighbour        #
#                            #

rm(list=ls())  # Clear work-space

#### Load packages:
library(tidyverse)
library(lubridate)
library(MatchIt)  # Load library enabling Optimal Matching routines
library(optmatch)  # Load library with core Optimal Matching engine


#Read in data:
project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name

load(file.path(project_dir, "R/data/19_April/df_2.RData"))  # Load analysis data-frame)

data_for_matching <- df_2
rm(df_2)

table(data_for_matching$pop_group)

#Read in forest plot function:

source(paste(project_dir, "/R/scripts/PC/functions/forest plot function_19_April.r", sep = ''))

#Let R know which variables are ordered:

df_1 <-  data_for_matching %>%
  mutate(IMD_quintile = as.integer(IMD_quintile)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_condition_count)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  mutate(h_date_quarter = as.character(h_date_quarter))
  
#Select variables for matching

not_for_matching <- c("NHS_Number_Flag", "h_date", "f_date", "provider_name", "ndh_date_valid","IMD_DECILE_PERSON", "GP_ORG_CODE",
                      "GP_NAME", "GP_POST_CODE", "pop_group", "h_date_end_month", "Person_Id", "DATE_OF_DEATH", "DATE_OF_BIRTH", 
                      "LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions")


all_names <- colnames(df_1) 

names_for_matching <- setdiff(all_names, not_for_matching)

#Set propensity score model for exact matching

ps1.frm <- formula(paste("treat ~ ", 
                         paste(names_for_matching, collapse=" + ")))  

ps2.frm <- update(ps1.frm, 
                  new=~ (. - h_date_quarter : h_date_quarter) ) 

print(ps2.frm) 

#####################################
####  Matching for 24 month sample ###
#####################################

#Trim dataset prior to sampling. Create dataset for only 24 mth follow up:

df_24m <- df_1 %>%
  filter(h_date_quarter<2018.1) %>%
  filter(is.na(f_date) | f_date < "2020-01-01")

dim(df_24m)

table(df_24m$pop_group)

#### Loop over the two different analyses: main (control: not referred) and supp (control: referred but didn't start). main and supplementary analyses are swapped in Chappell, Hatfield et al.
controls = c(1,2)

for (control in controls) {
  print(control)
}

for (control in controls) {
 
if (control ==1){
  control_name <- "not_referred" #main analysis. Note: in the publication Chappell, Hatfield et. al, this will be the supplementary analysis
  } else {
    control_name <- "referred_but_didnt_start" #supp analysis. Note: in the publication Chappell, Hatfield et. al, this will be the main analysis
  }


#match control group to group 4:

df_cv4 <- df_24m %>%
  mutate(treat = case_when(pop_group == control ~ 0, 
                           pop_group ==4 ~ 1)) %>%
  drop_na(treat)

#test correct people selected

table(df_cv4$pop_group, df_cv4$treat)

# Derive 1:1 NN Matching output for exact matching:

near2.out <- matchit(ps2.frm, data=df_cv4, method="nearest", 
                  distance="glm", link="logit", estimand="ATT", 
                   exact=~ h_date_quarter, ratio=1) 

# Save matchit object:

saveRDS(near2.out, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_24.RDS", sep ="/" ))

#read in matchit object to save time:

#near2.out <- readRDS(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_24.RDS", sep ="/" ))

#Forest plot prep:
near2_summary <- summary(near2.out)
all_SME <- near2_summary$sum.all[, "Std. Mean Diff."]
matched_SME <- near2_summary$sum.matched[, "Std. Mean Diff."]
SME_matrix_exact <- cbind(original=all_SME, matched=matched_SME)

#Forest plot:
forest_plot_SC(DATA = SME_matrix_exact, period = "twenty_four")

# Display balance statistics for nearest neighbour:

summary(near2.out)  

# Derive matched data-frame for nearest neighbour matching for exact matching:

NN_matched2.dat <- match.data(near2.out, 
                              data=df_cv4, distance="prop.score")

#Test exact matching:
                              
table(NN_matched2.dat$treat, NN_matched2.dat$h_date_quarter)

exact_match_pairs <-NN_matched2.dat %>%
  arrange(subclass) %>%
  mutate(treat = ifelse(treat == 1, "Treatment", "Control")) %>%
  select(subclass, treat, h_date_quarter) %>%
  pivot_wider(names_from = treat, values_from = h_date_quarter)

table(exact_match_pairs$Treatment, exact_match_pairs$Control)

# Save matched data-frame for nearest neighbour matching for exact matching:

write_csv(NN_matched2.dat, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_24mth.csv", sep = "/"))  

}
#####################################
####  Matching for 18 month sample ###
#####################################

rm(list=ls())

#Read in data:

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name


load(file.path(project_dir, "R/data/19_April/df_2.RData"))  # Load analysis data-frame)

data_for_matching <- df_2
rm(df_2)

#Read in forest plot function:

source(paste(project_dir, "/R/scripts/PC/functions/forest plot function_19_April.r", sep = ''))

#Let R know which variables are ordered:

df_1 <-  data_for_matching %>%
  mutate(IMD_quintile = as.integer(IMD_quintile)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_condition_count)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  mutate(h_date_quarter = as.character(h_date_quarter))

#Select variables for matching:

not_for_matching <- c("NHS_Number_Flag", "h_date", "f_date", "provider_name", "ndh_date_valid","IMD_DECILE_PERSON", "GP_ORG_CODE",
                      "GP_NAME", "GP_POST_CODE", "pop_group", "h_date_end_month", "Person_Id", "DATE_OF_DEATH", "DATE_OF_BIRTH", 
                      "LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions")


all_names <- colnames(df_1) 


names_for_matching <- setdiff(all_names, not_for_matching)

#Set propensity score model for exact matching:

ps1.frm <- formula(paste("treat ~ ", 
                         paste(names_for_matching, collapse=" + ")))  

ps2.frm <- update(ps1.frm, 
                  new=~ (. - h_date_quarter : h_date_quarter) ) 

print(ps2.frm) 

controls = c(1,2)

for (control in controls) {
  print(control)
}

for (control in controls) {
  
  if (control ==1){
    control_name <- "not_referred"}
  else {
    control_name <- "referred_but_didnt_start"
  }
  
  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_24mth.csv", sep = "/"))
  

#Create dataset for 18 mth follow-up:

matched_24 <- NN_matched2.dat %>%
  select(NHS_Number_Flag) %>%
  mutate(twenty_four = 1)


df_18m <- df_1 %>%
  filter(h_date_quarter<2018.3) %>%
  filter(is.na(f_date) | f_date < "2020-01-01") %>%
  left_join(matched_24)  %>%
  filter(is.na(twenty_four))


#match control group to group 4:

df_cv4 <- df_18m %>%
  mutate(treat = case_when(pop_group == control ~ 0,
                           pop_group ==4 ~ 1)) %>%
  drop_na(treat)

#test correct people selected:

table(df_cv4$pop_group, df_cv4$treat)

# Derive 1:1 NN Matching output for exact matching:

near2.out <- matchit(ps2.frm, data=df_cv4, method="nearest", 
                     distance="glm", link="logit", estimand="ATT", 
                     exact=~ h_date_quarter, ratio=1) 

# Save matchit object:

saveRDS(near2.out, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_18.RDS", sep ="/" ))

#read in matchit object to save time:

#near2.out <- readRDS(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_18.RDS", sep ="/"))

near2_summary <- summary(near2.out)
all_SME <- near2_summary$sum.all[, "Std. Mean Diff."]
matched_SME <- near2_summary$sum.matched[, "Std. Mean Diff."]
SME_matrix_exact <- cbind(original=all_SME, matched=matched_SME)
#Forest plot:
forest_plot_SC(DATA = SME_matrix_exact, period = "eighteen")

# Display balance statistics for nearest neighbour 

summary(near2.out)  

# Derive matched data-frame for nearest neighbour matching for exact matching

NN_matched2.dat <- match.data(near2.out, 
                              data=df_cv4, distance="prop.score")


#Test exact matching:

table(NN_matched2.dat$treat, NN_matched2.dat$h_date_quarter)

exact_match_pairs <-NN_matched2.dat %>%
  arrange(subclass) %>%
  mutate(treat = ifelse(treat == 1, "Treatment", "Control")) %>%
  select(subclass, treat, h_date_quarter) %>%
  pivot_wider(names_from = treat, values_from = h_date_quarter)

table(exact_match_pairs$Treatment, exact_match_pairs$Control)

# Save matched data-frame for nearest neighbour matching for exact matching

write_csv(NN_matched2.dat, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_18mth.csv", sep = "/"))  

}

#####################################
####  Matching for 12 month sample ###
#####################################

rm(list=ls())

#Read in data
project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name

load(file.path(project_dir, "R/data/19_April/df_2.RData"))  # Load analysis data-frame)

data_for_matching <- df_2
rm(df_2)

#Read in forest plot function

source(paste(project_dir, "/R/scripts/PC/functions/forest plot function_19_April.r", sep = ''))

#Let R know which variables are ordered

df_1 <-  data_for_matching %>%
  mutate(IMD_quintile = as.integer(IMD_quintile)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_condition_count)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  mutate(h_date_quarter = as.character(h_date_quarter))

#Select variables for matching

not_for_matching <- c("NHS_Number_Flag", "h_date", "f_date", "provider_name", "ndh_date_valid","IMD_DECILE_PERSON", "GP_ORG_CODE",
                      "GP_NAME", "GP_POST_CODE", "pop_group", "h_date_end_month", "Person_Id", "DATE_OF_DEATH", "DATE_OF_BIRTH", 
                      "LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions")

all_names <- colnames(df_1) 

names_for_matching <- setdiff(all_names, not_for_matching)

#Set propensity score model for exact matching

ps1.frm <- formula(paste("treat ~ ", 
                         paste(names_for_matching, collapse=" + ")))  

ps2.frm <- update(ps1.frm, 
                  new=~ (. - h_date_quarter : h_date_quarter) ) 

print(ps2.frm) 


controls = c(1,2)

for (control in controls) {
  print(control)
}

for (control in controls) {
  
  if (control ==1){
    control_name <- "not_referred"}
  else {
    control_name <- "referred_but_didnt_start"
  }
  

  #Bring in dataset for 18 mth follow-up:
  
  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_24mth.csv", sep = "/"))
  
  matched_24 <- NN_matched2.dat %>%
    select(NHS_Number_Flag) %>%
    mutate(twenty_four = 1)
  
#Create dataset for 12 mth follow-up:

  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_18mth.csv", sep = "/"))
  
  matched_18 <- NN_matched2.dat %>%
  select(NHS_Number_Flag) %>%
  mutate(eighteen = 1)


df_12m <- df_1 %>%
  filter(h_date_quarter<2019.1) %>%
  filter(is.na(f_date) | f_date < "2020-01-01") %>%
  left_join(matched_24)  %>%
  filter(is.na(twenty_four)) %>%
  left_join(matched_18)  %>%
  filter(is.na(eighteen))


#match control group to group 4:

df_cv4 <- df_12m %>%
  mutate(treat = case_when(pop_group == control ~ 0,
                           pop_group ==4 ~ 1)) %>%
  drop_na(treat)

#test correct people selected:

table(df_cv4$pop_group, df_cv4$treat)

# Derive 1:1 NN Matching output for exact matching:

near2.out <- matchit(ps2.frm, data=df_cv4, method="nearest", 
                     distance="glm", link="logit", estimand="ATT", 
                     exact=~ h_date_quarter, ratio=1) 


# Save matchit object:

saveRDS(near2.out, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_12.RDS", sep ="/" ))

#read in matchit object to save time:

#near2.out <- readRDS(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_12.RDS", sep ="/"))


near2_summary <- summary(near2.out)
all_SME <- near2_summary$sum.all[, "Std. Mean Diff."]
matched_SME <- near2_summary$sum.matched[, "Std. Mean Diff."]
SME_matrix_exact <- cbind(original=all_SME, matched=matched_SME)

#Forest plot:
forest_plot_SC(DATA = SME_matrix_exact, period = "twelve")


# Display balance statistics for nearest neighbour :

summary(near2.out)  

# Derive matched data-frame for nearest neighbour matching for exact matching:

NN_matched2.dat <- match.data(near2.out, 
                              data=df_cv4, distance="prop.score")


#Test exact matching:

table(NN_matched2.dat$treat, NN_matched2.dat$h_date_quarter)

exact_match_pairs <-NN_matched2.dat %>%
  arrange(subclass) %>%
  mutate(treat = ifelse(treat == 1, "Treatment", "Control")) %>%
  select(subclass, treat, h_date_quarter) %>%
  pivot_wider(names_from = treat, values_from = h_date_quarter)

table(exact_match_pairs$Treatment, exact_match_pairs$Control)

# Save matched data-frame for nearest neighbour matching for exact matching:


write_csv(NN_matched2.dat, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_12mth.csv", sep = "/"))  

}


#####################################
####  Matching for 6 month sample ###
#####################################

rm(list=ls())

#Read in data
project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name

load(file.path(project_dir, "R/data/19_April/df_2.RData"))  # Load analysis data-frame)

data_for_matching <- df_2
rm(df_2)
#Read in forest plot function:

source(paste(project_dir, "/R/scripts/PC/functions/forest plot function_19_April.r", sep = ''))

#Let R know which variables are ordered:

df_1 <-  data_for_matching %>%
  mutate(IMD_quintile = as.integer(IMD_quintile)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_condition_count)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  mutate(h_date_quarter = as.character(h_date_quarter))

#Select variables for matching:

not_for_matching <- c("NHS_Number_Flag", "h_date", "f_date", "provider_name", "ndh_date_valid","IMD_DECILE_PERSON", "GP_ORG_CODE",
                      "GP_NAME", "GP_POST_CODE", "pop_group", "h_date_end_month", "Person_Id", "DATE_OF_DEATH", "DATE_OF_BIRTH", 
                      "LTC_condition_count", "LTC_Healthy_Well", "LTC_sum_conditions")


all_names <- colnames(df_1) 


names_for_matching <- setdiff(all_names, not_for_matching)

#Set propensity score model for exact matching:

ps1.frm <- formula(paste("treat ~ ", 
                         paste(names_for_matching, collapse=" + ")))  

ps2.frm <- update(ps1.frm, 
                  new=~ (. - h_date_quarter : h_date_quarter) ) 

print(ps2.frm) 


controls = c(1,2)

for (control in controls) {
  print(control)
}

for (control in controls) {
  
  if (control ==1){
    control_name <- "not_referred"}
  else {
    control_name <- "referred_but_didnt_start"
  }


  #Bring in dataset for 18 mth follow-up:
  
  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_24mth.csv", sep = "/"))
  
  matched_24 <- NN_matched2.dat %>%
    select(NHS_Number_Flag) %>%
    mutate(twenty_four = 1)
  
  
  #Bring in dataset for 12 mth follow-up:
  
  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_18mth.csv", sep = "/"))
  
  
  matched_18 <- NN_matched2.dat %>%
    select(NHS_Number_Flag) %>%
    mutate(eighteen = 1)
  
  
#Create dataset for 6 mth follow-up:

  
  NN_matched2.dat <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_12mth.csv", sep = "/"))
  
matched_12 <- NN_matched2.dat %>%
  select(NHS_Number_Flag) %>%
  mutate(twelve = 1)


df_6m <- df_1 %>%
  filter(h_date_quarter<2019.3) %>%
  filter(is.na(f_date) | f_date < "2019-12-01") %>%
  left_join(matched_24)  %>%
  filter(is.na(twenty_four)) %>%
  left_join(matched_18)  %>%
  filter(is.na(eighteen)) %>%
  left_join(matched_12)  %>%
  filter(is.na(twelve))


#match control group to group 4:

df_cv4 <- df_6m %>%
  mutate(treat = case_when(pop_group == control ~ 0,
                           pop_group ==4 ~ 1)) %>%
  drop_na(treat)

#test correct people selected:

table(df_cv4$pop_group, df_cv4$treat)

# Derive 1:1 NN Matching output for exact matching:

near2.out <- matchit(ps2.frm, data=df_cv4, method="nearest", 
                     distance="glm", link="logit", estimand="ATT", 
                     exact=~ h_date_quarter, ratio=1) 

# Save matchit object:

saveRDS(near2.out, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_6.RDS", sep ="/" ))

#read in matchit object to save time:

#near2.out <- readRDS(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_list_6.RDS", sep ="/"))

near2_summary <- summary(near2.out)
all_SME <- near2_summary$sum.all[, "Std. Mean Diff."]
matched_SME <- near2_summary$sum.matched[, "Std. Mean Diff."]
SME_matrix_exact <- cbind(original=all_SME, matched=matched_SME)

#Forest plot:
forest_plot_SC(DATA = SME_matrix_exact, period = "six")

# Display balance statistics for nearest neighbour:

summary(near2.out)  

# Derive matched data-frame for nearest neighbour matching for exact matching:

NN_matched2.dat <- match.data(near2.out, 
                              data=df_cv4, distance="prop.score")

#Test exact matching:

table(NN_matched2.dat$treat, NN_matched2.dat$h_date_quarter)

exact_match_pairs <-NN_matched2.dat %>%
  arrange(subclass) %>%
  mutate(treat = ifelse(treat == 1, "Treatment", "Control")) %>%
  select(subclass, treat, h_date_quarter) %>%
  pivot_wider(names_from = treat, values_from = h_date_quarter)

table(exact_match_pairs$Treatment, exact_match_pairs$Control)

# Save matched data-frame for nearest neighbour matching for exact matching:

write_csv(NN_matched2.dat, paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_6mth.csv", sep = "/"))  

}

#read in separate datasets:

for (control in controls) {
    
    if (control ==1){
      control_name <- "not_referred"}
    else {
      control_name <- "referred_but_didnt_start"
    }
    

cases_24 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_24mth.csv", sep = "/"))  

cases_18 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_18mth.csv", sep = "/"))  

cases_12 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_12mth.csv", sep = "/"))  

cases_6 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact_6mth.csv", sep = "/"))  


#create one dataset by combining four matched samples:
cases_24 <- cases_24 %>% 
  mutate(source = "twenty_four")
cases_18 <- cases_18 %>% 
  mutate(source = "eighteen") %>%
  select(-twenty_four)
cases_12 <- cases_12 %>% 
  mutate(source = "twelve") %>%
  select(-twenty_four, -eighteen)
cases_6 <- cases_6 %>% 
  mutate(source = "six") %>%
  select(-twenty_four, -eighteen, - twelve)


full_matched_sample <- cases_6 %>% 
  full_join(cases_12) %>%
  full_join(cases_18) %>%
  full_join(cases_24) 

dim(full_matched_sample)


#write out exact_matching dataset:

write_csv(full_matched_sample, (paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact.csv", sep = "/")))  
          
}

#Count frequencies in two datasets:

NDH_matched_5_1 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/not_referred/NN_matched_exact.csv", sep = "/"))
NDH_matched_5_2 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/referred_but_didnt_start/NN_matched_exact.csv", sep = "/"))

table(NDH_matched_5_1$pop_group)
table(NDH_matched_5_2$pop_group)

#Calculate exact size of matching pools:

df_count <- df_1 %>%
  filter(h_date_quarter<2019.3) %>%
  filter(is.na(f_date) | f_date < "2020-01-01")

table(df_count$pop_group)
  