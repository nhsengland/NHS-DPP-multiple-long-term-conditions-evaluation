# NDPP Evaluation. Code author: Paul Chappell  

#                            #
#                            #
#   Explore missing data     #
#  and prepare dataset for   #
#     matching               #

#### Install packages, read data: 
library(scales)
library(odbc)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(cowplot)
library(mice)
library(VIM)
library(lubridate)

rm(list=ls()) #Clear environment

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name

con <- DBI::dbConnect(odbc(), dsn = "NCDR", bigint = "num") #open connection to NCDR sql server

#raw_100000 <- DBI::dbGetQuery(con, "SELECT TOP 100000 * FROM [ ].[dbo].[Multimorbidityphase3merge_for_matching]") #test read
raw <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3merge_for_matching_19_April]") #read in full dataset

odbc::dbDisconnect(con)

glimpse(raw)

raw %>% group_by(pop_group) %>% summarise(count=comma(n()))

#### Sort out dates and calculate age variable:

df_1 <- raw %>%
  mutate(Date_Id = as.character(Date_Id)) %>%
  mutate(across(.cols = c(h_date, f_date, ndh_date_valid, h_datePyyMMdd_2, h_date_end_month, DATE_OF_BIRTH, DATE_OF_DEATH, Date_Id), .fns = as_date)) %>%
  mutate(age_h_date = as.numeric(h_date- DATE_OF_BIRTH)/365.25)

hist(df_1$h_date, breaks = 20)

glimpse(df_1)

dim(df_1)


table(df_1$pop_group)


##### Identify dates out of study period:
df_h_dates_out_of_study_period <- df_1 %>%
  filter((h_date < "2016-04-01" | h_date >= "2020-03-01")) %>%
  mutate(before_after = if_else(h_date < "2016-04-01", "before", "after"))

table(df_h_dates_out_of_study_period$before_after, df_h_dates_out_of_study_period$pop_group)

df_2 <- df_1 %>% 
  filter(h_date >= "2016-04-01" & h_date < "2020-03-01")


dim(df_2)

table(df_2$pop_group)

hist(df_2$h_date, breaks = 20)

min(df_2$h_date)

max(df_2$h_date)

#Explore maternal flags:

table(df_2$Maternal_Health_Flag)

table(df_2$Maternal_Health_Flag, df_2$GENDER_PERSON)

table(df_2$Maternal_Health_Flag, df_2$pop_group)

#Filter out pregnant people:

df_3 <- df_2 %>% 
  filter(is.na(Maternal_Health_Flag))

dim(df_3)

table(df_3$pop_group)

# filter out under 18's at h_date:

glimpse(df_3)

hist(df_3$age_h_date)

df_younger_than_18 <- df_3 %>%
  filter(age_h_date<18)

table(df_younger_than_18$pop_group)

df_4 <- df_3 %>% 
  filter(age_h_date>=18)

dim(df_4)

table(df_4$pop_group)


hist(df_4$age_h_date)

#Diabetes exclusions:

table(df_4$LTC_Diabetes)

table(df_4$LTC_Diabetes, df_4$pop_group)

#filter out people with diabetes:

df_5 <- df_4 %>% 
  filter(is.na(LTC_Diabetes))

dim(df_5)

table(df_5$pop_group)

# count and then remove cases where missing data on demographic info:

missing_percent <- function(x) {(sum(is.na(x))/ (length(x)) *100)}
missing_n <- function(x) {sum(is.na(x))}

options(scipen=999)

apply(df_5, 2, missing_percent)
apply(df_5, 2, missing_n)

#Removing missing demographic data:
df_6 <- df_5 %>%
  filter(!is.na(DATE_OF_BIRTH)) %>%
  filter(!is.na(IMD_DECILE_PERSON))

dim(df_6)

table(df_6$pop_group)

apply(df_6, 2, missing_percent)

#Missing data on key segmentation LTC and segment variables:

df_7 <- df_6 %>%
  filter(!is.na(Subsegment_Combination_Id))
         
dim(df_7)

table(df_7$pop_group)

#Missing data on key segmentation dataset GP practice variables:

df_8 <- df_7 %>%
  filter(!is.na(GP_NAME))

dim(df_8)

table(df_8$pop_group)

apply(df_8, 2, missing_percent)
apply(df_8, 2, missing_n)

#### clean up, select variables we need, replace NAs with zeroes in dummy missings. NOTE: doing this to the dummy (indicator) variables does not mean missings were replaced - this is just a necessary part of the data prep process because previously not having a condition is recorded as NA:
LTC_cols <- df_8 %>%
  select(starts_with("LTC_"), -starts_with("LTC_Combination_"))

ltc_names <- colnames(LTC_cols)

df_9 <- df_8 %>%
  select(NHS_Number_Flag, Person_Id, h_date, f_date, provider_name, pop_group, ndh_date_valid, h_date_end_month, age_h_date, DATE_OF_DEATH, DATE_OF_BIRTH, GENDER_PERSON, ETHNICITY_PERSON,
         IMD_QUINTILE_PERSON, IMD_DECILE_PERSON, GP_ORG_CODE, GP_NAME, GP_POST_CODE, all_of(ltc_names)) %>%
  mutate(across(all_of(ltc_names), ~replace_na(., replace = 0)))
  
apply(df_9, 2, missing_percent)
apply(df_9, 2, missing_n)

dim(df_9)

table(df_9$pop_group)

#### Read in and join GP reference file to analysis dataset:

#NOTE: GP reference file holds practice-level variables about GP practices 
# was produced and maintained by the Improvement Analytics Unit based at the Health Foundation and is no longer being regularly updated. 
# For more info email liz.crellin@health.org.uk

gp_ref <- read_csv("C:/Users/PChappell/Documents/NDPP/data/final_ff.csv")

glimpse(gp_ref)

df_gp1 <- gp_ref %>%
  mutate(month = case_when(month == 1 ~ "01",
                           month == 2 ~ "02",
                           month == 3 ~ "03",
                           month == 4 ~ "04",
                           month == 5 ~ "05",
                           month == 6 ~ "06",
                           month == 7 ~ "07",
                           month == 8 ~ "08",
                           month == 9 ~ "09",
                           month == 10 ~ "10",
                           month == 11 ~ "11",
                           month == 12 ~ "12"),
         first_of_month = paste(year, month, "01", sep = ''),
         first_of_month = as_date(first_of_month),
         last_of_month = ceiling_date(first_of_month, "month") - days(1)) %>%
  filter(last_of_month>"2016-03-31" & last_of_month< "2020-03-2020") %>%
  rename(h_date_end_month = last_of_month, GP_ORG_CODE = gpprac) 

hist(df_gp1$h_date_end_month, breaks = 20)

#### Prepare for merge of main df and gp practice variables:

#check min and max h dates are all in study period in both datasets:
min(df_gp1$h_date_end_month)

min(df_9$h_date_end_month)

max(df_gp1$h_date_end_month)

max(df_9$h_date_end_month)

df_10 <- left_join(df_9, df_gp1) %>% #join dfs
  select(NHS_Number_Flag, Person_Id, h_date, f_date, provider_name, pop_group, ndh_date_valid, h_date_end_month, age_h_date, DATE_OF_DEATH, DATE_OF_BIRTH, GENDER_PERSON, ETHNICITY_PERSON,
         IMD_QUINTILE_PERSON, IMD_DECILE_PERSON, GP_ORG_CODE, GP_NAME, GP_POST_CODE, all_of(ltc_names), imdscore15, ru11, qofach_overall, ccg_gp, gp_size, gpfte) %>% #select matching variables
  rename(imdscore15_GP = imdscore15, rural_urban_GP = ru11, qofach_overall_GP= qofach_overall, ccg_GP = ccg_gp, size_GP = gp_size, fte_GP = gpfte) #rename vars

###Check missings introduced by GP variables:
apply(df_10, 2, missing_percent)

apply(df_10, 2, missing_n)

dim(df_10)

table(df_10$pop_group)

df_11 <- df_10 %>%
  filter(!is.na(imdscore15_GP)) %>%
  filter(!is.na(rural_urban_GP)) %>%
  filter(!is.na(qofach_overall_GP)) %>%
  filter(!is.na(ccg_GP)) %>%
  filter(!is.na(size_GP)) %>%
  filter(!is.na(fte_GP))

dim(df_11)

table(df_11$pop_group)

#### clean up variable names, value names and assign correct variable types in preparation for matching:

df_12 <-  df_11 %>%
  mutate(age = as.integer(floor(age_h_date))) %>%
  mutate(gender = case_when(GENDER_PERSON== 'MALE' ~ 'Male',
                            GENDER_PERSON == 'FEMALE' ~ 'Female')) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(IMD_quintile = as.integer(IMD_QUINTILE_PERSON)) %>%
  mutate(ethnicity = case_when(ETHNICITY_PERSON == 'Black' ~ 'Black or Black British',
                               ETHNICITY_PERSON == 'Black or Black British' ~ 'Black or Black British',
                               ETHNICITY_PERSON == 'Asian' ~ 'Asian or Asian British',
                               ETHNICITY_PERSON == 'Asian or Asian British' ~ 'Asian or Asian British',
                               ETHNICITY_PERSON == 'Conflicted' ~ 'Mixed',
                               ETHNICITY_PERSON == 'Mixed' ~ 'Mixed',
                               ETHNICITY_PERSON == 'Mixed' ~ 'Mixed',
                               ETHNICITY_PERSON == 'Not Known' ~ 'Unknown',
                               ETHNICITY_PERSON == 'Other Ethnic Groups' ~ 'Other',
                               ETHNICITY_PERSON == 'White' ~ 'White')) %>%
  mutate(ethnicity = as.factor(ethnicity)) %>%
  mutate(IMD_quintile_GP =  ntile(imdscore15_GP, 5)) %>%
  mutate(IMD_quintile_GP = as.integer(IMD_quintile_GP)) %>%
  mutate(LTC_condition_count = as.integer(LTC_CONDITION_COUNT)) %>%
  mutate(QOF_overall_quintile_GP =  ntile(qofach_overall_GP , 5)) %>%
  mutate(QOF_overall_quintile_GP = as.integer(QOF_overall_quintile_GP)) %>%
  mutate(ccg_GP = as.factor(ccg_GP)) %>%
  mutate(size_quintile_GP = ntile(size_GP , 5)) %>%
  mutate(size_quintile_GP = as.integer(size_quintile_GP)) %>%
  mutate(fte_quintile_GP = ntile(fte_GP, 5)) %>%
  mutate(fte_quintile_GP = as.integer(fte_quintile_GP)) %>%
  select(-c(fte_GP, imdscore15_GP, size_GP, qofach_overall_GP, LTC_CONDITION_COUNT, 
            IMD_QUINTILE_PERSON, ETHNICITY_PERSON,
            GENDER_PERSON, age_h_date, LTC_CONDITION_COUNT, LTC_Maternal_and_Infant_Health)) %>%
  filter(gender == 'Male' | gender == "Female") %>% #remove 8 people of 'indeterminate' gender # 
  mutate(h_date_quarter = quarter(h_date, with_year = TRUE)) %>%
  mutate(LTC_sum_conditions = rowSums(select(., c(starts_with("LTC_"), - LTC_condition_count, -LTC_Healthy_Well))))

#Check if LTC_CONDITION_COUNT is the same as sum of LTCs:

cor(df_12$LTC_sum_conditions, df_12$LTC_condition_count)

#Read in and join secondary care usage variable:

NDH_sec_care_attendances<-readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/scripts/IH/Secondary care usage/19_April/NDH_sec_care_attendances.RDS")

NDH_sec_care_attendances <- NDH_sec_care_attendances %>%
  mutate(nhs_number = as.numeric(nhs_number))

df_13 <- df_12 %>% mutate(NHS_Number_Flag = as.numeric(NHS_Number_Flag)) %>%
left_join(NDH_sec_care_attendances, by = c("NHS_Number_Flag" = "nhs_number")) %>% # join secondary care vars to main dataset
  replace_na(list(arrivals=0, admissions=0, appointments=0)) 

#Save ready for predicted probability calculation of completion before matching:

dim(df_13)

df_out <- df_13

table(df_13$pop_group)

save(df_out, 
     file=file.path(project_dir, "R/data/19_April/cleaned_data_for_pred_prob.RData")
)  # Save cleaned analysis data-frame

