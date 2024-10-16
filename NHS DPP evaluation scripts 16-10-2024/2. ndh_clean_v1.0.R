# NDPP Evaluation. Code author: Izzy Hatfield 

#                            #
#                            #
#   Explore missing data     #
#  and prepare dataset for   #
#     matching               #

##### load packages:

library(odbc) 
library(DBI)
library(tidyverse)
library(scales)
library(lubridate)
library(dplyr)

rm(list=ls()) # clear environment

##### pulling data from analyst machine@

con <- DBI::dbConnect(odbc(), dsn = "NCDR")
NDHpop <- DBI::dbGetQuery(con, "SELECT * FROM [ ].[dbo].[Multimorbidityphase3_PC_IH_19_April]") 
# option for bringing through only some of the first rows <- DBI::dbGetQuery(con, "SELECT TOP 100000 * FROM [ ].[dbo].[Multimorbidityphase3]")

##### summarise:
NDHpop %>% 
  summarise(count=comma(n()))

NDHpop_clean <- NDHpop %>% 
  
##### formatting all dates ymd:
  
  mutate(across(c(Date_NDPP_NDH_FLAG,
                  Date_ID_Flag,
                  Date_Of_Referral_Receipt,
                  Date_Of_Discharge,
                  n_DATEID,
                  End_dateid,
                  NDHMinDate,
                  n_DATEIDNDH,
                  End_dateidNDH,
                  Date_Reached_IV
  ),
  ~ymd(.x)
  )) %>% 
  
  ##### relabel source of referral:
  
  mutate(Source_of_Referral=recode(Source_of_Referral,
                                   `1` = "GP / NHS Health Check Provider / other HCP",
                                   `2` = "Contact initiated after letter of eligibility or advice from HCP",
                                   `3` = "Self-Referral",
                                   `4` = "Direct recruitment",
                                   `5` = "Transfer from Other Provider",
                                   `6` = "Re-Engaged Referral",
                                   `7` = "Direct to Consumer 2020 - Local Campaign",
                                   `8` = "Direct to Consumer 2020 - National Campaign",
                                   `9` = "GDM (Gestational Diabetes)"
  )) %>%   
  
  ##### drop unnecessary columns and rename consistently:
  
  select(referral_date=Date_Of_Referral_Receipt,
         delivery_mode=`Delivery Mode`,
         referral_source=Source_of_Referral,
         finishers=Finishers_new,
         completers=Completers_New,
         ndh_date_min=NDHMinDate,
         nhs_number=NHS_Number_Flag,
         start_date=Date_Reached_IV,
         provider_name=Provider_Name,
         sex_gr=sex,
         ethnicity_gr=ethnicity,
         imd_rank_gr=imd_rank,
         birth_year_gr=birth_year,
         age_grp_dpp=Age_Group_clean,
         ethnicity_dpp= `Ethnic Group clean`
  ) %>% 
  
  ##### create referral order for multiple referrals:
  
  arrange(referral_date,.by_group = TRUE) %>%
  group_by(nhs_number) %>% 
  mutate(referral_order = row_number(referral_date
  )) %>%
  
  ##### pivot wider so multiple referrals on same row:
  
  pivot_wider(id_cols=c(nhs_number,ndh_date_min),
              names_from=referral_order, 
              values_from =c(referral_date,
                             delivery_mode,
                             referral_source,
                             finishers,
                             completers,
                             start_date,
                             provider_name,
                             sex_gr,
                             ethnicity_gr,
                             imd_rank_gr,
                             birth_year_gr,
                             age_grp_dpp,
                             ethnicity_dpp
              )) %>% 
  ungroup() %>% 
  
  ##### drop cases with more than 1 referral:
  
  filter(is.na(referral_date_2)) %>% 
  select(referral_date=referral_date_1,
         delivery_mode=delivery_mode_1,
         referral_source=referral_source_1,
         finishers=finishers_1,
         completers=completers_1,
         ndh_date_min,
         nhs_number,
         start_date=start_date_1,
         provider_name=provider_name_1,
         sex_gr=sex_gr_1,
         ethnicity_gr=ethnicity_gr_1,
         imd_rank_gr=imd_rank_gr_1,
         birth_year_gr=birth_year_gr_1,
         age_grp_dpp=age_grp_dpp_1,
         ethnicity_dpp=ethnicity_dpp_1
  ) %>% 
  
  ##### define population groups where :
  # 1 - never referred : is.na(referral_date)
  # 2 - referred but never started : is.na(start_date)
  # 3 - started but did not complete : is.na(completers)
  # 4 - completed : !is.na(completers)
  
  mutate(pop_group=case_when(is.na(referral_date)~1,
                             is.na(start_date) ~ 2,
                             is.na(completers)~3,
                             !is.na(completers)~4))

#### summarise:

NDHpop_clean %>% 
  summarise(count=comma(n()))

##### dropping those in treatment group who has no referral date, or start date and no ndh date:
View(NDHpop_clean %>% 
       filter((is.na(ndh_date_min)&is.na(referral_date)&is.na(start_date))))

NDHpop_clean <-NDHpop_clean %>% 
  filter(!(is.na(ndh_date_min)&is.na(referral_date)&is.na(start_date)))

##### summarise:
NDHpop_clean %>% 
  summarise(count=comma(n()))

##### checking for invalid dates:

#start date occurs before referral date:
NDHpop_clean %>% 
  filter(start_date<referral_date) %>% 
  summarise(count=comma(n()))

#Drop these cases:
NDHpop_clean<-NDHpop_clean %>% filter(start_date>=referral_date|
                                        is.na(start_date)|
                                        is.na(referral_date))

##### summarise:
NDHpop_clean %>% 
  summarise(count=comma(n()))

NDHpop_clean %>% 
  group_by(pop_group) %>%
  summarise(count=comma(n()))

saveRDS(NDHpop_clean, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_clean.RDS") #save dataset

#### referral date or start date occurs ebfore NDH diagnosis (ndh_date_min):
NDHpop_clean %>% 
  filter(ndh_date_min>referral_date|ndh_date_min>start_date)%>% 
  summarise(count=comma(n()))

##### create ndh_date_valid - set to NA these invalid ndh_dates:

NDHpop_filter <- NDHpop_clean %>% 
  mutate(ndh_date_valid = case_when(      (is.na(ndh_date_min)|
                                             is.na(referral_date)|
                                             ndh_date_min<=referral_date)
                                          &
                                            (is.na(ndh_date_min)|
                                               is.na(start_date)|
                                               ndh_date_min<=start_date)
                                          ~ndh_date_min)) 

##### check removed ndh_date_valid has 69,396 more nas than ndh_date_min:
NDHpop_filter %>% 
  filter(!is.na(ndh_date_valid))%>% 
  summarise(count=comma(n()))


NDHpop_clean %>% 
  filter(!is.na(ndh_date_min))%>% 
  summarise(count=comma(n()))


##### summarise:
NDHpop_filter %>%  summarise(count=comma(n()))


##### removing valid ndh dates outside study period (keeping missing NDH dates) and calculating number of days between diagnosis, referral and start dates:

NDHpop_filter <- NDHpop_filter %>% 
  filter(ndh_date_valid> '2016-04-01'& ndh_date_valid<'2020-03-01'|is.na(ndh_date_valid)) %>% 
  mutate(diagnosis_to_referral_days=referral_date-ndh_date_valid) %>% 
  mutate(diagnosis_to_start_days=start_date-ndh_date_valid) %>% 
  mutate(referral_to_start_days=start_date-referral_date)

#summarise:
NDHpop_filter %>%  summarise(count=comma(n()))

table(NDHpop_filter$pop_group)

table(NDHpop_filter$ndh_date_valid)

#digital app users:

NDHpop_filter %>% 
       filter(delivery_mode=='Digital') %>% 
       summarise(count=comma(n()))


#remove digital app users:

NDHpop_filter<-NDHpop_filter %>% 
  filter(delivery_mode!='Digital'|is.na(delivery_mode))

NDHpop_filter %>%  summarise(count=comma(n()))

table(NDHpop_filter$pop_group)
saveRDS(NDHpop_filter, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_filter.RDS")
