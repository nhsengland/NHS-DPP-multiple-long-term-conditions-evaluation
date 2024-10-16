# NDPP Evaluation. Code author: Izzy Hatfield, edits by Paul Chappell

#                            #
#                            #
#   Assigning h dates and    #
#  f dates to cleaned dataset#
#                            #
 
#### Notes: 
#H dates are baseline dates (date of NDH diagnosis) from the NDA dataset. We also estimate some of these dates where missing

#f dates are the dates from which we start following up. f date is the date that individuals started or would have started . 
# The gap ('lag') between referral and dates varied in length of number of days.

#Some data is missing. In this script we estimate / impute dates from other dates by drawing values randomly from the full distribution of lags 
# and adding / subtracting this lag from the appropriate date that we do have.

rm(list=ls()) #clear environment

#### Load packages:

library(scales)
library(tidyverse)
library(lubridate)

NDHpop_filter<- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_filter.RDS")

NDHpop_filter %>% 
  group_by(delivery_mode) %>% 
  summarise(count=comma(n()))

NDHpop_filter %>% 
  group_by(pop_group) %>% 
  summarise(count=comma(n()))

##### calculating the number of people who need imputed f and h dates:

NDHpop_filter %>% group_by(is.na(ndh_date_valid),is.na(referral_date),is.na(start_date)) %>% summarise(count=comma(n()))

#### idenfity people with a valid referral date and start date. This code is to create a dataset (NDHpop_rs) with all the valid lags between referral to start date:
NDHpop_rs <- NDHpop_filter %>% 
  filter(!is.na(referral_date) & !is.na(start_date))

NDHpop_rs %>% 
  group_by(pop_group) %>% 
  summarise(count=comma(n())) #count in each pop group

  NDHpop_rs %>% 
    ggplot(data=.)+
    geom_bar(aes(referral_to_start_days,fill=factor(pop_group)))+
    scale_x_continuous()+
    scale_y_continuous(name="patients") #plot lags by whether in completed less than 60% or more than 60% 


#### find and impute f dates for people who have a referral date:
  
group2_need_f_date <- NDHpop_filter %>%
  filter(!is.na(referral_date)) %>% 
  filter(is.na(start_date)) %>%
  select(nhs_number) #create dataset of people who need f date

n_to_impute_f_from_referral <- nrow(group2_need_f_date) #count number of individuals who need f date but have referral date
 
referral_to_start_distribution <- NDHpop_rs$referral_to_start_days # vector with all the lags between referral and start dates
referral_to_start_sample <-  as.numeric(sample(referral_to_start_distribution,n_to_impute_f_from_referral,replace=TRUE)) #sample randomly from lags vector into a vector the same length as number of people who need an f date estaimted from a referral date

assigned_referral_to_start_lag <- group2_need_f_date %>%  bind_cols(referral_to_start_sample) #bind the randomly sampled lags onto the dataset of people who need f date

group2_f_dates <- NDHpop_filter %>% 
  left_join(assigned_referral_to_start_lag) %>% 
  mutate(group2_fdate=referral_date+...2) # join dataset with lags back onto dataset with all variables and calculate estimated dates by adding lags onto referral date 


#### people with a valid referral date and ndh date:

NDHpop_hr <- NDHpop_filter %>% 
  filter(!is.na(ndh_date_valid) & !is.na(referral_date))

#Graphs to exlore distriubution of lags across pop groups:

NDHpop_hr %>% 
  ggplot(data=.)+
  geom_bar(aes(diagnosis_to_referral_days,fill=factor(pop_group)))+
  scale_x_continuous()+
  scale_y_continuous(limits=c(0,10000),
                     name="patients") 

NDHpop_hr %>% 
  ggplot(data=.)+
  geom_bar(aes(diagnosis_to_referral_days,fill=factor(pop_group)))+
  scale_x_continuous(limits=c(0,1000))+
  scale_y_continuous(limits=c(0,5000),
                     name="patients")

NDHpop_hr %>% 
  filter(diagnosis_to_referral_days<50) %>% 
  summarise(count=comma(n()))


####people who need an h date, and were referred to programme. Estimating these dates based on diagnosis to referral lags:

group234_need_h_date <- NDHpop_filter %>%
  filter(!is.na(referral_date)) %>% 
  filter(is.na(ndh_date_valid)) %>%
  select(nhs_number)

n_to_impute_h_from_referral <- nrow(group234_need_h_date) #count number of individuals who need h date but have referral date

diagnosis_to_referral_distribution <- NDHpop_hr$diagnosis_to_referral_days # vector with all the lags between diagnosis and referral dates
diagnosis_to_referral_sample <-  as.numeric(sample(diagnosis_to_referral_distribution,n_to_impute_h_from_referral,replace=TRUE))  #sample randomly from lags vector into a vector the same length as number of people who need an h date estaimted from a referral date

assigned_diagnosis_to_referral_lag <- group234_need_h_date %>%  bind_cols(diagnosis_to_referral_sample) #bind the randomly sampled lags onto the dataset of people who need an h date

group234_h_dates <- NDHpop_filter %>% 
  left_join(assigned_diagnosis_to_referral_lag) %>% 
  mutate(group234_h_date=referral_date-...2) #calculate estimated h dates

#### combine all in to one data set:

group2_f_dates<-group2_f_dates %>% select(group2_fdate,nhs_number)
group234_h_dates<-group234_h_dates %>% select(group234_h_date,nhs_number)

NDHpop_combine <- NDHpop_filter %>% 
  left_join(group2_f_dates) %>% 
  left_join(group234_h_dates) # join on estimated dates to big dataset

NDHpop_combine1 <- NDHpop_combine %>% 
  mutate(h_date=case_when(
    !is.na(ndh_date_valid)~ndh_date_valid,
    !is.na(group234_h_date)~group234_h_date)) %>% 
  mutate(f_date=case_when(
    !is.na(start_date)~start_date,
    !is.na(group2_fdate)~group2_fdate)) # create new h date and f date variables where if the date was orinally missing it is replaced with the new estimated date
  
 NDHpop_combine1 %>% filter(is.na(f_date))%>% summarise(count=comma(n()))
 NDHpop_combine1 %>% filter(is.na(h_date))%>% summarise(count=comma(n()))

 saveRDS(NDHpop_combine1, "N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_combine1.RDS") #save dataset

 
 #NDHpop_combine1 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_combine1.RDS") #read this in to run the below analyses without all the above
 NDHpop_6mth<- NDHpop_combine1 %>% filter(h_date>= '2016-04-01'& f_date<'2019-09-01'|is.na(f_date)) # select dataset that shows only people who will be in 6 month follow-up cohort
 NDHpop_12mth<- NDHpop_combine1 %>% filter(h_date>= '2016-04-01'& f_date<'2019-03-01'|is.na(f_date)) # select dataset that shows only people who will be in 12 month cohort
 NDHpop_18mth<- NDHpop_combine1 %>% filter(h_date>= '2016-04-01'& f_date<'2018-09-01'|is.na(f_date)) # select dataset that shows only people who will be in 28 month follow-up cohort
 
 #####Examine numbers of cases in different groups, distributed over time, in different follow-up cohorts:
 
 NDHpop_combine1 %>% group_by(pop_group) %>% summarise(count=comma(n()))
 
 NDHpop_6mth %>% group_by(pop_group) %>% summarise(count=comma(n()))
 NDHpop_12mth %>% group_by(pop_group) %>% summarise(count=comma(n()))
 NDHpop_18mth %>% group_by(pop_group) %>% summarise(count=comma(n()))
 group234_need_h_date %>% group_by(pop_group) %>% summarise(count=comma(n()))
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(ndh_date_valid,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(limits=c(0,3500),name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(group234_h_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(h_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(limits=c(0,3500),name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(referral_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(start_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(group2_fdate,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(f_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1<-NDHpop_combine1 %>% mutate(h_to_fdate=f_date-h_date)
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(h_to_fdate,fill=factor(pop_group)))+
   scale_x_continuous()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(diagnosis_to_start_days,fill=factor(pop_group)))+
   scale_x_continuous()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(referral_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(limits=c(0,3500),name="patients")
 
 NDHpop_combine1 %>%
   ggplot(data=.)+
   geom_bar(aes(start_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(limits=c(0,3500),name="patients")
 
 NDHpop_combine1<-NDHpop_6mth %>% mutate(h_to_fdate=f_date-h_date)
 
 NDHpop_6mth %>%
   ggplot(data=.)+
   geom_bar(aes(h_to_fdate,fill=factor(pop_group)))+
   scale_x_continuous()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(h_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 
 NDHpop_combine1 %>% 
   ggplot(data=.)+
   geom_bar(aes(group234_h_date,fill=factor(pop_group)))+
   scale_x_date()+
   scale_y_continuous(name="patients")
 

 max(NDHpop_combine1$group234_h_date,na.rm=TRUE)
 
 