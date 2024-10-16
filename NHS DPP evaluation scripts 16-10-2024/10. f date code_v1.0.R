# NDPP Evaluation            #
#Code written by Izzy Hatfield & Paul Chappell #
#                            #
#                            #
#   Insert f dates            #
#   after matching           #
#                            #

rm(list=ls()) # clear environment

#### Load packages:
library(tidyverse)
library(dplyr)
library(scales)
library(tidyr)
library(vctrs)
library(lubridate)

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name


#### Open loop to run everything for (1) where control is not referred and (2) control is referred but didn;t start:
controls = c(1,2)

for (control in controls) {
  
  if (control ==1){
    control_name <- "not_referred"}
  else {
    control_name <- "referred_but_didnt_start"
  }
  
#### read in files:
  
NDH_matched_5_1 <- read_csv(paste("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April", control_name, "NN_matched_exact.csv", sep = "/"))
 

NDH_matched_5_1 <- NDH_matched_5_1 %>%
  arrange(NHS_Number_Flag)

glimpse(NDH_matched_5_1)

#### Separate treatment and control groups into separate dataframes, create hf_date which equals lag from referral to start date:

NDH_treat<-NDH_matched_5_1 %>% filter(pop_group==4) %>% mutate(hf_date=f_date-h_date)

NDH_control<-NDH_matched_5_1 %>% filter(pop_group==control)

#### Join treatment and control dfs so that each pair of matches is on the same row:
NDH_matched_wide <- NDH_treat %>%
  left_join(NDH_control, by=c("subclass", "source"))

#Make variable that identifies each pair - called subclass2:

NDH_matched_wide$subclass2 <- seq(1, length(NDH_matched_wide$subclass), 1)

Pairs_plus_unique_ID <- NDH_matched_wide %>%
  select(NHS_Number_Flag.y, NHS_Number_Flag.x, subclass, source, subclass2)

treat_plus_unique_ID <- NDH_matched_wide %>%
  select(NHS_Number_Flag.x, subclass2) %>%
  rename(NHS_Number_Flag = NHS_Number_Flag.x)

control_plus_unique_ID <- NDH_matched_wide %>%
  select(NHS_Number_Flag.y, subclass2) %>%
  rename(NHS_Number_Flag = NHS_Number_Flag.y)


#create control_f_date variable using h date plus treat lag between h and f:

NDH_matched_wide<-NDH_matched_wide %>% mutate(control_f_date=h_date.y+hf_date)

#checking that lags and dates are same in paired control and treated units:

NDH_matched_wide<-NDH_matched_wide %>% mutate(hf_date_control=control_f_date-h_date.y) %>% mutate(treat_control_f_diff=control_f_date-f_date.x)
look_5_1<-NDH_matched_5_1 %>% arrange(subclass) %>% select(pop_group, treat,subclass,h_date,f_date)
NDH_matched_5_1 %>% group_by(pop_group) %>%  summarise(count=comma(n()))

# selecting, renaming and adding variables to be consistent between treat and control dfs ready to bind back together:

NDH_control_f <- NDH_matched_wide %>%
  select(NHS_Number_Flag.y:weights.y, subclass, source, subclass2, hf_date_control, control_f_date)

NDH_control_f <- NDH_control_f %>%
  dplyr::rename_all(.funs=funs(sub("\\..*","",names(NDH_control_f))))

NDH_control_f <- NDH_control_f %>%
  rename(hf_date=hf_date_control)

NDH_treat_f<-NDH_matched_wide %>%
  select(NHS_Number_Flag.x:weights.x, subclass, source, subclass2, hf_date)

NDH_treat_f<-NDH_treat_f %>%
  dplyr::rename_all(.funs=funs(sub("\\..*","",names(NDH_treat_f))))

NDH_treat_f<-NDH_treat_f %>% mutate(control_f_date=NA)

#bind control and treat dfs into a long dataset:

NDH_all_f_dates<-rbind(NDH_control_f,NDH_treat_f) 

NDH_all_f_dates <- NDH_all_f_dates %>%
  arrange(subclass2)


#create new_f_date variable for all units:

NDH_all_f_dates<-NDH_all_f_dates %>%
  mutate(new_f_date=case_when(is.na(f_date)~control_f_date,
                              !is.na(f_date)~f_date))

# apply cut off date binary variables:
NDH_all_f_dates<-NDH_all_f_dates %>% 
  mutate(mth6=case_when((
    h_date>= '2016-04-01'& new_f_date<'2019-09-01')~1,
    TRUE~0)) %>% 
  mutate(mth12=case_when((
    h_date>= '2016-04-01'& new_f_date<'2019-03-01')~1,
    TRUE~0)) %>%  
  mutate(mth18=case_when((
      h_date>= '2016-04-01'& new_f_date<'2018-09-01')~1,
      TRUE~0)) %>% 
  mutate(mth24=case_when((
    h_date>= '2016-04-01'& new_f_date<'2018-03-01')~1,
    TRUE~0))

#applying follow up period cutoff dates:

NDH_6mth<- NDH_all_f_dates %>% filter(h_date>= '2016-04-01'& new_f_date<'2019-09-01')
NDH_12mth<- NDH_all_f_dates  %>% filter(h_date>= '2016-04-01'& new_f_date<'2019-03-01')
NDH_18mth<- NDH_all_f_dates  %>% filter(h_date>= '2016-04-01'& new_f_date<'2018-09-01')
NDH_24mth<- NDH_all_f_dates  %>% filter(h_date>= '2016-04-01'& new_f_date<'2018-03-01')


#save files

saveRDS(NDH_all_f_dates, file=file.path(paste(project_dir, "R/data/19_April", control_name, "NDH_all_f_dates1.RDS", sep = "/")))
saveRDS(NDH_6mth, file=file.path(paste(project_dir, "R/data/19_April", control_name, "NDH_6mth1.RDS", sep = "/")))
saveRDS(NDH_12mth, file=file.path(paste(project_dir, "R/data/19_April", control_name, "NDH_12mth1.RDS", sep = "/")))
saveRDS(NDH_18mth, file=file.path(paste(project_dir, "R/data/19_April", control_name, "NDH_18mth1.RDS", sep = "/")))
saveRDS(NDH_24mth, file=file.path(paste(project_dir, "R/data/19_April", control_name, "NDH_24mth1.RDS", sep = "/")))

}

#Adding id count variables to check whether pairs remain paired or have become dangling:

NDH_all_f_dates<-NDH_all_f_dates %>%
  group_by(subclass) %>%
  mutate(
    id_count = n()) %>%
  ungroup()

NDH_6mth<-NDH_6mth %>%
  group_by(subclass) %>%
  mutate(
    id_count = n())%>%
  ungroup()

NDH_12mth<-NDH_12mth %>%
  group_by(subclass) %>%
  mutate(
    id_count = n())%>%
  ungroup()

NDH_18mth<-NDH_18mth %>%
  group_by(subclass) %>%
  mutate(
    id_count = n())%>%
  ungroup()

NDH_24mth<-NDH_24mth %>%
  group_by(subclass) %>%
  mutate(
    id_count = n())%>%
  ungroup()


NDH_all_f_dates %>% group_by(id_count,treat) %>% summarise(count=comma(n()))
NDH_6mth %>% group_by(id_count,treat) %>% summarise(count=comma(n()))
NDH_12mth %>% group_by(id_count,treat) %>% summarise(count=comma(n()))
NDH_18mth %>% group_by(id_count,treat) %>% summarise(count=comma(n()))
NDH_24mth %>% group_by(id_count,treat) %>% summarise(count=comma(n()))


#### Distribution of f dates by year:

NDH_all_f_dates <- NDH_all_f_dates %>% mutate(hdate_year=year(h_date)) 
NDH_6mth<-NDH_6mth %>% mutate(hdate_year=year(h_date))

NDH_all_f_dates %>% group_by(hdate_year) %>% summarise(median = median(hf_date, na.rm = TRUE)) %>% ungroup()
NDH_all_f_dates %>% group_by(hdate_year,treat) %>% summarise(median = median(hf_date, na.rm = TRUE)) %>% ungroup()

NDH_6mth %>% group_by(hdate_year) %>% summarise(median = median(hf_date, na.rm = TRUE)) %>% ungroup()

median(NDH_6mth$hf_date)

#Combine two datasets together:

rm(list=ls())

project_dir <- "N:/Analytics - Improvement Analytics Unit/NDPP evaluation"  # Set project directory name

NDH_all_f_dates_1 <- readRDS(file.path(project_dir, "R/data/19_April/not_referred/NDH_all_f_dates.RDS"))
NDH_6mth_1 <- readRDS(file.path(project_dir, "R/data/19_April/not_referred/NDH_6mth.RDS"))
NDH_12mth_1 <- readRDS(file.path(project_dir, "R/data/19_April/not_referred/NDH_12mth.RDS"))
NDH_18mth_1 <- readRDS(file.path(project_dir, "R/data/19_April/not_referred/NDH_18mth.RDS"))
NDH_24mth_1 <- readRDS(file.path(project_dir, "R/data/19_April/not_referred/NDH_24mth.RDS"))

NDH_all_f_dates_2 <- readRDS(file.path(project_dir, "R/data/19_April/referred_but_didnt_start/NDH_all_f_dates.RDS"))
NDH_6mth_2 <- readRDS(file.path(project_dir, "R/data/19_April/referred_but_didnt_start/NDH_6mth.RDS"))
NDH_12mth_2 <- readRDS(file.path(project_dir, "R/data/19_April/referred_but_didnt_start/NDH_12mth.RDS"))
NDH_18mth_2 <- readRDS(file.path(project_dir, "R/data/19_April/referred_but_didnt_start/NDH_18mth.RDS"))
NDH_24mth_2 <- readRDS(file.path(project_dir, "R/data/19_April/referred_but_didnt_start/NDH_24mth.RDS"))

#lists of which datasets are main and which are supp (Note: main and supp are switched in later analyses):
mains <- list(NDH_all_f_dates_1, NDH_6mth_1, NDH_12mth_1, NDH_18mth_1, NDH_24mth_1)
supps <- list(NDH_all_f_dates_1, NDH_6mth_1, NDH_12mth_1, NDH_18mth_1, NDH_24mth_1)

label_as_main <- function(dataset) { #function that makes all values in variable main_or_supp to 'main' 
  dataset <- dataset %>%
    mutate(main_or_supp = "main")
}

NDH_all_f_dates_1 <- label_as_main(NDH_all_f_dates_1) #apply the function to appropriate datasets
NDH_6mth_1 <- label_as_main(NDH_6mth_1) 
NDH_12mth_1 <- label_as_main(NDH_12mth_1) 
NDH_18mth_1 <- label_as_main(NDH_18mth_1) 
NDH_24mth_1 <- label_as_main(NDH_24mth_1) 

label_as_supp <- function(dataset) { # #function that makes all values in variable main_or_supp to 'supp'
  dataset <- dataset %>%
    mutate(main_or_supp = "supp")
}

#apply the function to appropriate datasets:

NDH_all_f_dates_2 <- label_as_supp(NDH_all_f_dates_2) 
NDH_6mth_2 <- label_as_supp(NDH_6mth_2) 
NDH_12mth_2 <- label_as_supp(NDH_12mth_2) 
NDH_18mth_2 <- label_as_supp(NDH_18mth_2) 
NDH_24mth_2 <- label_as_supp(NDH_24mth_2) 


bind_them <- function(a,b) { #bind them function
  bind_rows(a, b)
}

#apply the function to appropriate datasets:

NDH_all_f_dates <- bind_them(NDH_all_f_dates_1, NDH_all_f_dates_2) 
NDH_6mth <- bind_them(NDH_6mth_1, NDH_6mth_2)
NDH_12mth <- bind_them(NDH_12mth_1, NDH_12mth_2)
NDH_18mth <- bind_them(NDH_18mth_1, NDH_18mth_2)
NDH_24mth <- bind_them(NDH_24mth_1, NDH_24mth_2)

#test missings:
missing_n <- function(x) {sum(is.na(x))}
apply(NDH_all_f_dates, 2, missing_n)
apply(NDH_all_f_dates_1, 2, missing_n)
apply(NDH_all_f_dates_2, 2, missing_n)
apply(NDH_6mth, 2, missing_n)
apply(NDH_12mth, 2, missing_n)
apply(NDH_18mth, 2, missing_n)
apply(NDH_24mth, 2, missing_n)

#write out datasets. These datasets include intervention group plus two different control groups
saveRDS(NDH_all_f_dates, file=file.path(project_dir, "R/data/19_April/NDH_all_f_dates.RDS"))
saveRDS(NDH_6mth, file=file.path(project_dir, "R/data/19_April/NDH_6mth.RDS"))
saveRDS(NDH_12mth, file=file.path(project_dir, "R/data/19_April/NDH_12mth.RDS"))
saveRDS(NDH_18mth, file=file.path(project_dir, "R/data/19_April/NDH_18mth.RDS"))
saveRDS(NDH_24mth, file=file.path(project_dir, "R/data/19_April/NDH_24mth.RDS"))
