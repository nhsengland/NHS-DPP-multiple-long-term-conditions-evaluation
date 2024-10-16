# NDPP Evaluation               #
#Code written by Paul Chappell  #
#                               #
#                               #
#   Prepare to Transfer data    #
#   back to SQL for outcomes    #
#                               #

#### load pacakges:

library(odbc)
library(DBI)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(cowplot)
library(csvread)

rm(list=ls()) #clear environment

#Prepare data for collecting outcomes from the segmentation dataset:

NDH_6 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_6mth.RDS")
NDH_12 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_12mth.RDS")
NDH_18 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_18mth.RDS")
NDH_24 <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDH_24mth.RDS")

#Add numbers of months to dates, convert NHS_number to int64 

outcomes_6 <- NDH_6 %>%
  mutate(f_date_plus_6 = new_f_date %m+% months(6)) %>%
  mutate(NHS_Number= as.int64(NHS_Number_Flag)) %>%
  select(NHS_Number, Person_Id, new_f_date, f_date_plus_6)

outcomes_12 <- NDH_12 %>%
  mutate(f_date_plus_12 = new_f_date %m+% months(12)) %>%
  mutate(NHS_Number= as.int64(NHS_Number_Flag)) %>%
  select(NHS_Number, Person_Id, new_f_date, f_date_plus_12)

outcomes_18 <- NDH_18 %>%
  mutate(f_date_plus_18 = new_f_date %m+% months(18)) %>%
  mutate(NHS_Number= as.int64(NHS_Number_Flag)) %>%
  select(NHS_Number, Person_Id, new_f_date, f_date_plus_18)

outcomes_24 <- NDH_24 %>%
  mutate(f_date_plus_24 = new_f_date %m+% months(24)) %>%
  mutate(NHS_Number= as.int64(NHS_Number_Flag)) %>%
  select(NHS_Number, Person_Id, new_f_date, f_date_plus_24)

#Open connection to sql server, write to sql server, close sql server:

con <- dbConnect(odbc(), dsn = "NCDR")

DBI::dbWriteTable(con, name = DBI::SQL("[ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_6M_19_April]"), outcomes_6, overwrite = TRUE)
DBI::dbWriteTable(con, name = DBI::SQL("[ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_12M_19_April]"), outcomes_12, overwrite = TRUE)
DBI::dbWriteTable(con, name = DBI::SQL("[ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_18M_19_April]"), outcomes_18, overwrite = TRUE)
DBI::dbWriteTable(con, name = DBI::SQL("[ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_24M_19_April]"), outcomes_24, overwrite = TRUE)
odbc::dbDisconnect(con)

  
