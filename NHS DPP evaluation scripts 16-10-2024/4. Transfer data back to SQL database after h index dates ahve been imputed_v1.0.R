# NDPP Evaluation. Code author: Paul Chappell 

#                            #
#                            #
#   Move data back to     #
#  SQL Analyst machine       #
#     to add matching vars   #

rm(list=ls()) # clear environemnt

####Load packages:
library(odbc) 
library(DBI)
library(tidyverse)

#### Read data from folder and write onto database:

all_dates <- readRDS("N:/Analytics - Improvement Analytics Unit/NDPP evaluation/R/data/19_April/NDHpop_combine1.RDS")

all_dates <- all_dates %>%
  select(nhs_number, h_date, f_date, provider_name,pop_group,ndh_date_valid)  #select subset of variables. These variables needed to merge with  Segmentation dataset in sql to derive variables for matching 

con <- dbConnect(odbc(), dsn = "NCDR") #open connection to Analyst machine on NCDR

DBI::dbWriteTable(con, name = DBI::SQL("[ ].[dbo].[Multimorbidityphase3_PC_IH_after_clean_19_April]"), all_dates, overwrite = TRUE) #write database

odbc::dbDisconnect(con) #close connection

