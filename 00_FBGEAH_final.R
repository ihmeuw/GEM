#############################################################################################################
# Purpose: FB Gender Equality At Home economic questions
#############################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(tidyverse)
source('/FILEPATH/econ_functions.R')

#source central fxns 
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
run_date <- gsub('-', '_', Sys.Date())

#age and loc dts
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
ages <- get_age_metadata(19, gbd_round_id=7)

# Read in FB data and plot props ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fb <- as.data.table(read.xlsx('/FILEPATH/DATASET.XLSX', sheet='Data'))
names(fb)

#initialize variables
vars <- c('a4', 'a6_r', 'region', 'sub_region', 'w_subregion', 'd8', 'a3', 'd6_job', 'b7', 'b10', 'd1', 'd3_r', 'c1d', 'c2d')
vars_names <- c('gender', 'urbanicity', 'region', 'location_name', 'weight', 'employment_status', 'emp_last_7_days', 'c19_emp_loss_perm', 
                'fihh_inc', 'food_insec_exp', 'household_size', 'education', 'care', 'chores')

#subset
dt <- fb[, (vars), with=F] %>% as.data.table
names(dt) <- vars_names
dt[, gender:=ifelse(gender==1, 'Female', 'Male')]
dt[, urbanicity:=ifelse(urbanicity==2, 'Rural', 'Urban')]
dt <- dt %>% filter(!is.na(gender)) %>% as.data.table() #Keeping only observations that have sex
dt[location_name=='Bolivia', location_name:='Bolivia (Plurinational State of)']
dt[location_name=='Vietnam', location_name:='Viet Nam']
dt[location_name=='Taiwan', location_name:='Taiwan (Province of China)']
dt[location_name=='Swaziland', location_name:='Eswatini']
dt[location_name=='Czech Republic', location_name:='Czechia']
dt[location_name=='Laos', location_name:="Lao People's Democratic Republic"]
dt[location_name=='Macedonia', location_name:='North Macedonia']
dt[location_name=='Moldova', location_name:='Republic of Moldova']
dt[location_name=='Russia', location_name:='Russian Federation']
# Merge in location data
dt <- left_join(dt, locs[, c("location_name", "region_name", "super_region_name", "ihme_loc_id")], by = "location_name") %>% as.data.table()

#Add NID, line_id
dt[, nid:='472461']
dt[, line_id:=1:.N]

#sex_id
dt[gender %in% c("Male"), sex_id :=1]
dt[gender %in% c("Female"), sex_id :=2]

#education_bin
dt <- dt %>% mutate(education_bin = case_when(
  education == 1 ~ "More than high school", 
  education == 2 ~ "High school graduate or less"
))

#Create economic and time use indicators
dt <- dt %>% mutate(emp_current = case_when(
  employment_status %in% c(2,3,4) ~ 1, 
  employment_status %in% c(1,5,6,7,8,9,10) ~0))

dt <- dt %>% mutate(emp_2 = case_when(
  emp_last_7_days ==1 ~ 1,
  emp_last_7_days ==0 ~ 0))

table(dt$c19_emp_loss_perm)

dt <- dt %>% mutate(c19_emp_loss_perm = case_when(
c19_emp_loss_perm ==1 & emp_2 == 0 ~ 1,
emp_2 == 1 ~ 0))

dt <- dt %>% mutate(emp_2 = case_when(
  emp_last_7_days ==1 ~ 1, 
  emp_last_7_days ==0 ~ 0))

dt <- dt %>% mutate(fihh_inc_ind = case_when(
  fihh_inc %in% c(1,2) ~ 1, 
  fihh_inc == 3 ~ 0))

table(dt$food_insec_exp)

dt <- dt %>% mutate(care_increase = case_when(
  care ==1 ~ 1, 
  care %in% c(2,3) ~ 0))

dt <- dt %>% mutate(chores_increase = case_when(
  chores ==1 ~ 1, 
  chores %in% c(2,3) ~ 0))

#subset microdata
microdata <- dt[, c('line_id', 'location_name', 'urbanicity', 'region', 'nid', 'sex_id', 'gender', 'emp_current', 'emp_2', 
                      'c19_emp_loss_perm', 'fihh_inc_ind', 'food_insec_exp', 'care_increase', 'chores_increase', 'weight', 'super_region_name', 
                    'ihme_loc_id', 'education_bin')]

microdata$date <- "2020-07-01"
microdata$yearmonth <- format(as.Date(microdata$date), "%Y-%m")

write.csv(microdata, '/FILEPATH/DATASET.csv')