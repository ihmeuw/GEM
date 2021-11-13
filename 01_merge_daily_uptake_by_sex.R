####################################################################################################################################################################
# 
# Purpose: Pull, format, and save all daily vaccinated data (any vaccine, >= 1 dose) 
#
####################################################################################################################################################################

rm(list=ls())

run_date <- Sys.Date()

dir.create(paste0('/FILEPATH/', run_date, '/'), recursive = T)
merged_output_dir <- paste0('/FILEPATH/', run_date, '/')

##### 0. SET UP #############################################################################################################################################################
library(data.table)
library(readstata13)
library(readr)
library(lme4)
library(dplyr)
library(magrittr)
library(stringr)
library(openxlsx)
library(httr)
library(jsonlite)
library(ggplot2)
library(gridExtra)
source(file.path("/FILEPATH/get_location_metadata.R"))

##### 1. SOURCE DATA FROM OTHER SCRIPTS #############################################################################################################################################################

### CTIS
source('/FILEPATH/00_get_fb_daily_uptake_data.R')


### GH 50/50
source('/FILEPATH/00_get_gh5050_api_data.R')
rm(gh5050_csa)


### CDC
source('/FILEPATH/00_get_cdc_api_data.R')


### COVerAGE-DB
source('/FILEPATH/00_get_coverageDB_data.R')
rm(coverageDB_csa) 

### YOUGOV
source('/FILEPATH/00_get_yougov_data.R')
yougov <- fread(paste0('/FILEPATH/', Sys.Date(), '/input_data/cleaned_yougov.csv'))


##### 2. LOCATION AND POPULATION DATA #############################################################################################################################################################

#standardize this data so that it is not dependent on last sourced script

#location meta_data
hier <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)

#get covid population data
pop_full <- fread("/FILEPATH/")
adult_age_groups <- c(66, 67, 9:19,20,30:32,235)
adult_pop <- pop_full[sex_id == 3 & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by="location_id", .SDcols = "population"]
adult_pop <- merge(adult_pop, hier[,c("location_id","location_name")], by = "location_id")

adult_sex_pop <- pop_full[sex_id %in% c(1,2) & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by=c("location_id",'sex_id'), .SDcols = "population"]
adult_sex_pop <- merge(adult_sex_pop, hier[,c("location_id","location_name")], by = "location_id")


##### 3. CALCULATE YOUGOV UPTAKE #############################################################################################################################################################

#fix location names
yougov[country %like% 'Korea', country := 'Republic of Korea']
yougov[country %like% 'United States', country := 'United States of America']

#make dates
yougov[, date := as.Date(substr(endtime, 1, 10), "%d/%m/%Y")]

#subset
yougov_up <- yougov[, c('qweek', 'date', 'country', 'gender', 'vac', 'weight')]

#change names
setnames(yougov_up, 'country', 'location_name')
setnames(yougov_up, 'gender', 'sex')

#remove NAs
yougov_up <- yougov_up[!is.na(vac) & vac != '' & vac != '-1']

#assign vaccine uptake
yougov_up[vac %like% 'No', unweighted_vac1 := 0]
yougov_up[vac %like% 'Yes', unweighted_vac1 := 1]
yougov_up[, unweighted_N := 1]

#calculate weights
yougov_up[, vac1 := unweighted_vac1*weight]
yougov_up[, vac1 := unweighted_vac1*weight]
yougov_up[, N := unweighted_N*weight]

#sum weighted vacs by location-sex
vac1_sex <- as.data.table(yougov_up)[, sum(vac1), by = .(location_name, date, sex)] %>%
  setnames('V1', 'vac1') %>%
  as.data.table()

#sum weighted vacs by location
vac1_total <- as.data.table(yougov_up)[, sum(vac1), by = .(location_name, date)] %>%
  setnames('V1', 'total1') %>%
  as.data.table()

#sum weighted sample by location-sex
responses <- as.data.table(yougov_up)[, sum(N), by = .(location_name, date, sex)] %>%
  setnames('V1', 'N') %>%
  as.data.table()

#sum unweighted sample by location-sex (use for exclusions)
sample_size <- as.data.table(yougov_up)[, sum(unweighted_N), by = .(location_name, date, sex)] %>%
  setnames('V1', 'raw_sample') %>%
  as.data.table()

#merge
yg_up_cs <- merge(vac1_sex, vac1_total, by = c('location_name', 'date')) %>%
  merge(responses, by = c('location_name', 'date', 'sex')) %>%
  merge(sample_size, by = c('location_name', 'date', 'sex'))

#calculate vaccine uptake
yg_up_cs[, uptake1 := vac1/total1]
yg_up_cs[, vaccinated := vac1/N]

#label
yg_up_cs[, data_source := 'yougov']

#restrict to days where >= 30 people responded
yg_up_cs <- yg_up_cs[raw_sample >= 30]

#subset
yougov_final <- yg_up_cs[, c('date', 'location_name', 'sex', 'uptake1', 'vaccinated', 'N', 'raw_sample', 'data_source')]
setnames(yougov_final, 'raw_sample', 'sample')
setnames(yougov_final, 'N', 'denom_nm')

#remove what we don't need
rm(vac1_sex)
rm(vac1_total)
rm(responses)
rm(sample_size)


##### 4. SUBSET AND MERGE DATA #############################################################################################################################################################

#subset ctis ---------------------------------------------------------------------------------------------
fb_temp <- copy(fb_daily_up_cs)
fb_temp <- fb_temp[, c('date', 'location_name', 'sex', 'uptake1', 'vaccinated', 'denom_nm', 'sample', 'data_source')]


#subset CDC ----------------------------------------------------------------------------------------------
#add location
cdc_vax$location_name <- 'United States of America'

#add sex
cdc_vax[sex == 'Male', sex_id := 1]
cdc_vax[sex == 'Female', sex_id := 2]

#add population
cdc_vax <- merge(cdc_vax, adult_sex_pop, by = c('location_name', 'sex_id'))

#calc, rename, subset
cdc_vax[, vaccinated := dose1/population]
setnames(cdc_vax, 'population', 'sample')
cdc_vax[, denom_nm := sample]
cdc_temp <- cdc_vax[, c('date', 'location_name', 'sex', 'uptake1', 'vaccinated', 'denom_nm', 'sample', 'data_source')]


#subset coverage ----------------------------------------------------------------------------------------------
#coverageDb_cs
setnames(coverageDB_cs, 'population', 'sample')
coverageDB_cs[, denom_nm := sample]
coverage_temp <- coverageDB_cs[, c('date', 'location_name', 'sex', 'uptake1', 'vaccinated', 'denom_nm', 'sample', 'data_source')]


#subset GH 5050  ----------------------------------------------------------------------------------------------
gh5050_temp <- copy(gh5050_cs)
setnames(gh5050_temp, 'population', 'sample')
gh5050_temp[, denom_nm := sample]
gh5050_temp <- gh5050_temp[, c('date', 'location_name', 'sex', 'uptake1', 'vaccinated', 'denom_nm', 'sample', 'data_source')]

#edit date
cdc_temp[, date := as.Date(date, "%m/%d/%Y")]
cdc_temp[, date := as.IDate(date)]
yougov_final[, date := as.IDate(date)]
gh5050_temp[, date := as.Date(date, "%m/%d/%Y")]
gh5050_temp[, date := as.IDate(date)]
gh5050_temp <- gh5050_temp[!is.na(date)]


##### MERGE VAX DATA + LOCATION DATA #############################################################################################################################################################

#merge
all_uptake <- rbind(yougov_final, fb_temp) %>%
  rbind(gh5050_temp) %>%
  rbind(cdc_temp) %>%
  rbind(coverage_temp)


#add location data
all_uptake <- merge(all_uptake, hier[, .(location_name, location_id, ihme_loc_id)], by = 'location_name')


#add sex_id
all_uptake[sex == 'Male', sex_id := 1]
all_uptake[sex == 'Female', sex_id := 2]


#add age limits
all_uptake[, age := '18+']


#format
all_uptake$uptake1 <- NULL
setnames(all_uptake, 'vaccinated', 'proportion')
all_uptake[, indicator := 'vaccinated']


#re-order
save_uptake <- copy(all_uptake)


#save
write.csv(save_uptake, paste0(merged_output_dir, 'one_dose_uptake_by_sex.csv'), row.names = FALSE)