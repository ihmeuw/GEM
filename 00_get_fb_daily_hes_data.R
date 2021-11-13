####################################################################################################################################################################
# 
# Purpose: Read in CTIS data and subset to hesitancy data
#
####################################################################################################################################################################

#rm(list=ls())

#create directory to save cleaned data
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir_clean <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

##### 0. Set up ####################################################################################################################################################
#library
library(data.table)
library(readr)
library(magrittr)
library(stringr)
source("/FILEPATH/get_location_metadata.R")

#filepaths
global.root <- paste0("/FILEPATH/")
us.root <- paste0("/FILEPATH/")

##### 1. Daily hesitancy by country-sex ############################################################################################################################

#get data by country-sex
fb_daily_hes_cs <- fread(paste0(global.root,'d_/d_cs.csv')) 
ust_daily_hes_us <- fread(paste0(us.root,'d_/d_us.csv'))

#keep variables of interest for males and females
fb_daily_hes_cs <- fb_daily_hes_cs[variable == 'getvax' & sex %in% c('Male', 'Female') & value == 0 & obs_nm >= 30]
setnames(fb_daily_hes_cs, 'prop_nm', 'hesitancy')

ust_daily_hes_us <- ust_daily_hes_us[variable == 'getvax' & sex %in% c('Male', 'Female') & value == 0 & obs_nm >= 30]
setnames(ust_daily_hes_us, 'prop_nm', 'hesitancy')

#drop columns we don't need
fb_daily_hes_cs <- fb_daily_hes_cs[, c('date', 'location_name', 'location_id', 'sex', 'num', 'denom_nm', 'obs_nm', 'hesitancy')]
ust_daily_hes_us <- ust_daily_hes_us[, c('date', 'location_name', 'location_id', 'sex', 'num', 'denom_nm', 'obs_nm', 'hesitancy')]

#bind facebook data
fb_daily_hes_cs <- bind_rows(fb_daily_hes_cs, ust_daily_hes_us)
fb_daily_hes_cs$data_source <- 'facebook'
setnames(fb_daily_hes_cs, 'obs_nm', 'sample')

rm(ust_daily_hes_us)


##### 2. Daily hesitancy by country-sex-age ########################################################################################################################

# #get facebook data by country, sex, and age ----------------------------------------------------------------------------------------------------------------------
#read in weighted data (total_country_sex_age)
fb_hes_csa <- fread(paste0(global.root,'d_/d_csa.csv'))
ust_hes_usa <- fread(paste0(us.root,'d_/d_usa.csv'))

#keep variables of interest for males and females
fb_hes_csa <- fb_hes_csa[variable == 'getvax' & sex %in% c('Male', 'Female') & value == 0 & obs_nm >= 30 & age != '']
setnames(fb_hes_csa, 'prop_nm', 'hesitancy')
setnames(fb_hes_csa, 'age', 'age_group')

ust_hes_usa <- ust_hes_usa[variable == 'getvax' & sex %in% c('Male', 'Female') & value == 0 & obs_nm >= 30 & age != '']
setnames(ust_hes_usa, 'prop_nm', 'hesitancy')
setnames(ust_hes_usa, 'age', 'age_group')

#keep columns we need
fb_hes_csa <- fb_hes_csa[, c('date', 'location_name', 'location_id', 'age_group', 'num', 'denom_nm', 'obs_nm', 'sex', 'hesitancy')]
ust_hes_usa <- ust_hes_usa[, c('date', 'location_name', 'location_id', 'age_group', 'num', 'denom_nm', 'sex', 'obs_nm', 'hesitancy')]

#rename
fb_daily_hes_csa <- bind_rows(fb_hes_csa, ust_hes_usa)
fb_daily_hes_csa$data_source <- 'facebook'
setnames(fb_daily_hes_csa, 'obs_nm', 'sample')


rm(fb_hes_csa)
rm(ust_hes_usa)


#save
write.csv(fb_daily_hes_csa, paste0(output_dir_clean,  'fb_hes_csa.csv'), row.names = FALSE)
write.csv(fb_daily_hes_cs, paste0(output_dir_clean,  'fb_hes_cs.csv'), row.names = FALSE)