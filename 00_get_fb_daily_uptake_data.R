########################################################################################################################################################
# 
# Purpose: Read in CTIS data and subset to uptake data
#
########################################################################################################################################################

#rm(list=ls())

#create directory to save cleaned data
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = T)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

# set-up -----------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(readr)
library(magrittr)
library(stringr)
source("/FILEPATH/get_location_metadata.R")

global.root <- paste0("/FILEPATH/")
us.root <- paste0("/FILEPATH/")


#get facebook data by country and sex ------------------------------------------------------------------------------------------------------------------
#read in weighted data (total_country_sex)
fb_up_cs <- fread(paste0(global.root,'d_/d_cs.csv')) 
ust_up_us <- fread(paste0(us.root,'d_/d_us.csv'))

#temp values - adding prop_nm and original sample back in later
temp <- rbind(fb_up_cs[, c('date', 'location_id', 'location_name', 'sex', 'variable', 'value', 'value_label', 'prop_nm', 'denom_nm', 'obs_nm')],
              ust_up_us[, c('date', 'location_id', 'location_name', 'sex', 'variable', 'value', 'value_label', 'prop_nm', 'denom_nm', 'obs_nm')])
temp <- temp[variable == 'V1' & sex %in% c('Male', 'Female') & value == 1 & obs_nm >= 30]
setnames(temp, 'prop_nm', 'vaccinated')

#keep variables of interest for males and females
fb_up_cs <- fb_up_cs[, c('date', 'location_id', 'ihme_loc_id', 'location_name', 'sex', 'variable', 'value', 'value_label', 'num', 'obs_nm')]
fb_up_cs <- fb_up_cs[variable == 'V1' & sex %in% c('Male', 'Female') & value == 1 & obs_nm >= 30]
fb_up_cs$obs_nm <- NULL

ust_up_us <- ust_up_us[, c('date', 'location_id', 'ihme_loc_id', 'location_name', 'sex', 'variable', 'value', 'value_label', 'num', 'obs_nm')]
ust_up_us <- ust_up_us[variable == 'V1' & sex %in% c('Male', 'Female') & value == 1 & obs_nm >= 30]
ust_up_us$obs_nm <- NULL

#bind facebook data
ust_up_us$value <- as.integer(ust_up_us$value)
fb_up_cs <- bind_rows(fb_up_cs, ust_up_us)


# calculate uptake -------------------------------------------------------------------------------------------------------------------------------------

#total number who have received vaccine by location
fb_up_c_agg <- as.data.table(fb_up_cs)[, sum(num), by = .(date, location_id, ihme_loc_id, location_name)]
fb_up_cs <- merge(fb_up_cs, fb_up_c_agg, by = c('date', 'location_id', 'ihme_loc_id', 'location_name'))
setnames(fb_up_cs, 'V1', 'all_dose1')

#uptake
fb_up_cs[, uptake1 := num/all_dose1]

#label
fb_up_cs$data_source <- 'facebook'

#rename
fb_daily_up_cs <- copy(fb_up_cs)

#remove data.frames we will not use
rm(fb_up_c_agg)
rm(ust_up_us)
rm(fb_up_cs)

#add vaccine uptake prop back in
temp$value <- as.integer(temp$value)
fb_daily_up_cs <- merge(fb_daily_up_cs, temp, by = c('date', 'location_id', 'location_name', 'sex', 'variable', 'value', 'value_label'))
setnames(fb_daily_up_cs, 'obs_nm', 'sample')

#remove temp file
rm(temp)

#save
write.csv(fb_daily_up_cs, paste0(output_dir, 'fb_daily_uptake_cs.csv'), row.names = FALSE)