######################################################################################################################################################################################
# 
# Purpose: Pull, format, and save CDC vaccine data for entire 18+ US population
#          Final data table will not be by age or sex; will just be entire 18+ population in the USA
#
######################################################################################################################################################################################

#rm(list=ls())
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

##### 0. SET UP ######################################################################################################################################################################
#load packages
library(data.table)
library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(openxlsx)
library(httr)
library(jsonlite)
source("/FILEPATH/get_location_metadata.R")

#location metadata
hier <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)

#functions
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

##### 1. GET DATA ####################################################################################################################################################################
#get data
cdc_api <- fread('/FILEPATH/')

#rename columns
colnames(cdc_api) <- tolower(colnames(cdc_api))

#select ages categories + select columns we need
cdc_api <- cdc_api[demographic_category %like% 'Age' & demographic_category != 'Age_unknown']
cdc_api <- cdc_api[, c("date", "demographic_category", "administered_dose1", "series_complete_yes")]


##### 2. FORMAT AGE ##################################################################################################################################################################
#drop overlapping age bins + ages <= 18
cdc_vax <- cdc_api[demographic_category %not like% 'known' & 
                   demographic_category %not like% '<12' & 
                   demographic_category %not like% '<18' & 
                   demographic_category %not like% '18-29' & 
                   demographic_category %not like% '30-39' & 
                   demographic_category %not like% '12-15' &
                   demographic_category %not like% '16-17']

#add age bins
cdc_vax[demographic_category %like% '18-24', age_range := '18-24']
cdc_vax[demographic_category %like% '25-39', age_range := '25-39']
cdc_vax[demographic_category %like% '40-49', age_range := '40-49']
cdc_vax[demographic_category %like% '50-64', age_range := '50-64']
cdc_vax[demographic_category %like% '65-74', age_range := '65-74']
cdc_vax[demographic_category %like% '75+', age_range := '75+']


##### 2. CALCULATE VACCINES PER DAY ##################################################################################################################################################
#rename
setnames(cdc_vax, 'administered_dose1', 'dose1')
setnames(cdc_vax, 'series_complete_yes', 'dose2')

#calculate vaccines administered to the entire 18+ population
#sum administered first dose by date
vac1 <- as.data.table(cdc_vax)[, sum(dose1), by = .(date)] %>%
  setnames('V1', 'dose1') %>%
  as.data.table()

#sum administered second dose (full vax) by date
vac2 <- as.data.table(cdc_vax)[, sum(dose2), by = .(date)] %>%
  setnames('V1', 'dose2') %>%
  as.data.table()

#merge vaccines by date
cdc_vax_age <- merge(vac1, vac2, by = c('date'))

#make sure numbers are numbers
cdc_vax_age[, c('dose1', 'dose2')] <- lapply(cdc_vax_age[, c('dose1', 'dose2')], as.numeric)

#delete data we don't need
rm(cdc_api)
rm(vac1)
rm(vac2)
rm(cdc_vax)

##### 3. POPULATION DATA + CALCULATIONS ##############################################################################################################################################
#labels
cdc_vax_age[, date := substr(date, 1, 10)]
cdc_vax_age[, data_source := 'cdc']
cdc_vax_age$location_id <- 102

#add US population data
#get covid population data
pop_full <- fread("/FILEPATH/")

#subset population to 18+
adult_age_groups <- c(66, 67, 9:19,20,30:32,235) 
adult_pop <- pop_full[sex_id == 3 & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by="location_id", .SDcols = "population"]
adult_pop <- merge(adult_pop, hier[,c("location_id","location_name")], by = "location_id")

#merge pop data
cdc_vax_age <- merge(cdc_vax_age, adult_pop[location_id == 102], by = c('location_id'))

#calculate proportion vaccinated and fully vaccinated
cdc_vax_age[, vaccinated := dose1/population]
cdc_vax_age[, full_vax := dose2/population]


##### 3. FORMAT #####################################################################################################################################################################
#rename
setnames(cdc_vax_age, 'population', 'sample')

#rename dt
cdc_vax_all <- copy(cdc_vax_age)

#format data
cdc_vax_all[, date := as.Date(date, "%m/%d/%Y")]

#delete data we don't need
rm(hier)
rm(pop_full)
rm(adult_pop)
rm(cdc_vax_age)
rm(adult_age_groups)

#save
write.csv(cdc_vax_all, paste0(output_dir, 'cdc_data.csv'), row.names = FALSE)