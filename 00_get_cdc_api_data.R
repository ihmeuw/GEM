#################################################################################################################################################################################
# 
# Purpose: Pull, format, and save sex-specific vaccine data from CDC
#
#################################################################################################################################################################################

#rm(list=ls())

#load packages ------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(readstata13)
library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(openxlsx)
library(httr)
library(jsonlite)
library(ggplot2)
source("/FILEPATH/get_location_metadata.R")

hier <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9) %>% 
  select(location_id, location_name, region_name, super_region_name, path_to_top_parent, parent_id, level) %>%
  as.data.table()

"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")


#read and format data -----------------------------------------------------------------------------------------------------------------------------------------------------------

#get saved cdc data
cdc_api <- fread('/FILEPATH/')
colnames(cdc_api) <- tolower(colnames(cdc_api))

#subset to sex information
cdc_api <- cdc_api[demographic_category %like% 'Sex' & demographic_category != 'Sex_unknown']
cdc_api <- cdc_api[, c("date", "demographic_category", "administered_dose1", "series_complete_yes")]

#subset to sex-specific and total vaccine numbers + merge back ------------------------------------------------------------------------------------------------------------------
cdc_vax <- cdc_api[demographic_category %not like% 'known']
setnames(cdc_vax, 'administered_dose1', 'dose1')
setnames(cdc_vax, 'series_complete_yes', 'dose2')

cdc_total <- cdc_api[demographic_category %like% 'known']
cdc_total$demographic_category <- NULL
setnames(cdc_total, 'administered_dose1', 'total_dose1')
setnames(cdc_total, 'series_complete_yes', 'total_dose2')

cdc_vax <- merge(cdc_vax, cdc_total, by = 'date')


#calculate gender breakdown of uptake ------------------------------------------------------------------------------------------------------------------------------------------
cdc_vax[, c('dose1', 'dose2', 'total_dose1', 'total_dose2')] <- lapply(cdc_vax[, c('dose1', 'dose2', 'total_dose1', 'total_dose2')], as.numeric)

cdc_vax[, uptake1 := dose1/total_dose1]
cdc_vax[, uptake2 := dose2/total_dose2]


#rename sex --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cdc_vax[demographic_category %like% 'Male', sex := 'Male']
cdc_vax[demographic_category %like% 'Female', sex := 'Female']


#edit date --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cdc_vax[, date := substr(date, 1, 10)]
cdc_vax[, data_source := 'cdc']


#delete data we don't need ----------------------------------------------------------------------------------------------------------------------------------------------------
rm(cdc_api)
rm(cdc_total)