####################################################################################################################################################################
# 
# Purpose: Pull, format, and save data from Our World in Data
#          Calculates the % of the population that has been vaccinated
#
####################################################################################################################################################################

#rm(list=ls())

dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

##### 0. SET UP ####################################################################################################################################################

#libraries
pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,readstata13,magrittr,readr,openxlsx,httr,jsonlite)

#location data
source(file.path("/FILEPATH/get_location_metadata.R"))
hier <- get_location_metadata(111, 771, release_id = 9)

#get population data
pop_full <- fread("/FILEPATH/")
adult_age_groups <- c(66, 67, 9:19,20,30:32,235) #18 and older
adult_pop <- pop_full[sex_id == 3 & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by="location_id", .SDcols = "population"]
adult_pop <- merge(adult_pop, hier[,c("location_id","location_name")], by = "location_id")


##### 1. LOAD + FORMAT OUR WORLD IN DATA ###########################################################################################################################

#get owid data
owid <- fread('/FILEPATH/')

#subset owid
owid <- owid[, c('location_id', 'location_name', 'date', 'people_vaccinated', 'people_fully_vaccinated')]

#add location data
owid <- merge(owid, adult_pop, by = c('location_name', 'location_id'))

#calculated percent vaccinated and fully vaccinated
owid[, vaccinated := people_vaccinated/population]
owid[, full_vax := people_fully_vaccinated/population]

#label and rename
owid[, data_source := 'owid']
setnames(owid, 'population', 'sample')

#subset
owid <- owid[, c('date', 'location_id', 'location_name', 'vaccinated', 'full_vax', 'sample', 'data_source')]

#add age tag
owid[, age := '18+']

#exclude USA
owid <- owid[location_id != 102]

#save
write.csv(owid, paste0(output_dir, 'owid_vaccine_data.csv'), row.names = F)