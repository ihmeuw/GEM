####################################################################################################################################################################
# 
# Purpose: Pull, format, and save data from COVerAGE-DB
#
####################################################################################################################################################################

#rm(list=ls())

#create directory to save cleaned data
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

##### 0. Set up ####################################################################################################################################################

pacman::p_load(data.table, readstata13, readr, dplyr, magrittr, stringr, openxlsx, httr, jsonlite, ggplot2)
source("/FILEPATH/get_location_metadata.R")

#location_metadata
hierarchy <- get_location_metadata(111, 771, release_id = 9)

#functions
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

##### 1. Ppopulation data ##########################################################################################################################################
pop_full <- fread("/FILEPATH/")
plus_18 <- c(66, 67, 9:19,20,30:32,235)
plus_20 <- c(9:19,20,30:32,235)

#for peru, which is 18+
adult18_pop <- pop_full[sex_id %in% c(1,2) & age_group_id %in% plus_18, lapply(.SD, function(x) sum(x)), by=c("location_id","sex_id"), .SDcols = "population"]
adult18_pop <- merge(adult18_pop, hierarchy[,c("location_id","location_name")], by = "location_id")
adult18_pop <- adult18_pop[location_id == 123]

#for other locations, which are 20+
adult20_pop <- pop_full[sex_id %in% c(1,2) & age_group_id %in% plus_20, lapply(.SD, function(x) sum(x)), by=c("location_id","sex_id"), .SDcols = "population"]
adult20_pop <- merge(adult20_pop, hierarchy[,c("location_id","location_name")], by = "location_id")
adult20_pop <- adult20_pop[location_id != 123]

#merge
adult_pop <- rbind(adult18_pop, adult20_pop)

#remove
rm(adult18_pop)
rm(adult20_pop)


##### 2. Get COVerAGE-DB data ######################################################################################################################################
coverageDB <- fread('/FILEPATH/')
coverageDB <- coverageDB[location != 'Michigan']
coverageDB <- coverageDB[location != 'Belgium']

#subset to sex-specific info + rename
coverageDB <- coverageDB[sex %in% c('male', 'female')]
coverageDB[sex == 'male', sex := 'Male']
coverageDB[sex == 'female', sex := 'Female']
coverageDB$Vaccinations <- NULL

##### 3. Fix ages #################################################################################################################################################
#remove rows with no vaccine info
coverageDB <- coverageDB[!is.na(Vaccination1) & !is.na(Vaccination2)]

#peru reported in single age bins; separate and adjust
peru <- coverageDB[location_id == 123 & age_start >= 18]
coverageDB <- coverageDB[location_id != 123 & age_start >= 18]

#create Peru age_range manually
peru[between(age_start, 18, 24), age_range := '18-24']
peru[between(age_start, 25, 34), age_range := '25-34']
peru[between(age_start, 35, 44), age_range := '35-44']
peru[between(age_start, 45, 54), age_range := '45-54']
peru[between(age_start, 55, 64), age_range := '55-64']
peru[between(age_start, 65, 74), age_range := '65-74']
peru[between(age_start, 75, 150), age_range := '75+']

#aggregate vaccines by age_range
peru_agg1 <- as.data.table(peru)[, sum(Vaccination1), by = .(location, location_id, date, sex, data_filename, age_range)] %>%
  setnames('V1', 'Vaccination1')
peru_agg2 <- as.data.table(peru)[, sum(Vaccination2), by = .(location, location_id, date, sex, data_filename, age_range)] %>%
  setnames('V1', 'Vaccination2')

#merge peru data
peru <- merge(peru_agg1, peru_agg2, by = c('location', 'location_id', 'date', 'sex', 'data_filename', 'age_range'))

#add peru age_start and age_end manually
peru[, age_start := as.numeric(substr(age_range, 1, 2))]
peru[, age_end := as.numeric(substr(age_range, 4, 5))]

#create age ranges for non-Peru locations
coverageDB[, age_range := paste0(age_start, '-', age_end)]

#merge data back
coverageDB <- rbind(coverageDB, peru)

#remove column we don't need
coverageDB$data_filename <- NULL


##### 3. Calculate breakdown of vaccinated people ##################################################################################################################

# calculate by age and sex -----------------------------------------------------------------------------------------------------------------------------------------

#total vac1 by location-age
total1 <- as.data.table(coverageDB)[, sum(Vaccination1), by = .(location_id, location, date, age_range)] %>%
  setnames('V1', 'total1') %>%
  as.data.table()

#total vac2 by location-age
total2 <- as.data.table(coverageDB)[, sum(Vaccination2), by = .(location_id, location, date, age_range)] %>%
  setnames('V1', 'total2') %>%
  as.data.table()

#merge
coverageDB <- merge(coverageDB, total1, by = c('location_id', 'location', 'date', 'age_range')) %>%
  merge(total2, by = c('location_id', 'location', 'date', 'age_range')) %>%
  as.data.table()

#calculate breakdown (of vaccinated people aged Y, X% are female/male)
coverageDB[, uptake1 := Vaccination1/total1]
coverageDB[, uptake2 := Vaccination2/total2]

#format
setnames(coverageDB, 'date', 'original_time')
coverageDB[, date := as.IDate(as.Date(original_time, "%d.%m.%Y"))]
setnames(coverageDB, 'location', 'location_name')
coverageDB[, data_source := 'coverage']
coverageDB_csa <- copy(coverageDB)

#remove data.frames we don't need
rm(peru)
rm(peru_agg1)
rm(peru_agg2)
rm(hierarchy)
rm(total1)
rm(total2)

# calculate by sex -------------------------------------------------------------------------------------------------------------------------------------------------

coverageDB_cs <- copy(coverageDB[, c('location_id', 'location_name', 'date', 'age_range', 'sex', 'Vaccination1', 'data_source')])

#total vac1 by location-sex
vac1_agg <- as.data.table(coverageDB)[, sum(Vaccination1), by = .(location_id, location_name, date, sex)] %>%
  setnames('V1', 'Vaccination1') %>%
  as.data.table()

#total vac1 by location
vac1_total <- as.data.table(coverageDB)[, sum(Vaccination1), by = .(location_id, location_name, date)] %>%
  setnames('V1', 'total1') %>%
  as.data.table()

#total vac2 by location-sex
vac2_agg <- as.data.table(coverageDB)[, sum(Vaccination2), by = .(location_id, location_name, date, sex)] %>%
  setnames('V1', 'Vaccination2') %>%
  as.data.table()

#total vac2 by location
vac2_total <- as.data.table(coverageDB)[, sum(Vaccination2), by = .(location_id, location_name, date)] %>%
  setnames('V1', 'total2') %>%
  as.data.table()

#merge
coverageDB_cs <- merge(vac1_agg, vac1_total, by = c('location_id', 'location_name', 'date')) %>%
  merge(vac2_agg, by = c('location_id', 'location_name', 'date', 'sex')) %>%
  merge(vac2_total, by = c('location_id', 'location_name', 'date'))

#add population
coverageDB_cs[sex == 'Male', sex_id := 1]
coverageDB_cs[sex == 'Female', sex_id := 2]
coverageDB_cs <- merge(coverageDB_cs, adult_pop, by = c('location_id', 'location_name', 'sex_id'))

#gender breakdown (of those vaccinated, x% are female/male)
coverageDB_cs[, uptake1 := Vaccination1/total1]
coverageDB_cs[, uptake2 := Vaccination2/total2]

#vaccinations
coverageDB_cs[, vaccinated := Vaccination1/population]
coverageDB_cs[, full_vax := Vaccination2/population]

#label
coverageDB_cs[, data_source := 'coverage']

#remove everything we won't need (helps when sourcing code)
rm(vac1_agg)
rm(vac1_total)
rm(vac2_agg)
rm(vac2_total)
rm(coverageDB)
rm(pop_full)
rm(adult_pop)
rm(plus_18)
rm(plus_20)

#save
write.csv(coverageDB_csa, paste0(output_dir, 'coverage_csa.csv'), row.names = FALSE)
write.csv(coverageDB_cs, paste0(output_dir, 'coverage_cs.csv'), row.names = FALSE)
