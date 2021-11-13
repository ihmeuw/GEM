rm(list=ls())

run_date <- Sys.Date()

dir.create(paste0('/FILEPATH/', run_date, '/'), recursive = T)
merged_output_dir <- paste0('/FILEPATH/', run_date, '/')

##### 0. Set Up #############################################################################################################################################################

#libraries
pacman::p_load(data.table, readstata13, readr, dplyr, stringr, magrittr, openxlsx, jsonlite, httr, ggplot2, gridExtra)

#central function
source(file.path("/ihme/cc_resources/libraries/current/r/get_location_metadata.R"))


##### 1. SOURCE DATA #############################################################################################################################################################

##### YOUGOV
source('/FILEPATH/00_get_yougov_data.R')
yougov <- fread(paste0('/FILEPATH/', Sys.Date(), '/input_data/cleaned_yougov.csv'))


##### CTIS
#get ctis data
source('/FILEPATH/00_get_fullvax_fb.R')


##### GLOBAL HEALTH 50/50
#get global health data
source('/FILEPATH/00_get_gh5050_api_data.R')
rm(gh5050_csa)


##### CDC
#get CDC data
source('/FILEPATH/00_get_cdc_api_data.R')


##### COVERAGE
#get COVerAGE-DB data
source('/FILEPATH/00_get_coverageDB_data.R')
rm(coverageDB_csa) #only going to look at country-sex


##### 2. GET LOCATION + POPULATION DATA #############################################################################################################################################################

#location data
hierarchy <- get_location_metadata(111, 771, release_id = 9)

#get covid population data
pop_full <- fread("/FILEPATH/")
adult_age_groups <- c(66, 67, 9:19,20,30:32,235)
adult_pop <- pop_full[sex_id == 3 & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by="location_id", .SDcols = "population"]
adult_pop <- merge(adult_pop, hierarchy[,c("location_id","location_name")], by = "location_id")

adult_sex_pop <- pop_full[sex_id %in% c(1,2) & age_group_id %in% adult_age_groups, lapply(.SD, function(x) sum(x)), by=c("location_id",'sex_id'), .SDcols = "population"]
adult_sex_pop <- merge(adult_sex_pop, hierarchy[,c("location_id","location_name")], by = "location_id")


##### 3. GET YOUGOV DATA AND CALCULATE FULLY VACCINATED #############################################################################################################################################################

#rename locations to match gbd ones
yougov[country %like% 'Korea', country := 'Republic of Korea']
yougov[country %like% 'United States', country := 'United States of America']

#make dates
yougov[, date := as.Date(substr(endtime, 1, 10), "%d/%m/%Y")]

#subset
yougov_up <- yougov[, c('qweek', 'date', 'country', 'gender', 'weight', 'vac')]

#change names
setnames(yougov_up, 'country', 'location_name')
setnames(yougov_up, 'gender', 'sex')

#remove NAs
yougov_up <- yougov_up[!is.na(vac) & vac != '' & vac != '-1']

#assign vaccine uptake
yougov_up[vac %like% 'No', unweighted_vac2 := 0]
yougov_up[vac %like% 'one', unweighted_vac2 := 0]
yougov_up[vac %like% 'two', unweighted_vac2 := 1]
yougov_up[, unweighted_N := 1]

#assign weights
yougov_up[, vac2 := unweighted_vac2*weight]
yougov_up[, N := unweighted_N*weight]

#sex-specific uptake
vac2_sex <- as.data.table(yougov_up)[, sum(vac2), by = .(location_name, date, sex)] %>%
  setnames('V1', 'vac2') %>%
  as.data.table()

#total uptake
vac2_total <- as.data.table(yougov_up)[, sum(vac2), by = .(location_name, date)] %>%
  setnames('V1', 'total2') %>%
  as.data.table()

#weighted sample size
responses <- as.data.table(yougov_up)[, sum(N), by = .(location_name, date, sex)] %>%
  setnames('V1', 'N') %>%
  as.data.table()

#unweighted sample size
sample_size <- as.data.table(yougov_up)[, sum(unweighted_N), by = .(location_name, date, sex)] %>%
  setnames('V1', 'raw_sample') %>%
  as.data.table()

#merge all summed values
yg_up_cs <- merge(vac2_sex, vac2_total, by = c('location_name', 'date')) %>%
  merge(responses, by = c('location_name', 'date', 'sex')) %>%
  merge(sample_size, by = c('location_name', 'date', 'sex')) 

#calculate gender breakdown and % fully vaccinated
yg_up_cs[, uptake2 := vac2/total2]
yg_up_cs[, full_vax := vac2/N]

#label
yg_up_cs[, data_source := 'yougov']

#set sample limit
yg_up_cs <- yg_up_cs[raw_sample >= 30] #sample needs to be >= 30

#subset and rename
yougov_final <- yg_up_cs[, c('date', 'location_name', 'sex', 'full_vax', 'N', 'raw_sample', 'data_source')]
setnames(yougov_final, 'raw_sample', 'sample')
setnames(yougov_final, 'N', 'denom_nm')

#remove what we don't need
rm(vac2_sex)
rm(vac2_total)
rm(responses)


##### 3. SUBSET AND MERGE OTHER DATA #############################################################################################################################################################

#subset ctis ----------------------------------------------------------------------------------------------------------------------------
hierarchy <- get_location_metadata(111, 771, release_id = 9)
setnames(fb_fullvax_cs, 'ISO_3', 'ihme_loc_id')
fb_fullvax_cs <- merge(fb_fullvax_cs, hierarchy[,.(location_name, ihme_loc_id)], by = 'ihme_loc_id')
fb_temp <- copy(fb_fullvax_cs)
fb_temp <- fb_temp[, c('date', 'location_name', 'sex', 'full_vax', 'sample', 'denom_nm', 'data_source')]


#subset CDC ----------------------------------------------------------------------------------------------------------------------------
cdc_vax$location_name <- 'United States of America'
cdc_vax[sex == 'Male', sex_id := 1]
cdc_vax[sex == 'Female', sex_id := 2]
cdc_vax <- merge(cdc_vax, adult_sex_pop, by = c('location_name', 'sex_id'))
cdc_vax[, full_vax := dose2/population]
setnames(cdc_vax, 'population', 'sample')
cdc_vax[, denom_nm := sample]
cdc_temp <- cdc_vax[, c('date', 'location_name', 'sex', 'full_vax', 'denom_nm', 'sample', 'data_source')]


#subset coverage ----------------------------------------------------------------------------------------------------------------------------
setnames(coverageDB_cs, 'population', 'sample')
coverageDB_cs[, denom_nm := sample]
coverage_temp <- coverageDB_cs[, c('date', 'location_name', 'sex', 'full_vax', 'denom_nm', 'sample', 'data_source')]


#subset GH 5050
gh5050_temp <- copy(gh5050_cs)
setnames(gh5050_temp, 'population', 'sample')
gh5050_temp[, denom_nm := sample]
gh5050_temp <- gh5050_temp[, c('date', 'location_name', 'sex', 'full_vax', 'denom_nm', 'sample', 'data_source')]


#edit date
cdc_temp[, date := as.Date(date, "%m/%d/%Y")]
cdc_temp[, date := as.IDate(date)]
yougov_final[, date := as.IDate(date)]
gh5050_temp[, date := as.Date(date, "%m/%d/%Y")]
gh5050_temp[, date := as.IDate(date)]
gh5050_temp <- gh5050_temp[!is.na(date)]


##### MERGE #############################################################################################################################################################

final_full_vax <- rbind(yougov_final, coverage_temp) %>%
  rbind(gh5050_temp) %>%
  rbind(cdc_temp) %>%
  rbind(fb_temp)

final_full_vax <- merge(final_full_vax, hierarchy[, .(location_name, location_id, ihme_loc_id)], by = 'location_name')


#add sex_id
final_full_vax[sex == 'Male', sex_id := 1]
final_full_vax[sex == 'Female', sex_id := 2]


#age limits
final_full_vax[, age := '18+']


#format
setnames(final_full_vax, 'full_vax', 'proportion')
final_full_vax[, indicator := 'fully_vaccinated']

#save
save_uptake <- copy(final_full_vax)

write.csv(save_uptake, paste0(merged_output_dir, 'fully_vaccinated_by_sex.csv'), row.names = FALSE)