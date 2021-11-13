#############################################################################################################################################################################
# 
# Purpose: Aggregate daily vaccine information to get monthly measures
#          Not that OWID and CDC are calculated separately, as those are not sex-specific
#
#############################################################################################################################################################################

rm(list=ls())

run_date <- Sys.Date()

fp <- paste0('/FILEPATH/', run_date, '/')
   
##### 0. Set Up #############################################################################################################################################################

#libraries
pacman::p_load(data.table, readstata13, readr, dplyr, magrittr, stringr, openxlsx, httr, jsonlite, ggplot2, gridExtra)

#daily owid and cdc both-sex data
source('/FILEPATH/00_get_owid_data.R')
source('/FILEPATH/00_get_cdc_api_age18+_data.R')

#location data
source(file.path("/FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)


##### 1. VACCINE HESITANCY BY SEX AND AGE ###################################################################################################################################

#get data
hes_csa <- fread(paste0(fp, 'hesitancy_by_age_sex.csv'))

#rename sources
hes_csa[data_source %like% 'facebook', data_source := 'Facebook Global Symptoms Survey']
hes_csa[data_source %like% 'yougov', data_source := 'YouGov']

#add month and year
hes_csa[, month := month(date)]
hes_csa[, year := year(date)]

#calculated number of people hesitant
hes_csa[, x := proportion*denom_nm]

#re-assign age bins
hes_csa[age_group == '18-24' | age_group == '25-34', age_bin := '18-34']
hes_csa[age_group == '35-44' | age_group == '45-54', age_bin := '35-54']
hes_csa[age_group == '55-64' | age_group == '65-74', age_bin := '55-74']
hes_csa[age_group == '75+', age_bin := '75+']

#sum
csa_x <- as.data.table(hes_csa)[, sum(x), by = .(location_name, sex, data_source, age_bin, month, year)] %>%
  setnames('V1', 'x')
csa_sample <- as.data.table(hes_csa)[, sum(sample), by = .(location_name, sex, data_source, age_bin, month, year)] %>%
  setnames('V1', 'sample')
csa_denom <- as.data.table(hes_csa)[, sum(denom_nm), by = .(location_name, sex, data_source, age_bin, month, year)] %>%
  setnames('V1', 'denom_nm')

monthly_csa <- merge(csa_x, csa_sample, by =c('location_name', 'sex', 'data_source', 'age_bin', 'month', 'year')) %>%
  merge(csa_denom, by =c('location_name', 'sex', 'data_source', 'age_bin', 'month', 'year'))
monthly_csa[, proportion := x/denom_nm]

#clean + add sex data
monthly_csa$x <- NULL
monthly_csa[, indicator := 'hesitancy']
monthly_csa[sex == 'Male', sex_id := 1]
monthly_csa[sex == 'Female', sex_id := 2]
monthly_csa <- merge(monthly_csa, hierarchy[, .(location_name, location_id, ihme_loc_id)], by = 'location_name')

#save
write.csv(monthly_csa, paste0(fp, 'monthly_hesitancy_by_age_sex.csv'), row.names = F)


##### 2. ANY VACCINATION BY SEX (>=1 DOSE) ##################################################################################################################################

#get data
df_vax <- fread(paste0(fp, 'one_dose_uptake_by_sex.csv'))

#rename sources
df_vax[data_source %like% 'facebook', data_source := 'Facebook Global Symptoms Survey']
df_vax[data_source %like% 'yougov', data_source := 'YouGov']
df_vax[data_source %like% 'gh5050', data_source := 'Global Health 50/50']
df_vax[data_source %like% 'coverage', data_source := 'COVerAGE-DB']
df_vax[data_source %like% 'cdc', data_source := 'CDC']

#add month and year
df_vax[, month := month(date)]
df_vax[, year := year(date)]

#calculate number of people vaccinated
df_vax[, x := proportion*denom_nm]

#sum
vax_x <- as.data.table(df_vax)[, sum(x), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'x')
vax_sample <- as.data.table(df_vax)[, sum(sample), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'sample')
vax_denom <- as.data.table(df_vax)[, sum(denom_nm), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'denom_nm')

#merge
monthly_vax <- merge(vax_x, vax_sample, by =c('location_id', 'ihme_loc_id', 'location_name', 'sex_id', 'sex', 'data_source', 'age', 'month', 'year')) %>%
  merge(vax_denom, by =c('location_id', 'ihme_loc_id', 'location_name', 'sex_id', 'sex', 'data_source', 'age', 'month', 'year'))

#calculate final proportion
monthly_vax[, proportion := x/denom_nm]

#clean + label
monthly_vax$x <- NULL
monthly_vax[, indicator := 'vaccinated']

#save
write.csv(monthly_vax, paste0(fp, 'monthly_any_vaccine_by_sex.csv'), row.names = F)


##### 3. FULLY VACCINATED BY SEX #############################################################################################################################################

#get data
df_full_vax <- fread(paste0(fp, 'fully_vaccinated_by_sex.csv'))

#rename sources
df_full_vax[data_source %like% 'facebook', data_source := 'Facebook Global Symptoms Survey']
df_full_vax[data_source %like% 'yougov', data_source := 'YouGov']
df_full_vax[data_source %like% 'gh5050', data_source := 'Global Health 50/50']
df_full_vax[data_source %like% 'coverage', data_source := 'COVerAGE-DB']
df_full_vax[data_source %like% 'cdc', data_source := 'CDC']

#add month and year
df_full_vax[, month := month(date)]
df_full_vax[, year := year(date)]

#calculate number of fully vaccinated people
df_full_vax[, x := proportion*denom_nm]

#sum
full_x <- as.data.table(df_full_vax)[, sum(x), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'x')
full_sample <- as.data.table(df_full_vax)[, sum(sample), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'sample')
full_denom <- as.data.table(df_full_vax)[, sum(denom_nm), by = .(location_id, ihme_loc_id, location_name, sex_id, sex, data_source, age, month, year)] %>%
  setnames('V1', 'denom_nm')

#merge
monthly_full <- merge(full_x, full_sample, by =c('location_id', 'ihme_loc_id', 'location_name', 'sex_id', 'sex', 'data_source', 'age', 'month', 'year')) %>%
  merge(full_denom, by =c('location_id', 'ihme_loc_id', 'location_name', 'sex_id', 'sex', 'data_source', 'age', 'month', 'year'))
monthly_full[, proportion := x/denom_nm]

#clean + label
monthly_full$x <- NULL
monthly_full[, indicator := 'fully_vaccinated']

#save
write.csv(monthly_full, paste0(fp, 'monthly_fully_vaccinated_by_sex.csv'), row.names = F)


##### 4. OWID (NOT BY SEX) ###################################################################################################################################################

#get data
owid <- fread(paste0(fp, 'FILEPATH/owid_vaccine_data.csv'))
owid[, data_source := 'Our World in Data']

#get month
owid[, date := as.Date(date, "%d.%m.%Y")]
owid[, month := month(date)]
owid[, year := year(date)]

#get exposed
owid[, any_vax_sample := vaccinated*sample]
owid[, full_vax_sample := full_vax*sample]

#sum any vaccines by location-sex-month
owid1_x <- as.data.table(owid)[, mean(any_vax_sample), by = .(location_id, location_name, data_source, month, age, year)] %>%
  setnames('V1', 'dose1')

owid_sample <- as.data.table(unique(owid[, c('location_id', 'location_name', 'data_source', 'sample')]))

owid_any <- merge(owid1_x, owid_sample, by =c('location_id', 'location_name', 'data_source'))
owid_any[, vaccinated := dose1/sample]

#sum full vaccines by location-sex-month
owid2_x <- as.data.table(owid)[, mean(full_vax_sample), by = .(location_id, location_name, data_source, month, age, year)] %>%
  setnames('V1', 'dose2')

owid_full <- merge(owid2_x, owid_sample, by =c('location_id', 'location_name', 'data_source'))
owid_full[, full_vax := dose2/sample]

#merge all
owid_final <- merge(owid_any, owid_full, by =c('location_id', 'location_name', 'data_source', 'month', 'age', 'year', 'sample'))

#edit
owid_final$dose1 <- NULL
owid_final$dose2 <- NULL


write.csv(owid_final, paste0(fp, 'monthly_owid.csv'), row.names = F)


##### 5. CDC (NOT BY SEX) ####################################################################################################################################################

#get data
cdc <- fread(paste0(fp, 'FILEPATH/cdc_data.csv'))
cdc[, data_source := 'CDC']

#label age
cdc[, age := '18+']

#get month
cdc[, month := month(date)]
cdc[, year := year(date)]

#get exposed
cdc[, any_vax_sample := vaccinated*sample]
cdc[, full_vax_sample := full_vax*sample]

#sum any vaccines by location-sex-month
cdc1_x <- as.data.table(cdc)[, mean(any_vax_sample), by = .(location_id, location_name, data_source, month, age, year)] %>%
  setnames('V1', 'dose1')
cdc_sample <- as.data.table(unique(cdc[, c('location_id', 'location_name', 'data_source', 'sample')]))

cdc_any <- merge(cdc1_x, cdc_sample, by =c('location_id', 'location_name', 'data_source'))
cdc_any[, vaccinated := dose1/sample]

#sum full vaccines by location-sex-month
cdc2_x <- as.data.table(cdc)[, mean(full_vax_sample), by = .(location_id, location_name, data_source, month, age, year)] %>%
  setnames('V1', 'dose2')

cdc_full <- merge(cdc2_x, cdc_sample, by =c('location_id', 'location_name', 'data_source'))
cdc_full[, full_vax := dose2/sample]

#merge all
cdc_final <- merge(cdc_any, cdc_full, by =c('location_id', 'location_name', 'data_source', 'month', 'age', 'year', 'sample'))

#edit
cdc_final$dose1 <- NULL
cdc_final$dose2 <- NULL


write.csv(cdc_final, paste0(fp, 'monthly_cdc.csv'), row.names = F)