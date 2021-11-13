#############################################################################################################################################################
# 
# Purpose: Compare DAILY YouGov and CTIS hesitancy data by sex and age
#
#############################################################################################################################################################

rm(list=ls())

run_date <- Sys.Date()

dir.create(paste0('/FILEPATH/', run_date, '/'), recursive = T)
output_dir <- paste0('/FILEPATH/', run_date, '/')

##### 0. SET UP #############################################################################################################################################################
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
source("/FILEPATH/get_age_metadata.R")
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")


##### 1. GET DATA #############################################################################################################################################################
#ctis daily hes
source('/FILEPATH/00_get_fb_daily_hes_data.R')
rm(fb_daily_hes_cs)

#yougov
source('/FILEPATH/00_get_yougov_data.R')
yougov <- fread(paste0('/FILEPATH/', Sys.Date(), '/input_data/cleaned_yougov.csv'))


##### 2. CLEAN YOUGOV #############################################################################################################################################################
#subset
yougov <- yougov[, c('qweek', 'endtime', 'country', 'gender', 'weight', 'age', 'vac', 'vac5')] 

#make dates
yougov[, date := as.Date(substr(endtime, 1, 10), "%d/%m/%Y")]
yougov[, check := paste0(date, country, gender)]

#loop through unique country-sex-date combinations to find waves with this question
to_remove <- unique(yougov[,c('date', 'country','gender','vac','vac5', 'check')])
loop_later <- list()
for (i in unique(to_remove$check)) {
  hold <- to_remove[check == i]
  if (length(unique(hold$vac5)) > 1 | length(unique(hold$vac5)) == 0) {print('next')}
  else if (unique(hold$vac5) == '-1' | is.na(unique(hold$vac5))) {loop_later <- append(loop_later, i)} 
}
yougov <- yougov[check %not in% loop_later]
yougov$check <- NULL

#fix gateway
yougov[vac %like% 'Yes', vac5 := 'Yes']

#remove NAs
yougov <- yougov[!is.na(vac5) & vac5 != -1]

#fix location names
yougov[country %like% 'Korea', country := 'Republic of Korea']
yougov[country %like% 'United States', country := 'United States of America']

#assign age
yougov[between(age, 18, 24), age_group := '18-24']
yougov[between(age, 25, 34), age_group := '25-34']
yougov[between(age, 35, 44), age_group := '35-44']
yougov[between(age, 45, 54), age_group := '45-54']
yougov[between(age, 55, 64), age_group := '55-64']
yougov[between(age, 65, 74), age_group := '65-74']
yougov[between(age, 75, 200), age_group := '75+']

#save copy just in case
og_yougov <- copy(yougov)


##### 3. CALC YOUGOV HESITANCY #############################################################################################################################################################

#assign indiv hes based on hes question
yougov[vac5 == 'Yes', unweighted_hesitant := 0]
yougov[vac5 == 'Not sure', unweighted_hesitant := 1]
yougov[vac5 == 'No', unweighted_hesitant := 1]
yougov[, unweighted_N := 1]

#assign weighted hes
yougov[, hesitant := unweighted_hesitant*weight]
yougov[, N := unweighted_N*weight]

#sum hesitancy responses (hesitant)
yougov_resp <- as.data.table(yougov)[, sum(hesitant), by = .(country, qweek, date, age_group, gender)] %>%
  setnames('V1', 'hesitant') %>%
  as.data.table()

#sum sample size (N)
yougov_sample <- as.data.table(yougov)[, sum(N), by = .(country, qweek, date, age_group, gender)] %>%
  setnames('V1', 'N') %>%
  as.data.table()

#sum raw sample size (N)
yougov_raw <- as.data.table(yougov)[, sum(unweighted_N), by = .(country, qweek, date, age_group, gender)] %>%
  setnames('V1', 'raw_sample') %>%
  as.data.table()

#merge + calc hes
vac5 <- merge(yougov_resp, yougov_sample, by = c('country', 'qweek', 'date', 'age_group', 'gender')) %>%
  merge(yougov_raw, by = c('country', 'qweek', 'date', 'age_group', 'gender'))

vac5[, proportion := hesitant/N]

#exclude raw samples < 30
vac5 <- vac5[raw_sample >= 30]

#keep hesitancy data
yougov_hes <- copy(vac5[,c('date', 'country', 'age_group', 'gender', 'N', 'raw_sample', 'proportion')])

#label
yougov_hes[, data_source := 'yougov']


##### 5. MERGE CTIS + YOUGOV DATA #############################################################################################################################################################

#rename YG to match CTIS
setnames(yougov_hes, 'gender', 'sex')
setnames(yougov_hes, 'country', 'location_name')
setnames(yougov_hes, 'raw_sample', 'sample')
setnames(yougov_hes, 'N', 'denom_nm')

#subset CTIS to  YG locations
fb_temp <- copy(fb_daily_hes_csa)
setnames(fb_temp, 'hesitancy', 'proportion')
fb_temp[, c('location_id', 'num')] <- NULL
fb_temp[, date := as.Date(date)]

#merge CTIS + YG
yg_fb_hes <- rbind(yougov_hes, fb_temp)

#label
yg_fb_hes[, indicator := 'hesitancy']

#save
write.csv(yg_fb_hes, paste0(output_dir, 'hesitancy_by_age_sex.csv'))