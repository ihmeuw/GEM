#############################################################################################################################################################
# 
# Purpose: Compare DAILY YouGov and CTIS hesitancy data
#
#############################################################################################################################################################

rm(list=ls())

run_date <- Sys.Date()

dir.create(paste0('/FILEPATH/', run_date, '/'), recursive = T)
output_dir <- paste0('/FILEPATH/', run_date, '/')

##### 0. Set Up #############################################################################################################################################################
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


##### 1. Get data #############################################################################################################################################################
#fb daily hes
source('/FILEPATH/00_get_fb_daily_hes_data.R')
rm(fb_daily_hes_csa)

#yougov
yougov <- fread(paste0('/FILEPATH/2021-10-01/input_data/cleaned_yougov.csv'))


##### 2. Clean YouGov data #############################################################################################################################################################
#subset
yougov <- yougov[, c('qweek', 'endtime', 'country', 'gender', 'weight', 'age', 'vac', 'vac5')] 

#make dates
yougov[, date := as.Date(substr(endtime, 1, 10), "%d/%m/%Y")]

yougov[, check := paste0(date, country, gender)]

#loop through unique country-sex-date combinations
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


##### 3. Calculate YouGov hesitancy #############################################################################################################################################################

#assign indiv hes based on hes question
yougov[vac5 == 'Yes', unweighted_hesitant := 0]
yougov[vac5 == 'Not sure', unweighted_hesitant := 1]
yougov[vac5 == 'No', unweighted_hesitant := 1]
yougov[, unweighted_N := 1]

#assign weigthts
yougov[, hesitant := unweighted_hesitant*weight]
yougov[, N := unweighted_N*weight]

#sum hesitancy responses (hesitant)
yougov_resp <- as.data.table(yougov)[, sum(hesitant), by = .(country, qweek, date, gender)] %>%
  setnames('V1', 'hesitant') %>%
  as.data.table()

#sum sample size (N)
yougov_sample <- as.data.table(yougov)[, sum(N), by = .(country, qweek, date, gender)] %>%
  setnames('V1', 'N') %>%
  as.data.table()

yougov_raw <- as.data.table(yougov)[, sum(unweighted_N), by = .(country, qweek, date, gender)] %>%
  setnames('V1', 'raw_sample') %>%
  as.data.table()

#merge + calc hes
vac5 <- merge(yougov_resp, yougov_sample, by = c('country', 'qweek', 'date', 'gender')) %>%
  merge(yougov_raw, by = c('country', 'qweek', 'date', 'gender'))

vac5[, prop1 := hesitant/N]

#exclude  samples < 30
vac5 <- vac5[raw_sample >= 30]

#keep hesitancy data
yougov_hes <- copy(vac5[,c('date', 'country', 'gender', 'prop1', 'N', 'raw_sample')])
setnames(yougov_hes, 'raw_sample', 'sample')
setnames(yougov_hes, 'N', 'denom_nm')

#label
yougov_hes[, data_source := 'yougov']


##### 5. Merge CTIS + YouGov data #############################################################################################################################################################

#rename YG to match CTIS
setnames(yougov_hes, 'gender', 'sex')
setnames(yougov_hes, 'country', 'location_name')

#subset CTIS to  YG locations
fb_temp <- copy(fb_daily_hes_cs)
setnames(fb_temp, 'hesitancy', 'prop1')
fb_temp[, c('location_id', 'num')] <- NULL
fb_temp[, date := as.Date(date)]

#merge CTIS + YG
yg_fb_hes <- rbind(yougov_hes, fb_temp)
setnames(yg_fb_hes, 'prop1','proportion')
yg_fb_hes[, age := '18+']
yg_fb_hes[, variable := 'hesitancy']

write.csv(yg_fb_hes, paste0(output_dir, 'hesitancy_by_sex.csv'))
