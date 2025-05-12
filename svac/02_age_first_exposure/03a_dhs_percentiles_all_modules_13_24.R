####################################################################################################################################################################
# 
# Author: USERNAME
# Purpose: DHS distributions of age at first SV experience - sensitivity analysis with no modules excluded among 15-24 year olds
#
####################################################################################################################################################################

rm(list=ls())

# set-up environment -----------------------------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(survival)
library(readstata13)
library(ggridges)
library(wesanderson, lib.loc='FILEPATH')
library(ggsci)

invisible(sapply(list.files("FILEPATH", full.names = T), source))
source('FILEPATH/core_functions.R')
source('FILEPATH/demogsurv_functions.R')
locs <- get_location_metadata(22, release_id=9)

data <- fread('FILEPATH')

data2 <- copy(data)
data2[age_first_exp==99, age_first_exp:=NA]
data2 <- data2[!is.na(age_first_exp)]
data2 <- data2[age_year<25]

np <- copy(data2)

percentile_by_region <- function(region, age=11){
  percentile <- ecdf(np[super_region_name==region]$age_first_exp)
  dt <- data.table(super_region_name=region, percentile=percentile(age))
  setnames(dt, 'percentile', paste0('percentile_age', age))
  return(dt)
}

percentile_age_11 <- lapply(unique(np$super_region_name), percentile_by_region) %>% rbindlist()

percentile_by_region <- function(region, age=15){
  percentile <- ecdf(np[super_region_name==region]$age_first_exp)
  dt <- data.table(super_region_name=region, percentile=percentile(age))
  setnames(dt, 'percentile', paste0('percentile_age', age))
  return(dt)
}

percentile_age_15 <- lapply(unique(np$super_region_name), percentile_by_region) %>% rbindlist()

percentile_by_region <- function(region, age=17){
  percentile <- ecdf(np[super_region_name==region]$age_first_exp)
  dt <- data.table(super_region_name=region, percentile=percentile(age))
  setnames(dt, 'percentile', paste0('percentile_age', age))
  return(dt)
}

percentile_age_17 <- lapply(unique(np$super_region_name), percentile_by_region) %>% rbindlist()

percentiles <- merge(percentile_age_11, percentile_age_15, by='super_region_name')
percentiles <- merge(percentiles, percentile_age_17, by='super_region_name')

percentile <- ecdf(np$age_first_exp)

cases <- np[!is.na(age_first_exp), .(total_cases=.N), by=.(survey_year)]
np <- merge(np, cases, by='survey_year')

ss1 <- unique(np[, .(total_cases, survey_year, super_region_name)])
ss <- ss1[, .(n=sum(total_cases)), by=.(super_region_name)]

global_row <- data.table(super_region_name='All data', 
                         percentile_age11=percentile(11), 
                         percentile_age15=percentile(15),
                         percentile_age17=percentile(17), n=sum(ss$n))

percentiles <- merge(percentiles, ss, by='super_region_name')

percentiles <- rbind(percentiles, global_row)
percentiles[, names(percentiles)[names(percentiles) %like% 'percentile']:=lapply(.SD, function(x) round(x*100, 1)), .SDcols=names(percentiles)[names(percentiles) %like% 'percentile']]

write.csv(percentiles, 'FILEPATH', row.names=F)

