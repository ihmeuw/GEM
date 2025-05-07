####################################################################################################################################################################
# 
# Author: USERNAME
# Purpose: Aggregate distributions of age at first SV experience using DHS and VACS Data
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


dhs <- fread('FILEPATH/all_dhs_cleaned_indiv_level_data.csv')
dhs[age_first_exp==99, age_first_exp:=NA]

dhs_cor <- dhs[!is.na(age_first_exp) & !is.na(age_year)]
cor(dhs_cor$age_first_exp, dhs_cor$age_year)

dhs <- dhs[age_year<25]
dhs[, source:='DHS']

# apply < 50% missingness exclusion criteria
dhs <- dhs[age_first_exp_missingness<=0.5]

vacs <- fread('FILEPATH/all_vacs_cleaned_indiv_level_data.csv')
vacs[age_first_exp>age_year, age_first_exp:=99]
vacs[age_first_exp %in% c(99,98,88), age_first_exp:=NA]
vacs[, source:='VACS']
vacs[age_year>100, age_year:=NA]

fvacs_cor <- vacs[sex_id==2 & !is.na(age_first_exp) & !is.na(age_year)]
cor(fvacs_cor$age_first_exp, fvacs_cor$age_year)

mvacs_cor <- vacs[sex_id==1 & !is.na(age_first_exp) & !is.na(age_year)]
cor(mvacs_cor$age_first_exp, mvacs_cor$age_year)

#female only data
vacs <- vacs[sex_id==2]
data2 <- rbind(dhs, vacs, fill=T)
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

cases <- np[!is.na(age_first_exp), .(n=.N), by=.(super_region_name)]
np <- merge(np, cases, by='super_region_name')

global_row <- data.table(super_region_name='All data', 
                         percentile_age11=percentile(11), 
                         percentile_age15=percentile(15),
                         percentile_age17=percentile(17), n=sum(cases$n))

percentiles <- merge(percentiles, cases, by='super_region_name')

percentiles <- rbind(percentiles, global_row)
percentiles[, names(percentiles)[names(percentiles) %like% 'percentile']:=lapply(.SD, function(x) round(x*100, 1)), .SDcols=names(percentiles)[names(percentiles) %like% 'percentile']]

write.csv(percentiles, paste0('FILEPATH'), row.names=F)





