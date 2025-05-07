####################################################################################################################################################################
# 
# Author: USERNAME
# Purpose: Clean and collapse VACS microdata extractions for violence indicators
#
####################################################################################################################################################################

rm(list=ls())

library(data.table)
library(readstata13)
library(readr)
library(magrittr)
library(stringr)
library(binom)
library(Hmisc)
library(DBI)
library(RMySQL)
library(survey)
library(haven)
library(knitr)
library(dplyr)
library(ini)
library(bit64)

jpath <- "FILEPATH"
user  <- Sys.getenv(x="USER")
source('FILEPATH/db_tools.r')
source('FILEPATH/collapse.r')


# 1. CLEAN RAW DATA ----------------------------------------------------------------------------------------------------------------------------------------------------------------

out_dir <- 'FILEPATH'
vacs_mapping <- fread('FILEPATH/vacs_variables.csv')
vacs_cb <- fread('FILEPATH/vacs_basic_codebook.csv')[!is.na(nid)]
vacs_edu <- fread('FILEPATH/vacs_education.csv')
file_paths <- vacs_mapping$filepath
age_vars <- c("sv_touching_age_first", "sv_attempted_sex_age_first","sv_pressured_sex_age_first","sv_phys_forced_sex_age_first")

for (f in file_paths){
  print(f)
  data <- as.data.table(read.dta13(f, convert.factors=F))
  names(data) <- tolower(names(data))
  var_names <- c()
  for (a in age_vars){
    if (f == "FILEPATH" & a=="sv_touching_age_first"){
      data[f103b>=f101b, paste0(a):=f101b]
      data[f101b>f103b, paste0(a):=f103b]
      data[is.na(f101b) & !is.na(f103b), paste0(a):=f103b]
      data[!is.na(f101b) & is.na(f103b), paste0(a):=f101b]
    } else {
      var <- tolower(vacs_mapping[filepath==f, get(a)])
      data[, paste0(a):=get(var)]
    }
  }
  data2 <- copy(data)
  
  #set metadata
  n <- vacs_mapping[filepath==f]$nid
  sm <- vacs_mapping[filepath==f]$survey_module
  data2[, nid:=n]
  data2[, survey_module:=sm]
  data2[, ihme_loc_id:=vacs_mapping[filepath==f]$ihme_loc_id]
  data2[, year_id:=vacs_mapping[filepath==f]$year_id]
  
  #set demo data
  if (sm == 'WN') {
    data2[, sex_id:=2]
  } else if (sm == 'MN') {
    data2[, sex_id:=1]
  } else {
    data2[, sex_id:=get(tolower(vacs_cb[nid==n & survey_module==sm]$sex))]
  }
  
  data2[, age_year:=get(tolower(vacs_cb[nid==n & survey_module==sm]$age_year))]
  
  if (vacs_cb[nid==n & survey_module==sm]$strata == ''){
    data2[, strata:='']
  } else if (vacs_cb[nid==n & survey_module==sm]$strata == 'prov, provstr') {
    data2[,strata:='prov']
  } else {
    data2[, strata:=get(tolower(vacs_cb[nid==n & survey_module==sm]$strata))]
  }
  
  data2[, psu:=get(tolower(vacs_cb[nid==n & survey_module==sm]$psu))]
  data2[, pweight:=get(tolower(vacs_cb[nid==n & survey_module==sm]$pweight))]
  
  #educational attainment variables
  data2[, ever_school:=get(tolower(vacs_edu[nid==n & survey_module==sm]$ever_school))]
  data2[, current_school:=get(tolower(vacs_edu[nid==n & survey_module==sm]$in_school))]

  data2[get(tolower(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu)) %in% as.integer(unlist(strsplit(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu_true, ","))), completed_primary_edu:=1]
  data2[get(tolower(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu)) %in% as.integer(unlist(strsplit(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu_false, ","))), completed_primary_edu:=0]
  data2[get(tolower(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu)) %in% as.integer(unlist(strsplit(vacs_edu[nid==n & survey_module==sm]$completed_primary_edu_missing, ","))), completed_primary_edu:=99]
  
  data2 <- data2[, .SD, .SDcols=c(age_vars, 'pweight', 'psu', 'strata', 'age_year', 'sex_id', 'year_id', 'ihme_loc_id', 'nid', 'ever_school', 'current_school', 'completed_primary_edu')]
  
  #find minimum across all first ages reported
  numeric_vars <- c('psu', 'pweight', age_vars)
  data2[, (numeric_vars):=lapply(.SD, as.numeric), .SDcols=numeric_vars]
  data2[, age_first_exp:=pmin(sv_touching_age_first, sv_attempted_sex_age_first, sv_pressured_sex_age_first, sv_phys_forced_sex_age_first, na.rm=T)]
  
  save_name <- paste0(unique(data2$nid), '_', unique(data2$ihme_loc_id), '_', sm, '_', unique(data2$year_id))
  dir.create(paste0(out_dir, '01_cleaned/'))
  write.csv(data2, paste0(out_dir, '01_cleaned/', save_name, '.csv'), row.names=F)
}
