####################################################################################################################################################################
# 
# Author: USERNAME
# Purpose: Missingness analysis for age at first sexual violence analysis using DHS and VACS modules
#
####################################################################################################################################################################

rm(list=ls())

# set-up environment -----------------------------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(openxlsx)
library(stringr)
library(survival)
library(readstata13)
library(ggridges)
library(wesanderson, lib.loc='FILEPATH')
library(ggsci)
library(finalfit, lib.loc='FILEPATH')

invisible(sapply(list.files("FILEPATH", full.names = T), source))
source('FILEPATH/core_functions.R')
source('FILEPATH/demogsurv_functions.R')
locs <- get_location_metadata(22, release_id=9)

unw_vars <- c("nid", "ihme_loc_id", "location_name", "year_id", "sex_id", "source")

## DHS Associations --------------------------------------------------------------------------------------------------------------------------------------
dhs <- fread('FILEPATH')
dhs[age_first_exp>age_year, age_first_exp:=99]
dhs[, missing:=ifelse(age_first_exp %in% c(98,99,88),"missing","non-missing")]
dhs_full <- copy(dhs)
dhs <- dhs[ever_sv==1]
young_dhs <- dhs[age_year<25]

# Age
t.test(dhs$age_year~dhs$missing)
t.test(young_dhs$age_year~young_dhs$missing)

# Urbanicity
tbl_Length<-table(dhs$urban,dhs$missing) #1 = urban, 2 = rural
prop.table(tbl_Length,margin = 2)
urbanicity_x2 <- chisq.test(tbl_Length)
urbanicity_x2
urbanicity_x2$observed

# Years of education
dhs[education %in%98:99, education:=NA]
hist(dhs$education)
dhs[education>=6, completed_primary_edu:='completed primary education']
dhs[education<6, completed_primary_edu:='did not complete primary education']
dhs[education==0, ever_school:='never been to school']
dhs[education>0, ever_school:='ever school']

#completed primary
tbl_Length<-table(dhs$completed_primary_edu,dhs$missing)
prop.table(tbl_Length,margin = 2)
edu_x2 <- chisq.test(tbl_Length)
edu_x2
edu_x2$observed

## VACS Associations --------------------------------------------------------------------------------------------------------------------------------------

vacs <- fread('FILEPATH')
vacs[, source:='VACS']
vacs[!is.na(sv_touching_age_first) | !is.na(sv_attempted_sex_age_first) | !is.na(sv_pressured_sex_age_first) | !is.na(sv_phys_forced_sex_age_first), ever_sv:=1]
vacs[is.na(sv_touching_age_first) & is.na(sv_attempted_sex_age_first) & is.na(sv_pressured_sex_age_first) & is.na(sv_phys_forced_sex_age_first), ever_sv:=0]
vacs[, year_id:=str_sub(survey_year, -4,-1)]

vacs <- vacs[ever_sv==1]
vacs[age_first_exp>age_year, age_first_exp:=99]
vacs[age_year<13 | age_year>24, age_year:=NA]
vacs[, missing:=ifelse(age_first_exp %in% c(98,99,88),"missing","non-missing")]
vacs[completed_primary_edu==99, completed_primary_edu:=NA]
vacs[current_school==99, current_school:=NA]
vacs[ever_school==99, ever_school:=NA]
vacs_fem <- vacs[sex_id==2]
vacs_male <- vacs[sex_id==1]

# Age
t.test(vacs$age_year~vacs$missing)
t.test(vacs_fem$age_year~vacs_fem$missing)
t.test(vacs_male$age_year~vacs_male$missing)

# Education
edu_cts <- vacs[, .N, by=.(missing, completed_primary_edu, sex_id)]

#completed primary edu
tbl_Length<-table(vacs$completed_primary_edu,vacs$missing)
prop.table(tbl_Length,margin = 2)
edu_x2 <- chisq.test(tbl_Length)
edu_x2
edu_x2$observed

#sex id
tbl_Length<-table(vacs$sex_id,vacs$missing)
prop.table(tbl_Length,margin = 2)
sex_x2 <- chisq.test(tbl_Length)
sex_x2
sex_x2$observed

