#############################################################################################################
#
# Author: Cory Spencer
# Purpose: Create proportions for selected indicators from time-series sources; UPDATED FOR RTR
#
#############################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)

#paths
out.dir <- 'FILEPATH'

#functions: 

#create one large file
read_draw <- function(c.file){
  print(c.file)
  fread(c.file) -> dat
  dat$V1 <- NULL
  return(dat)
}

#calculate props for finmark
prop <- function(dt, var) {
    dt <- as.data.table(dt)
    dt[, paste0(var):=as.numeric(get(var))]
    dt <- dt[!get(var) %in% c('SKIP', 'MISSING', NA)]
    dt[, weighted_var:=get(var)*weight]
    #get sample size
    denom <- dt[, .(denom=sum(weight)), by=.(sex, date, location_name)]
    sample <- dt[, .(sample=.N), by=.(sex, date, location_name)]
    #get num who said yes to reason out of all who felt less safe
    n <- dt[, .(n=sum(weighted_var)), by=c('sex', 'date', 'location_name', paste0(var))][get(var)==1]
    #calculate prop and se
    props <- merge(n, denom, by=c('sex', 'location_name', 'date'))[, paste0(var):=NULL]
    props <- merge(props, sample, by=c('sex', 'location_name', 'date'))
    props[, proportion:=n/denom]
    props[, indicator:=paste0(var)]
    return(props)
}


# 1. global symptoms survey, healthcare disruptions due to covid ---------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- fread(paste0('FILEPATH/', gsub('-', '_', Sys.Date()), '.csv'))

#remove AFG for resub
data <- dt[location_name!='Afghanistan']

#format for prop calc
data$date <- format(data$date, '%Y-%m')
dem.vars <- c("location_id", "location_name", "sex", "date", "weight", "pid")
b13_names <- c('emergency_transport', 'inpatient_care', 'outpatient_care', 'preventative_health', 'medication', 'ppe', 'health_products')
data <- data[, .SD, .SDcols=c(dem.vars, b13_names, paste0(b13_names, '_covid'), 'healthcare', 'healthcare_covid')]
data <- melt.data.table(data, id.vars=dem.vars)

#set levels of aggregation for props (location/gender/date)
c.lvls2 <- c('location_id', 'location_name', 'sex', 'date', 'variable', 'value')
c.lvls <- c('location_id', 'location_name', 'sex', 'date', 'variable')

#Get numerators and samples
props <- data[,.(num=sum(weight,na.rm=T),obs=uniqueN(pid,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings, no skips)
props[!(value%in%c("SKIP","MISSING")),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!(value%in%c("SKIP","MISSING")),obs_nm:=sum(obs,na.rm=T),by=c.lvls]
props[!(value%in%c("SKIP","MISSING")),prop_nm:=num/denom_nm]
props[!(value%in%c("SKIP","MISSING"))&(num==0|denom_nm==0),prop_nm:=0]
props[!(value%in%c("SKIP","MISSING")),prop_nm_lower:=prop_nm- (1.96 * sqrt((prop_nm*(1-prop_nm))/obs_nm))]
props[!(value%in%c("SKIP","MISSING")),prop_nm_upper:=prop_nm+ (1.96 * sqrt((prop_nm*(1-prop_nm))/obs_nm))]

#format for central saving
fb.data <- copy(props)
fb.data[, `:=` (data_source='Facebook Global Symptoms Survey', age='18+')]
fb.data <- fb.data[sex %in% c('Male', 'Female') & value=='1' & obs_nm>30]
fb.data[, c('num', 'denom_nm', 'obs', 'prop_nm_lower', 'prop_nm_upper', 'value'):=NULL]
setnames(fb.data, c('prop_nm', 'obs_nm', 'variable'), c('proportion', 'sample', 'indicator'))
setcolorder(fb.data, neworder=c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'data_source', 'age'))
write.csv(fb.data, paste0(out.dir, 'fb_healthcare_microdata_formatted_for_timeseries.csv'))

#2. finmark trust medication disruptions due to covid ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# finmark <- fread('FILEPATH/cleaned_microdata.csv')[, V1:=NULL]
finmark <- fread('FILEPATH/finmark_cleaned_microdata.csv')[, V1:=NULL]
names(finmark)[names(finmark)%like%'reasons_for_not_accessing_medicine'] <- str_sub(names(finmark)[names(finmark)%like%'reasons_for_not_accessing_medicine'], 36, -1)
finmark <- finmark[, c('access_to_medicine', 'was_scared_to_go_buy', 'was_scared_to_buy', 'scared_to_buy', 'was_scared_to_go_purchase_it', 'reasons_for_not_accessing_medic4',
                       'Gender', 'AgeGroup', 'ihme_loc_id', 'pweight', 'wave', 'urban_city', 'education_level')]

#'scared to purchase' reason stored in separate variables across country modules;
#combine these variables into one single master variable here:
finmark[, scared_to_purchase_master:=was_scared_to_go_buy]
finmark[is.na(scared_to_purchase_master), scared_to_purchase_master:=was_scared_to_buy]
finmark[is.na(scared_to_purchase_master), scared_to_purchase_master:=scared_to_buy]
finmark[is.na(scared_to_purchase_master), scared_to_purchase_master:=was_scared_to_go_purchase_it]
finmark[is.na(scared_to_purchase_master), scared_to_purchase_master:=reasons_for_not_accessing_medic4]

#create medication access disruption due to covid from combination of survey questions
finmark[!access_to_medicine %in% c('DK/Refused', '', "DON'T KNOW", 'REFUSED'), medication:=ifelse(access_to_medicine=='YES',1,0)]
finmark[, missing_fup:=ifelse(access_to_medicine=='YES' & !scared_to_purchase_master %in% c('No', 'Was scared to go to purchase it'), 'missing', 0)]
finmark[!is.na(medication) & missing_fup!='missing', 
        medication_covid:=ifelse(access_to_medicine=='YES' & (scared_to_purchase_master=='Was scared to go to purchase it'),1,0)]

#format for central saving
finmark <- finmark[, c('medication', 'medication_covid', 'Gender', 'AgeGroup', 'ihme_loc_id', 'pweight', 'wave', 'urban_city', 'education_level')]
setnames(finmark, c('Gender', 'AgeGroup', 'pweight', 'wave', 'urban_city', 'education_level'), c('gender', 'age', 'weight', 'date', 'urbanicity', 'education'))
finmark <- merge(finmark, locs[, c('ihme_loc_id', 'location_name')], by='ihme_loc_id')

#recode waves as dates
finmark[date %like% 'W1' | date %like% 'Wave 1', wave:=1]
finmark[date %like% 'W2', wave:=2]
finmark[date %like% 'W3', wave:=3]
finmark[date %like% 'W4', wave:=4]
finmark[date %like% 'W5', wave:=5]

finmark <- finmark %>% mutate(date2= case_when(
  ihme_loc_id == "GHA" & wave == 1 ~ "2020-06-25",
  ihme_loc_id == "GHA" & wave == 2 ~ "2020-07-25",
  ihme_loc_id == 'GHA' & wave == 3 ~ '2020-09-27',
  ihme_loc_id == "KEN" & wave == 1 ~ "2020-04-15",
  ihme_loc_id == "KEN" & wave == 2 ~ "2020-05-01",
  ihme_loc_id == "KEN" & wave == 3 ~ "2020-05-25",
  ihme_loc_id == "KEN" & wave == 4 ~ "2020-06-05",
  ihme_loc_id == "KEN" & wave == 5 ~ "2020-09-10",
  ihme_loc_id == "NGA" & wave == 1 ~ "2020-04-10",
  ihme_loc_id == "NGA" & wave == 2 ~ "2020-05-01",
  ihme_loc_id == "NGA" & wave == 3 ~ "2020-06-01",
  ihme_loc_id == "NGA" & wave == 4 ~ "2020-06-11",
  ihme_loc_id == "RWA" & wave == 1 ~ "2020-04-24",
  ihme_loc_id == "RWA" & wave == 2 ~ "2020-05-01",
  ihme_loc_id == "RWA" & wave == 3 ~ "2020-05-27",
  ihme_loc_id == "RWA" & wave == 4 ~ "2020-06-15",
  ihme_loc_id == "RWA" & wave == 5 ~ "2020-08-30",
  ihme_loc_id == "UGA" & wave == 1 ~ "2020-05-01",
  ihme_loc_id == "UGA" & wave == 2 ~ "2020-06-01",
  ihme_loc_id == "UGA" & wave == 3 ~ "2020-06-16",
  ihme_loc_id == "UGA" & wave == 4 ~ "2020-09-04",
  ihme_loc_id == "ZAF" & wave == 1 ~ "2020-04-12",
  ihme_loc_id == "ZAF" & wave == 2 ~ "2020-04-27",
  ihme_loc_id == "ZAF" & wave == 3 ~ "2020-05-27",
  ihme_loc_id == "ZAF" & wave == 4 ~ "2020-06-13",
  ihme_loc_id == "ZMB" & wave == 1 ~ "2020-06-01",
  ihme_loc_id == "ZMB" & wave == 2 ~ "2020-06-20",
  ihme_loc_id == "ZMB" & wave == 3 ~ "2020-07-22",
  ihme_loc_id == "ZMB" & wave == 4 ~ "2020-09-12",))
finmark$yearmonth <- format(as.Date(finmark$date2), "%Y-%m")
finmark[, c('date', 'date2', 'wave'):=NULL]
setnames(finmark, c('gender', 'yearmonth'), c('sex', 'date'))

#get weighted proportions 
fm.data <- prop(finmark, c('medication_covid'))

#clean up for central saving
fm.data[, `:=` (data_source='FinMark Trust', age='18+')]
fm.data[, c('n', 'denom'):=NULL]
fm.data <- merge(fm.data, locs[, c('location_name', 'location_id')], by='location_name')
fm.data <- fm.data[sex %in% c('Male', 'Female')]
setcolorder(fm.data, neworder=c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'data_source', 'age'))
write.csv(fm.data, paste0(out.dir, 'finmark_healthcare_microdata_formatted_for_timeseries.csv'))

#3. Goalkeepers healthcare disruptions ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gk <- fread('FILEPATH/health_inds_clean_weights_combo_indicators.csv')
dem.vars <- c("observation_id", "weight", "location_id", "country", "gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion", "data_collection", 'time_period')

#subset to indicators of interest, clean up locations and variable naming
gk.hc <- gk[, .SD, .SDcols= c(dem.vars, 'healthcare', 'healthcare_covid', 'hc_visit_dis_covid', 'hc_no_access', 'hc_visit_dis_avoidance', 'healthcare_avoidance',
                              'medication', 'medication_covid', 'med_no_access', 'med_no_access_covid', 'health_products', 'health_products_covid')]
gk.hc <- melt.data.table(gk.hc, id.vars=dem.vars)
gk.hc <- merge(gk.hc, locs, by='location_id')
gk.hc <- gk.hc[, c('location_id', 'location_name', 'weight', 'observation_id', 'variable', 'time_period', 'gender', 'age', 'geography', 'education', 'value')]
setnames(gk.hc, c('geography', 'time_period', 'gender'), c('urbanicity', 'date', 'sex'))

#set dates
gk.hc[, date:=trimws(str_sub(date,-9,-1))]
gk.hc <- gk.hc[date!='Feb 2020'] #remove pre-pandemic time point

#for 2021 module, use variables exactly equivalent to global symptoms survey and remove alternate construction;
gk.hc <- gk.hc[!(variable %in% c('med_no_access', 'med_no_access_covid', 'med_no_access_avoidance', 'hc_no_access', 'hc_visit_dis_covid', 'hc_visit_dis_avoidance') &
                   date=='May 2021')]

#for 2020 module, reassign alternate construction to same variable names as 2021 and global symptom surveys (equivalent indicators)
gk.hc[variable=='med_no_access', variable:='medication']
gk.hc[variable=='med_no_access_covid', variable:='medication_covid']
gk.hc[variable=='hc_no_access', variable:='healthcare']
gk.hc[variable=='hc_visit_dis_covid', variable:='healthcare_covid']
gk.hc[variable=='hc_visit_dis_avoidance', variable:='healthcare_avoidance']

#set stratifiers for proportions
c.lvls2 <- c('location_id', 'location_name', 'sex', 'date', 'variable', 'value')
c.lvls <- c('location_id', 'location_name', 'sex', 'date', 'variable')

#Get numerators and sample sizes
props <- gk.hc[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings, no skips)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),obs_nm:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),prop_nm:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),prop_nm:=0]

#subset to TRUE values, apply ss restriction, and clean up for central saving
gk.data <- copy(props[value==1 & obs_nm>30])
gk.data[, `:=` (data_source='Goalkeepers', age='16+')]
gk.data[, c('num', 'obs', 'denom_nm', 'value'):=NULL]
setnames(gk.data, c('obs_nm', 'prop_nm', 'variable'), c('sample', 'proportion', 'indicator'))
gk.data <- gk.data[sex %in% c('Male', 'Female')]
gk.data[date=='July 2020', date2:='2020-07-30']
gk.data[date=='May 2021', date2:='2021-05-31']
gk.data[, date2:=as.Date(date2)]
gk.data[, date:=format(date2, "%Y-%m")]
gk.data[, c('date2'):=NULL]
setcolorder(gk.data, neworder=c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'data_source', 'age'))
write.csv(gk.data, paste0(out.dir, 'gk_healthcare_microdata_weighted_formatted_for_timeseries.csv'))

#4. FB GEAH healthcare disruptions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#read in and initial formatting
fb.geah <- fread('FILEPATH/fb_geah_cleaned_microdata.csv')[, V1:=NULL]
fb.geah <- fb.geah[, c('location_name', 'gender', 'weight', 'healthcare_covid', 'health_products_covid')]
fb.geah <- melt.data.table(fb.geah, measure.vars=c('healthcare_covid', 'health_products_covid'), value.name='value', variable.name='variable')
setnames(fb.geah, 'gender', 'sex')

#set stratifiers for props
c.lvls2 <- c('location_name', 'sex', 'variable', 'value')
c.lvls <- c('location_name', 'sex', 'variable')

#Get numerators and sample sizes, removing missing sex observations
fb.geah[, observation_id:=1:.N]
fb.geah <- fb.geah[!is.na(sex)]
props <- fb.geah[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings, no skips)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),proportion:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),proportion:=0]

#subset to TRUE values and clean for central saving
geah.data <- copy(props[value==1 & sample>30])
geah.data[, `:=` (date='2020-07', data_source='Facebook Gender Equality at Home', age='18+')]

#formatting locs
geah.data[location_name=='Bolivia', location_name:='Bolivia (Plurinational State of)']
geah.data[location_name=='Vietnam', location_name:='Viet Nam']
geah.data[location_name=='Taiwan', location_name:='Taiwan (Province of China)']
geah.data[location_name=='Swaziland', location_name:='Eswatini']
geah.data[location_name=='Czech Republic', location_name:='Czechia']
geah.data[location_name=='Laos', location_name:="Lao People's Democratic Republic"]
geah.data[location_name=='Macedonia', location_name:='North Macedonia']
geah.data[location_name=='Moldova', location_name:='Republic of Moldova']
geah.data[location_name=='Russia', location_name:='Russian Federation']
geah.data <- merge(geah.data, locs, by='location_name', all.x=T)
geah.data <- geah.data[!(location_name=='Georgia' & super_region_name=='High-income')] #remove repeat observations merged on for US state Georgia

#finish cleaning for central saving, save
geah.data <- geah.data[, c('location_id', 'location_name', 'sex', 'variable', 'sample', 'proportion', 'date', 'data_source', 'age')]
setnames(geah.data, 'variable', 'indicator')
setcolorder(geah.data, neworder=c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'data_source', 'age'))
write.csv(geah.data, paste0(out.dir, 'fb_geah_healthcare_microdata_formatted_for_timeseries.csv'))

#5. UN WOMEN RGA healthcare disruptions -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rga <- fread('/FILEPATH/un_women_rga/rga_harmonized_for_regs.csv')[ihme_loc_id!='CHL']
rga_uga <- fread("FILEPATH/un_women_rga/rga_uganda_microdata_cleaned.csv")
rga <- rbind(rga, rga_uga, fill=T)[, location_id:=NULL]     
rga <- merge(rga, locs[, .(ihme_loc_id, location_id)], by='ihme_loc_id', all.x=T)
rga <- rga[, c('sex', 'location_name', 'super_region_name', 'regional_file', 'weight', 'healthcare_covid', 'health_products_covid', 'ihme_loc_id',
               'location_id')]

rga <- rga %>% mutate(date = case_when(
  ihme_loc_id == "MOZ" ~ "2020-11-01",
  ihme_loc_id == "CAF" ~ "2020-11-01",
  ihme_loc_id == "GIN" ~ "2020-10-01",
  ihme_loc_id == "MLI" ~ "2020-11-01",
  ihme_loc_id == "SEN" ~ "2020-08-01",
  ihme_loc_id == "CIV" ~ "2020-09-01",
  ihme_loc_id == 'UGA' ~ "2020-11-30", 
  ihme_loc_id == 'COL' ~ "2020-10-09",
  ihme_loc_id == 'MEX' ~ '2020-11-15',
  regional_file == 'arab_states' ~ '2020-05-27',
  ihme_loc_id == 'ETH' ~ '2020-11-15',
  ihme_loc_id == 'KEN' ~ '2020-09-15',
  ihme_loc_id == 'MWI' ~ '2020-11-30',
  ihme_loc_id == 'ZAF' ~ '2020-11-15',
  ihme_loc_id == 'SWZ' ~ '2021-02-08',
  ihme_loc_id == 'ALB' ~ '2020-04-25',
  ihme_loc_id == 'BIH' ~ '2020-05-15',
  ihme_loc_id == 'GEO' ~ '2020-05-08',
  ihme_loc_id == 'MDA' ~ '2020-04-25',
  ihme_loc_id == 'MKD' ~ '2020-05-20',
  ihme_loc_id == 'TUR' ~ '2020-05-25',
  ihme_loc_id == 'AZE' ~ '2020-05-03',
  ihme_loc_id == 'SRB' ~ '2020-07-30',
  ihme_loc_id == 'ARM' ~ '2020-06-01', #no date available for armenia
  ihme_loc_id == 'BLR' ~ '2020-06-01', #no date available for belarus
))
rga[, date:=as.Date(date)]
rga[, yearmonth:=format(date, "%Y-%m")]
rga[, date:=yearmonth]
rga <- melt.data.table(rga, measure.vars=c('healthcare_covid', 'health_products_covid'))

#set stratifiers
c.lvls2 <- c('location_name', 'location_id', 'sex', 'variable', 'value', 'date')
c.lvls <- c('location_name', 'location_id', 'sex', 'variable', 'date')

#Get numerators and samples
rga[, observation_id:=1:.N]
rga <- rga[!is.na(sex)]
props <- rga[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings, no skips)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),proportion:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),proportion:=0]

#subset to TRUE values and format for central saving
rga.data <- copy(props[value==1 & sample>30])
rga.data[, `:=` (data_source='UN Women RGA', age='18+')]
rga.data <- rga.data[, c('location_id', 'location_name', 'sex', 'variable', 'sample', 'proportion', 'date', 'data_source', 'age')]
setnames(rga.data, 'variable', 'indicator')
setcolorder(rga.data, neworder=c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'data_source', 'age'))
write.csv(rga.data, paste0(out.dir, 'rga_healthcare_microdata_formatted_for_timeseries.csv'))

#CREATE ONE FINAL FILE FOR EASY READ-IN -----------------------------------------------------------------------------------------------------------------------------------------------------------------

fm.data <- fread(paste0(out.dir, 'finmark_healthcare_microdata_formatted_for_timeseries.csv'))
fb.data <- fread(paste0(out.dir, 'fb_healthcare_microdata_formatted_for_timeseries.csv'))
fb.data <- fb.data[indicator %in% c('medication', 'medication_covid', 'healthcare', 'healthcare_covid',
                                    'healthcare_avoidance', 'health_products', 'health_products_covid',
                                    'preventative_health', 'preventative_health_covid')]
gk.data <- fread(paste0(out.dir, '/gk_healthcare_microdata_weighted_formatted_for_timeseries.csv'))
geah.data <- fread(paste0(out.dir, 'fb_geah_healthcare_microdata_formatted_for_timeseries.csv'))
un.women.data <- fread(paste0(out.dir, 'rga_healthcare_microdata_formatted_for_timeseries.csv'))

all.timeseries <- rbind(fm.data, fb.data, gk.data, geah.data, un.women.data)
write.csv(all.timeseries, paste0(out.dir, 'all_healthcare_microdata_formatted_for_timeseries_', gsub('-', '_', Sys.Date()), '.csv'))
