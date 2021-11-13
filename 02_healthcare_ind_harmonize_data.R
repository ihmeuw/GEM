#############################################################################################################
#
# Author: USERNAME
# Purpose: Harmonize non-Facebook healthcare indicator data for standard covariate values 
#          in preparation for individual-level logistic regressions
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
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

# 1. GOALKEEPERS ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#read in and format
gk <- fread('FILEPATH')
dem.vars <- c("observation_id", "weight", "location_name", "location_id", "country", "gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion", "data_collection", 'time_period')
gk <- merge(gk,locs,by='location_id',all.x=T)

#subset to relevant indicators
gk.hc <- gk[, .SD, .SDcols= c(dem.vars, 'healthcare', 'healthcare_covid', 'hc_visit_dis_covid', 'hc_no_access', 'hc_visit_dis_avoidance', 'healthcare_avoidance',
                              'medication', 'medication_covid', 'med_no_access', 'med_no_access_covid', 'health_products', 'health_products_covid', 'preventative_health_covid')]

gk.hc <- melt.data.table(gk.hc, id.vars=dem.vars)
gk.hc <- gk.hc[variable %in% c('healthcare', 'healthcare_covid', 'hc_visit_dis_covid', 'hc_no_access', 'hc_visit_dis_avoidance', 'healthcare_avoidance',
                               'medication', 'medication_covid', 'med_no_access', 'med_no_access_covid', 'medication_avoidance', 'med_no_access_avoidance',
                               'health_products', 'health_products_covid', 'preventative_health_covid')]

gk.hc <- gk.hc[, c('location_name', 'variable', 'time_period', 'gender', 'age', 'geography', 'education', 'value')]
gk.hc[, source:='Goalkeepers']
setnames(gk.hc, c('geography', 'time_period'), c('urbanicity', 'date'))

#set dates
gk.hc[, date:=trimws(str_sub(date,-9,-1))]
gk.hc[date=='July 2020', date2:='2020-07-30']
gk.hc[date=='Feb 2020', date2:='2020-02-28']
gk.hc[date=='May 2021', date2:='2021-05-31']
gk.hc[, date2:=as.Date(date2)]
gk.hc[, yearmonth:=format(date2, "%Y-%m")]
gk.hc[, c('date', 'date2'):=NULL]

#for 2021 module, use variables exactly equivalent to global symptoms survey and remove alternate construction;
gk.hc <- gk.hc[!(variable %in% c('med_no_access', 'med_no_access_covid', 'med_no_access_avoidance', 'hc_no_access', 'hc_visit_dis_covid', 'hc_visit_dis_avoidance') & 
                   yearmonth=='2021-05')]

#for 2020 module, reassign alternate construction to same variable names as 2021 and global symptom surveys (equivalent indicators)
gk.hc[variable=='med_no_access', variable:='medication']
gk.hc[variable=='med_no_access_covid', variable:='medication_covid']
gk.hc[variable=='med_no_access_avoidance', variable:='medication_avoidance']
gk.hc[variable=='hc_no_access', variable:='healthcare']
gk.hc[variable=='hc_visit_dis_covid', variable:='healthcare_covid']
gk.hc[variable=='hc_visit_dis_avoidance', variable:='healthcare_avoidance']

#remove pre-pandemic time period
gk.hc <- gk.hc[yearmonth!='2020-02']

# 3. FACEBOOK GENDER EQUALITY AT HOME ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#read in and format
fb.geah <- fread('FILEPATH')[, V1:=NULL]
fb.geah[, `:=` (source='Facebook Gender Equality at Home', date='July 2020')]
fb.geah <- fb.geah[, c('location_name', 'gender', 'urbanicity', 'weight', 'healthcare_covid', 'health_products_covid', 'source', 'date', 'education')]
fb.geah <- melt.data.table(fb.geah, measure.vars=c('healthcare_covid', 'health_products_covid'), value.name='value', variable.name='variable')
fb.geah <- fb.geah[!is.na(value)]
fb.geah[, date:='2020-07-30']
fb.geah[, date:=as.Date(date)]
fb.geah[, yearmonth:=format(date, "%Y-%m")]
fb.geah[, date:=NULL]

#3. FINMARK -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#read in and format
finmark <- fread('FILEPATH')[, V1:=NULL]
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

#formatting
finmark <- finmark[, c('medication', 'medication_covid', 'Gender', 'AgeGroup', 'ihme_loc_id', 'pweight', 'wave', 'urban_city', 'education_level')]
setnames(finmark, c('Gender', 'AgeGroup', 'pweight', 'wave', 'urban_city', 'education_level'), c('gender', 'age', 'weight', 'date', 'urbanicity', 'education'))
finmark <- melt.data.table(finmark, measure.vars = c('medication', 'medication_covid'))
finmark[, source:='FinMark Trust']
finmark <- merge(finmark, locs[, c('ihme_loc_id', 'location_name')], by='ihme_loc_id')
finmark[date %like% 'W1' | date %like% 'Wave 1', wave:=1]
finmark[date %like% 'W2', wave:=2]
finmark[date %like% 'W3', wave:=3]
finmark[date %like% 'W4', wave:=4]
finmark[date %like% 'W5', wave:=5]

#dates for finmark waves
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

#rbind to one master file
all <- rbind(finmark, fb.geah, gk.hc, fill=T)[, ihme_loc_id:=NULL]

#Harmonize all data for standard covariate bins ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#age NAs
all[is.na(age) | age=='Not Available', age:=NA]

#age categories for surveys that do not cut off at 45+, allowing for a 65+ bin
all <- all %>%
  mutate(age_nogoalkeepers = case_when(
    age %in% c('16-', '16-24', '16-25', '18-24', '25-34', '26-35') ~ "Less than 35", 
    age %in% c('35-44', '36-45', '45-54', '55-64') ~ "35 to 64", 
    age %in% c('65-74', '65+', '75+') ~ "More than 65"
  ))

all <- all %>% 
  mutate(
    lessthan35 = case_when(
      age_nogoalkeepers == "Less than 35" ~ 1,
      age_nogoalkeepers %in% c("35 to 64", "More than 65") ~0), 
    age35to64 = case_when(
      age_nogoalkeepers == "35 to 64" ~ 1,
      age_nogoalkeepers %in% c("Less than 35", "More than 65") ~0),
    age65plus = case_when(
      age_nogoalkeepers == "More than 65" ~ 1, 
      age_nogoalkeepers %in% c("Less than 35", "35 to 64") ~0)
  )

#otherwise, create age categories for surveys that are restricted to 45+
all <- all %>%
  mutate(age_simple = case_when(
    age %in% c('16-', '16-24', '16-25', '18-24', '16 to 25 years old') ~ "Less than 25", 
    age %in% c('25-34', '26-35', '35-44', '36-45', '26 to 35 years old', '36 to 45 years old') ~ "25 to 45", 
    age %in% c('45-54', '46+', '55-64', '65-74', '65+', '75+', 'Over 45 years old', '55+') ~ "More than 45"
  ))
all <- all %>%
  mutate(age25less = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~1,
    age_simple == 'More than 45' ~0
  ))
all <- all %>%
  mutate(age25to45 = case_when(
    age_simple == '25 to 45' ~ 1,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~0
  ))
all <- all %>%
  mutate(age45more = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~1
  ))

#education bins: primary/secondary/tertiarty/none
all[education %in% c('Primary', 'Complete primary', 'Primary School', 
                     'Primary complete', 'Primary school', 'Primary school completed',
                     'Some secondary / high school', 'Secondary incomplete',
                     'Some secondary', 'Secondary not complete', 'Incomplete secondary',
                     'Secondary[1st Cycle]', 'Some secondary school / high school'), edu_dummy:='Primary']
all[education %in% c('Secondary Technical', 'Secondary', 'Complete secondary', 'Secondary School',
                     'Incomplete tertiary', 'Technical/vocational training',
                     'Technical & Vocational Training', 'Secondary complete',
                     'Secondary / A-level', 'Technical training after secondary complete',
                     'Some technical training after secondary',
                     'Secondary school/ high school completed',
                     'Some university or college', 'Secondary/high school',
                     'Some technical education (e.g polytechnic school)', 
                     'Technical school diploma or degree completed', 'Technical school',
                     'Some university', 'Secondary[2nd Cycle]'), edu_dummy:='Secondary']
all[education %in% c('Post graduate', 'College or university', 'University or college degree completed',
                     'Post-graduate education', 'University', 'Diploma / Degree', 'Higher',
                     'University complete', 'College [Middle level]',
                     'Higher than secondary', 'Completed Post Graduate',
                     'Completed tertiary or higher', 'Tertiary or higher', 
                     'Completed University/College', 'Complete tertiary or higher', '2'), edu_dummy:='Tertiary or higher'] #FB GEAH, 2=more than secondary
all[education %in% c('No education', 'None', 'No formal education'), edu_dummy:='None']
all[education %in% c('Incomplete primary',
                     'Primary not complete', 'Some primary', 'Pre-primary / Grade R', 
                     'Religious', 'Pre-primary', 'Preschool', 'Some primary education'), edu_dummy:='None']
all[education %in% c('REFUSED', "DON'T KNOW", 'DK/Refused', 'NO ANSWER/DO NOT KNOW',
                     'NR/DK'), edu_dummy:=NA]
all[education=='1', edu_dummy:='Secondary or less'] #FB GEAH, 1=secondary or less

#make a binary higher education bin
all[edu_dummy %in% c('Secondary or less', 'None', 'Primary', 'Secondary'), higher_edu:=0]
all[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#urbanicity: rural/urban
unique(all$urbanicity)
all[urbanicity %in% c('Urban [City or Town]', 'Urban',
                      'Urban area', 'Big city', 'Capital city/Big city', 
                      'Capital city', 'Other city', 'Big city', 'Suburban/Peri-urban',
                      'City', 'City center or metropolitan area', 'Small city/Small town', 'Small town'), urbanicity_dummy:='Urban']
all[urbanicity %in% c('Village/Rural', 'Village/rural',
                      'Rural',  'Rural [Village or Farm]', 'Rural area'), urbanicity_dummy:='Rural']
all[urbanicity %in% c("DON'T KNOW", "REFUSED"), urbanicity_dummy:=NA]
all[, rural:=ifelse(urbanicity_dummy=='Rural',1,ifelse(urbanicity_dummy=='Urban',0,NA))]

#create binary female indicator
all[!is.na(gender), female:=ifelse(gender=='Female',1,0)]

#ensure values are numeric
all[, value:=as.numeric(value)]

#now read in RGA (already harmonized to cov bins):
rga <- fread('FILEPATH')
setnames(rga, c('sex'), c('gender'))
rga[, c('unsafe_at_home', 'community_gbv_change'):=NULL]
rga <- melt.data.table(rga, measure.vars=c('health_products_covid', 'healthcare_covid'))
rga <- rga[, .SD, .SDcols=c('gender', 'age', 'weight','education', 'variable', 'value', 'source', 'location_name', 
                            'lessthan35', 'age35to64', 'age65plus', 'rural', 'higher_edu', 'female')]

rga.hc.locs <- unique(rga[variable=='healthcare_covid' & !is.na(value)]$location_name)
rga.hp.locs <- unique(rga[variable=='health_products_covid' & !is.na(value)]$location_name)

rga <- rga[(variable=='healthcare_covid' & location_name %in% rga.hc.locs) | (variable=='health_products_covid' & location_name %in% rga.hp.locs)]

#rbind
all <- rbind(all, rga, fill=T)

#write master file with cleaned covariate bins
out.dir <- 'FILEPATH'
write.csv(all, paste0(out.dir, 'hc_microdata_harmonized_for_regs.csv'))
