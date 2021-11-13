#############################################################################################################
#
# Author: USERNAME
# Purpose: Clean and process goalkeeper's general population premise data from both 2020 and 2021
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

invisible(sapply(list.files("FILEPATH", full.names = T), source))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
out.dir <- "FILEPATH"
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

#load raw data
new.dt <- fread('FILEPATH')
old.dt <- fread('FILEPATH')
old.dt[is.na(location_id) & country=='United States', location_id:=102]

#clean up data from 2021 collection -----------------------------------------------------------------------------------------------------------------------------------------------------
condition.list <- c('prev_routine', 'heart_disease', 'stroke', 'cancer', 'diabetes', 'kidney_disease', 'asthma', 'tb', 'covid19',
                    'pneumonia', 'other_lung_disease', 'hiv', 'malaria', 'liver_disease', 'mental_health', 'injury', 'hearing_vision',
                    'bone_joint_muscle', 'sexual_reproductive', 'alc_drug', 'high_cholesterol', 'high_bp', 'other')
reason.list <- c('health_facility_closed', 'turned_away_from_healthfacility', 
                 'not_available', 'no_transport', 'no_money', 'partner_family_disapprove',
                 'unable_access_due_to_covid_rest','fear_of_covid_infection', 'other', 'DR')
med.reason.list <- c('health_facility_closed', 'turned_away_from_healthfacility', 
                     'not_available', 'no_transport', 'no_money', 'unable_access_due_to_covid_rest',
                     'fear_of_covid_infection', 'partner_family_disapprove', 'forgot_to_take', 'other', 'DR')

new.dt[, `:=` (data_collection='2021', time_period='Feb 2021 - May 2021')]

#recode need for hc visit and conditions
setnames(new.dt, c('gp_provider_need', paste0('gp_provider_condition_', c(1:22,99))), 
         c('need_hc', paste0('need_visit_', condition.list)))

#recode no access
new.dt[!is.na(gp_provider_visit), hc_no_access:=ifelse(gp_provider_visit==0,1,0)]
new.dt[need_hc==0, hc_no_access:=0] #recode those who didn't need hc to be included in denom

#recode location of care
setnames(new.dt, c(paste0('gp_provider_where_', c(1:3,99))), c('hc_at_facility', 'hc_at_home', 'hc_online', 'hc_other'))

#recode reason for no access
setnames(new.dt, paste0('gp_provider_why_', c(1:8, 99, 88)), paste0('hc_no_access_', reason.list))

#recode questions that are global symptoms survey equivalents
fb.b13.equiv <- c('emergency_transport', 'inpatient_care', 'outpatient_care', 'preventative_health', 'medication', 'ppe', 'health_products')
setnames(new.dt, paste0('gp_unmet_need_', 1:7), fb.b13.equiv)

fb.b14.equiv <- c('did_not_know_where', 'could_not_afford', 'unable_to_travel', 'afraid_of_infection', 'not_available', 'none_of_the_above')
setnames(new.dt, paste0('gp_unmet_need_reasons_', c(1:5,0)), fb.b14.equiv)

#medication questions
setnames(new.dt, c('gp_medication', paste0('gp_medication_condition_', c(1:22,99)), paste0('gp_miss_dose_why_', c(1:9, 99, 88))),
         c('need_med', paste0('need_med_', condition.list), paste0('med_no_access_', med.reason.list)))

#re-code no access to medication (e.g. do not include missed bc forgot to take)
new.dt[!is.na(gp_miss_dose), med_no_access:=ifelse(gp_miss_dose==1 & med_no_access_forgot_to_take!=1, 1, 0)]
new.dt[need_med==0, med_no_access:=0] #recode those who didn't need medicine to be in denom

#list of final vars of interest
final.vars.new <- c("observation_id", "weight", "location_id", "country", "gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion", "data_collection", 'time_period',
                    'need_hc', paste0('need_visit_', condition.list), paste0('hc_no_access_', reason.list), 'hc_no_access', 'hc_at_facility', 'hc_at_home', 'hc_online', 'hc_other',
                    fb.b13.equiv, fb.b14.equiv, 'need_med', paste0('need_med_', condition.list), paste0('med_no_access_', med.reason.list), 'med_no_access')

#subset
clean.new <- new.dt[, .SD, .SDcols=c(final.vars.new)]

#clean data from 2020 collection, starting with 'post-pandemic' conditions -----------------------------------------------------------------------------------------------------------------------------------------------------
old.dt.pre <- copy(old.dt) #copy for pre-pandemic conditions section below

#set metadata
old.dt[, `:=` (data_collection='2020', time_period='March 2020 - July 2020')]

#recode need for hc visit and conditions
setnames(old.dt, c('gp_post_provider_need', paste0('gp_post_provider_condition_', c(1:22,99))), 
         c('need_hc', paste0('need_visit_', condition.list)))

#recode no access
old.dt[!is.na(gp_post_provider_visit), hc_no_access:=ifelse(gp_post_provider_visit %in% c('No', 'I saw a provider during this time, but not every time I needed one',
                                                                                          "No – I did not see a provider", "No - I did not see a provider", 
                                                                                          "Partly – I saw a provider during this time, but not every time I needed",
                                                                                          "Partly - I saw a provider during this time, but not every time I needed",
                                                                                          "No I did not see a provider"), 1, 0)]
old.dt[need_hc=='No', hc_no_access:=0]#recode those who did not need services to be included in denom

#recode reasons wide
reasons <- c('Health facility closed', 'Turned away from health facility', 'Treatment or tests unavailable', 'No transportation', 
             'Lack of money', 'Unable to access due to lockdown restrictions', 'Fear of being infected with COVID-19', 
             'Partner or family does not approve', 'Other', 'Decline to respond', 'Prefer not to answer')
for (r in reasons){
  old.dt[hc_no_access==1, paste0('hc_no_access_', tolower(gsub(' ', '_', r))):=ifelse(gp_post_provider_why == paste0(r),1,0)]
}

#standardize to new module
setnames(old.dt, c('hc_no_access_fear_of_being_infected_with_covid-19', 'hc_no_access_unable_to_access_due_to_lockdown_restrictions'),
         c('hc_no_access_fear_of_covid_infection', 'hc_no_access_unable_access_due_to_covid_rest'))

#medication questions
setnames(old.dt, c('gp_medication', paste0('gp_medication_condition_', c(1:22,99)), paste0('gp_post_miss_dose_why_', c(1:9, 99, 88))),
         c('need_med', paste0('need_med_', condition.list), paste0('med_no_access_', med.reason.list)))

#re-code no access to medication (e.g. do not include missed bc forgot to take)
old.dt[!is.na(gp_post_miss_dose), med_no_access:=ifelse(gp_post_miss_dose=='Yes' & med_no_access_forgot_to_take!=1, 1, 0)]
old.dt[need_med=='No', med_no_access:=0] #include those who didn't need medicine in denom

#list of final vars of interest
final.vars.old <- c("observation_id", "weight", "location_id", "country", "gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion", "data_collection", 'time_period',
                    'need_hc', paste0('need_visit_', condition.list), names(old.dt)[names(old.dt) %like% 'hc_no_access_'], 'hc_no_access', 'need_med', paste0('need_med_', condition.list), 
                    paste0('med_no_access_', med.reason.list), 'med_no_access')

#subset to final vars
clean.old <- old.dt[, .SD, .SDcols=c(final.vars.old)]

#now do pre-pandemic questions --------------------------------------------------------------------------------------------------------------------------------------------------
pre.med.reason.list <- c('health_facility_closed', 'turned_away_from_healthfacility', 
                         'not_available', 'no_transport', 'no_money', 'partner_family_disapprove', 'forgot_to_take', 'other', 'DR')

#set metadata
old.dt.pre[, `:=` (data_collection='2020', time_period='Dec 2019 - Feb 2020')]

#recode need for hc visit and conditions
setnames(old.dt.pre, c('gp_pre_provider_need', paste0('gp_pre_provider_condition_', c(1:22,99))), 
         c('need_hc', paste0('need_visit_', condition.list)))

#recode no access
old.dt.pre[!is.na(gp_pre_provider_visit), hc_no_access:=ifelse(gp_pre_provider_visit %in% c('No', 'I saw a provider during this time, but not every time I needed one',
                                                                                            "No – I did not see a provider", "No - I did not see a provider", 
                                                                                            "Partly – I saw a provider during this time, but not every time I needed",
                                                                                            "Partly - I saw a provider during this time, but not every time I needed",
                                                                                            "No I did not see a provider"), 1, 0)]
old.dt.pre[need_hc=='No', hc_no_access:=0]#recode those who did not need services to be included in denom

#recode reasons wide
pre.reasons <- c('Health facility closed', 'Turned away from health facility', 'Treatment or tests unavailable', 'No transportation', 
                 'Lack of money', 'Partner or family does not approve', 'Other', 'Decline to respond', 'Prefer not to answer')
for (r in pre.reasons){
  old.dt.pre[hc_no_access==1, paste0('hc_no_access_', tolower(gsub(' ', '_', r))):=ifelse(gp_pre_provider_why == paste0(r),1,0)]
}

#medication questions
setnames(old.dt.pre, c('gp_medication', paste0('gp_medication_condition_', c(1:22,99)), paste0('gp_pre_miss_dose_why_', c(1:7, 99, 88))),
         c('need_med', paste0('need_med_', condition.list), paste0('med_no_access_', pre.med.reason.list)))

#re-code no access to medication (e.g. do not include missed bc forgot to take)
old.dt.pre[!is.na(gp_pre_miss_dose), med_no_access:=ifelse(gp_pre_miss_dose=='Yes' & med_no_access_forgot_to_take!=1, 1, 0)]
old.dt.pre[need_med=='No', med_no_access:=0] #include those who didn't need medicine in denom

#list of final vars of interest
final.vars.pre <- c("observation_id", "weight", "location_id", "country", "gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion", "data_collection", 'time_period',
                    'need_hc', paste0('need_visit_', condition.list), names(old.dt.pre)[names(old.dt.pre) %like% 'hc_no_access_'], 'hc_no_access', 'need_med', paste0('need_med_', condition.list), 
                    paste0('med_no_access_', pre.med.reason.list), 'med_no_access')

#subset to final vars
clean.pre <- old.dt.pre[, .SD, .SDcols=c(final.vars.pre)]

# write and format compiled data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clean.genhealth <- rbind(clean.new, clean.old, clean.pre, fill=T)
write.csv(clean.genhealth, 'FILEPATH')

#create custom healthcare indicators -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#create indicators 'due to covid' using a combination of survey questions from 2021 module only
clean.genhealth[!is.na(outpatient_care) & !is.na(inpatient_care) & !is.na(preventative_health), 
                healthcare:=ifelse(outpatient_care==1 | inpatient_care==1 | preventative_health==1, 1, 0)]
clean.genhealth[!is.na(healthcare), healthcare_covid:=ifelse(healthcare==1 & (unable_to_travel==1 | afraid_of_infection==1), 1, 0)]
clean.genhealth[!is.na(health_products), health_products_covid:=ifelse(health_products==1 & (unable_to_travel==1 | afraid_of_infection==1), 1, 0)]
clean.genhealth[!is.na(preventative_health), preventative_health_covid:=ifelse(preventative_health==1 & (unable_to_travel==1 | afraid_of_infection==1), 1, 0)]
clean.genhealth[!is.na(medication), medication_covid:=ifelse(medication==1 & (unable_to_travel==1 | afraid_of_infection==1), 1, 0)]

#create 'due to covid' indicators using a combination of questions from 2020/2021 modules
clean.genhealth[!is.na(hc_no_access), hc_visit_dis_covid:=ifelse(hc_no_access==1 & (hc_no_access_unable_access_due_to_covid_rest==1 |
                                                                                      hc_no_access_fear_of_covid_infection==1),1,0)]
clean.genhealth[!is.na(hc_no_access), hc_visit_dis_avoidance:=ifelse(hc_no_access==1 & hc_no_access_fear_of_covid_infection==1,1,0)]
clean.genhealth[!is.na(med_no_access), med_no_access_covid:=ifelse(med_no_access==1 & (med_no_access_unable_access_due_to_covid_rest==1 |
                                                                                         med_no_access_fear_of_covid_infection==1),1,0)]
clean.genhealth[!is.na(med_no_access), med_no_access_avoidance:=ifelse(med_no_access==1 & med_no_access_fear_of_covid_infection==1,1,0)]

#create indicators for those who needed sexual/reproductive healthcare or medicine specifically
clean.genhealth[, srhc_visit:=ifelse(need_visit_sexual_reproductive==1 & hc_no_access==1, 1, 0)]
clean.genhealth[, srhc_meds:=ifelse(need_med_sexual_reproductive==1 & med_no_access==1,1,0)]
clean.genhealth[, srhc:=ifelse(srhc_visit == 1 | srhc_meds == 1, 1, 0)]
clean.genhealth[, srhc_visit_covid:=ifelse(srhc_visit==1 & (hc_no_access_unable_access_due_to_covid_rest==1 |
                                                              hc_no_access_fear_of_covid_infection==1),1,0)]
clean.genhealth[, srhc_meds_covid:=ifelse(srhc_meds==1 & (med_no_access_unable_access_due_to_covid_rest==1 |
                                                            med_no_access_fear_of_covid_infection==1),1,0)]
clean.genhealth[, srhc_covid:=ifelse(srhc_meds_covid==1 | srhc_visit_covid==1,1,0)]

#save file with custom indicators
write.csv(clean.genhealth, 'FILEPATH')
