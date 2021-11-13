#############################################################################################################
#
# Author: USERNAME
# Purpose: Clean and harmonize women's health module from Goalkeeper's Data 2020 and 2021; GBV indicators
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

#load data
new_dt <- fread('FILEPATH') %>% filter(gender != "Male" & age!="Under 16")
old_dt <- fread('FILEPATH') %>% filter(gender != "Male")

# 1. GBV indicators -----------------------------------------------------------------------------------------------------------------------------------------
dem.vars <- c("location_id", "country","observation_id","gender","age","geography","financial_situation","education", "employment_status","ethnicity", "religion")

old_gbv <- old_dt[wmn_alone=='Yes', .SD, .SDcols=c(dem.vars, 'wmn_alone', 'wmn_how_safe', 'wmn_safe_change', 'wmn_safe_place', 
                                                   'wmn_pre_safe_place', 'wmn_post_safe_place', 'wmn_safe_place_no_access',
                                                   'wmn_safe_place_no_access_why', 'wmn_pre_help', 'wmn_post_help', 'wmn_post_no_help', 
                                                   'wmn_no_help_why', 'wmn_help_resource')][, data_collection:='2020']

new_gbv <- new_dt[wmn_alone==1, .SD, .SDcols=c(dem.vars, 'wmn_alone', 'wmn_how_safe_community', 'wmn_how_safe', 'wmn_how_safe_change', 'wmn_safe_place',
                                               'wmn_safe_place_need', 'wmn_safe_place_howoften', 'wmn_safe_place_no_access',
                                               'wmn_safe_place_no_access_why', 'wmn_help', 'wmn_help_unmet_need', 'wmn_help_unmet_need_why', 
                                               'wmn_partner_violence', 'wmn_partner_violence_change', 'wmn_employment_loss', 'wmn_partner_alcohol',
                                               'wmn_partner_alcohol_change', 'wmn_married')][, data_collection:='2021']

#pre-pandemic indicators -----------------------------------------------------------------------------------------------------------------------------------------

#harmonize naming/coding of old variables with new module, when possible
setnames(old_gbv, c('wmn_safe_change', 'wmn_post_help', 'wmn_post_no_help', 'wmn_no_help_why'), 
         c('wmn_how_safe_change', 'wmn_help', 'wmn_help_unmet_need', 'wmn_help_unmet_need_why'))

#create var corresponding to new question by coding whether post safe place often > pre safe place often
old_gbv[wmn_post_safe_place %in% c('Decline to respond', "Don't know", 'No Response'), wmn_post_safe_place:=NA]

#map categorical answers to numerical sequence
mapping <- data.table(category=c('Never', 'Rarely', 'Once a month', 'Once a week', 'Every day'),
                      num_ranking=c(0,1,2,3,4))

#merge numerical ranking onto dt
old_gbv <- merge(old_gbv, mapping, by.x='wmn_pre_safe_place', by.y='category')
setnames(old_gbv, 'num_ranking', 'wmn_pre_safe_place_numerical')
old_gbv <- merge(old_gbv, mapping, by.x='wmn_post_safe_place', by.y='category')
setnames(old_gbv, 'num_ranking', 'wmn_post_safe_place_numerical')

#create binary column for those who had to go to safe place more frequently post-March
old_gbv[, wmn_safe_place_howoften:=ifelse(wmn_post_safe_place_numerical>wmn_pre_safe_place_numerical,1,0)]

#create merged dt
gbv <- rbind(old_gbv, new_gbv, fill=T)

#safety indicators
gbv[wmn_how_safe_community %in% c(77,88), wmn_how_safe_community:=NA]
gbv[!is.na(wmn_how_safe_community), community_unsafe_binary:=ifelse(wmn_how_safe_community %in% c(1,2), 1, 0)]
gbv[wmn_how_safe %in% c('Decline to respond', 'DonÃ¢â‚¬â„¢t know', 77, 88), wmn_how_safe:=NA]
gbv[, hh_unsafe_binary:=ifelse(wmn_how_safe %in% c('Very unsafe', 'A little unsafe', 1, 2), 1, 0)]
gbv[wmn_how_safe_change %in% c('Decline to respond', 'DonÃ¢â‚¬â„¢t know', 77, 88), wmn_how_safe_change:=NA]
gbv[, hh_less_safe_since_covid:=ifelse(wmn_how_safe_change %in% c('Less safe', 1), 1, 0)]
gbv[, safe_place_avail:=ifelse(wmn_safe_place %in% c('Yes', 1), 1, 0)]
gbv[is.na(wmn_safe_place_need)&data_collection=='2020'&safe_place_avail==1, wmn_safe_place_need:=1] #new module has gateway for if need this place (wmn_safe_place_need), but old module did not
gbv[safe_place_avail==1 & wmn_safe_place_need==1, safe_place_more_often_since_covid:=ifelse(wmn_safe_place_howoften==1, 1, 0)]
gbv[safe_place_avail==1 & wmn_safe_place_need==1, safe_place_no_access:=ifelse(wmn_safe_place_no_access %in% c(1, 'Yes'), 1, 0)]
gbv[, safe_place_access_barrier:=wmn_safe_place_no_access_why]
gbv[safe_place_access_barrier %in% c(1, 'Unable to access place due to lockdown'), safe_place_access_barrier:='Unable to access place due to COVID-19 restrictions']
gbv[safe_place_access_barrier %in% c(2, 'Place was closed or unavailable for reason other than lockdown',
                                     'Place was close or unavailable for reason other than lockdown'), safe_place_access_barrier:='Place was closed or unavailable for reason other than COVID-19 restrictions']
gbv[safe_place_access_barrier==3, safe_place_access_barrier:='Afraid of consequences']
gbv[safe_place_access_barrier==4, safe_place_access_barrier:='No transportation']
gbv[safe_place_access_barrier==5, safe_place_access_barrier:='Fear of being infected with COVID-19']
gbv[safe_place_access_barrier==99, safe_place_access_barrier:='Other']
gbv[safe_place_access_barrier==88, safe_place_access_barrier:='Decline to respond']
gbv[!is.na(safe_place_access_barrier), safe_place_access_barrier_covidrelated:=ifelse(safe_place_access_barrier %in% c("Unable to access place due to COVID-19 restrictions",
                                                                                                                       "Fear of being infected with COVID-19"), 1, 0)]
#GBV services
gbv[wmn_help %in% c('Decline to respond', 'DonÃ¢â‚¬â„¢t know', 77, 88), wmn_help:=NA]
gbv[!is.na(wmn_help), sought_services_since_covid:=ifelse(wmn_help %in% c(1, 'Yes'), 1, 0)]
gbv[wmn_help_unmet_need %in% c('Decline to respond', 'DonÃ¢â‚¬â„¢t know', 77, 88), wmn_help_unmet_need:=NA]
gbv[!is.na(wmn_help_unmet_need), services_no_access:=ifelse(wmn_help_unmet_need %in% c(1, 'Yes'),1,0)]
gbv[, services_access_barrier:=wmn_help_unmet_need_why]
gbv[services_access_barrier==1, services_access_barrier:='Did not know where to go']
gbv[services_access_barrier==2, services_access_barrier:='Too far from services']
gbv[services_access_barrier==3, services_access_barrier:='Could not afford transport or service fees']
gbv[services_access_barrier==4, services_access_barrier:='Services unavailable due to COVID-19 restrictions']
gbv[services_access_barrier%in%c(5,"Unable to travel due to COVID-19 restrictions","Unable to travel due to COVID-19 restriction"), services_access_barrier:='Unable to travel due to COVID-19 restrictions']
gbv[services_access_barrier==6, services_access_barrier:='Fear of being infected with COVID-19']
gbv[services_access_barrier==8, services_access_barrier:='Fear of threats/consequences/getting into trouble']
gbv[services_access_barrier==7, services_access_barrier:='Embarrassed for myself or my family']
gbv[services_access_barrier==99, services_access_barrier:="Other"]
gbv[services_access_barrier==88, services_access_barrier:='Decline to Answer']
gbv[services_access_barrier %in% c(77, "DonÃ¢â‚¬â„¢t know"), services_access_barrier:="Don’t know"]
gbv[!is.na(services_access_barrier), services_access_barrier_covidrelated:=ifelse(services_access_barrier %in% c("Services unavailable due to COVID-19 restrictions",
                                                                                                                 'Unable to travel due to COVID-19 restrictions',
                                                                                                                 "Fear of being infected with COVID-19"), 1, 0)]
gbv[!is.na(sought_services_since_covid) & !is.na(services_no_access), sought_or_wanted_gbvservices:=ifelse(sought_services_since_covid==1 | services_no_access==1, 1, 0)]

#questions included in 2021 module only
gbv[!is.na(wmn_partner_alcohol), partner_alc:=ifelse(wmn_partner_alcohol==1, 1, 0)]
gbv[!is.na(wmn_partner_alcohol_change), partner_alc_more_since_covid:=ifelse(wmn_partner_alcohol_change==2, 1, 0)]
gbv[!is.na(wmn_partner_violence_change), community_gbv_change:=ifelse(wmn_partner_violence_change==3, 1, ifelse(wmn_partner_violence_change %in% 1:2, 0, NA))]
gbv[!is.na(wmn_partner_violence), community_gbv_common:=ifelse(wmn_partner_violence %in% c(3,4), 1, ifelse(wmn_partner_violence %in% 1:2, 0, NA))]
gbv[!is.na(wmn_employment_loss), lost_income_since_covid:=ifelse(wmn_employment_loss==1, 1, ifelse(wmn_employment_loss==0,0,NA))]

#subet to final clean version
gbv[is.na(location_id), location_id:=102] #manually recode united states (only missing location id)
gbv_clean <- gbv[, .SD, .SDcols=c(dem.vars, 'wmn_married', 'community_unsafe_binary', 'hh_unsafe_binary', 'hh_less_safe_since_covid', 'safe_place_avail', 'wmn_safe_place_need', 'safe_place_more_often_since_covid',
                                  'safe_place_no_access', 'safe_place_access_barrier', 'safe_place_access_barrier_covidrelated', 'sought_services_since_covid',
                                  'services_no_access', 'services_access_barrier', 'services_access_barrier_covidrelated', 'partner_alc', 'partner_alc_more_since_covid',
                                  'community_gbv_change', 'community_gbv_common', 'lost_income_since_covid', 'data_collection', 'sought_or_wanted_gbvservices')]

#write files
write.csv(gbv_clean, paste0(out.dir, 'gbv_clean.csv'))
write.csv(gbv_clean[data_collection=='2021'], paste0(out.dir, 'gbv_2021_clean.csv'))
