#############################################################################################################
# Purpose: Analyze UN women RGA 
#############################################################################################################

rm(list=ls())

library(data.table)
library(tidyverse)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)

source('/FILEPATH/econ_functions.R')

# user arguments
run_date <- gsub('-', '_', Sys.Date())

invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

#set dirs, read in and clean files --------------------------------------------------------------------------------------------------------------------------------------------------------
in.dir <- '/FILEPATH/'

# (1) Read in data and variables--------------------------------------------------------------------

wc_africa <- fread(paste0(in.dir, 'DATASET.CSV'))[, regional_file:='west_central_africa']
eur_asia <- fread(paste0(in.dir, 'DATASET.CSV'))[, regional_file:='europe_central_asia']
es_africa <- fread(paste0(in.dir, 'DATASET.CSV'))[, regional_file:='east_southern_africa']
arab <- fread(paste0(in.dir, 'DATASET.CSV'))[, regional_file:='arab_states']
lac <- fread(paste0(in.dir, 'DATASET.CSV'))[, regional_file:='latin_america_caribbean']


# (3) Microdata: Create global dataset, clean up and prep demographic variables to be used in regression  -------------------------

global <- rbind(wc_africa, eur_asia, es_africa, arab, lac, fill=T)

#loc standardization
global[, location_name:=country]
global[location_name=='RCA', location_name:='Central African Republic']
global[location_name=='B&H', location_name:='Bosnia and Herzegovina']
global[location_name=="Cote D'Ivoire", location_name:="Côte d'Ivoire"]
global[location_name=='Moldova', location_name:='Republic of Moldova']
global <- merge(global, locs, by='location_name', all.x=T, allow.cartesian=T)

global$Country <- global$country

#drop kosovo and duplicated info for us state georgia
global <- global[location_name!='Kosovo']
global <- global[ihme_loc_id!='USA_533']

#merge on wc-africa urbanicity info from country-specific files (missing from regional combined file)
civ <- as.data.table(read.xlsx(paste0(in.dir, 'FILEPATH/DATASET.XLSX')))[, .(IDI, Region)]
setnames(civ, c('IDI'), c('ID'))
civ[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region == 3, 'Rural', NA))]
civ[, ihme_loc_id:='CIV']

gin <- as.data.table(read.dta13(paste0(in.dir, 'FILEPATH/DATASET.DTA')))[, .(IDI, Region)]
setnames(gin, c('IDI'), c('ID'))
gin[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region == 3, 'Rural', NA))]
gin[, ihme_loc_id:='GIN']

mli <- as.data.table(read.dta13(paste0(in.dir, 'FILEPATH/DATASET.DTA')))[, .(IDI, Region)]
setnames(mli, c('IDI'), c('ID'))
mli[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region %in% c(3,4), 'Rural', NA))]
mli[, ihme_loc_id:='MLI']

sen <- as.data.table(read.dta13(paste0(in.dir, 'FILEPATH/DATASET.DTA')))[, .(ID, Q4_Région)]
sen[, wc_africa_urban_rural:=ifelse(Q4_Région %in% c(1,2), 'Urban', ifelse(Q4_Région == 3, 'Rural', NA))]
sen[, ihme_loc_id:='SEN']

#rbind
wc_africa_urb <- rbind(civ, gin, mli, sen, fill=T)[, .(ID, wc_africa_urban_rural, ihme_loc_id)]
wc_africa_urb[, ID:=as.integer(ID)]

#merge on urbanicity info and fill-in to main variable for west-central africa locs
global <- merge(global, wc_africa_urb, by=c('ID', 'ihme_loc_id'), all.x=T)
global[regional_file=='west_central_africa', urban_rural:=wc_africa_urban_rural]

#gender standardization
global[, sex:=ifelse(sex=='Men', 'Male', 'Female')]
global[, female:=ifelse(sex=='Female', 1, 0)]

global$Gender <- global$sex

#age standardization, first for main reg categs
global[, lessthan35:=ifelse(age<35, 1, 0)]
global[, age35to64:=ifelse(age<65 & age>=35, 1, 0)]
global[, age65plus:=ifelse(age>=65, 1, 0)]

#more granular categories for appendix regs
global[age<25, age_bin:='18-24']
global[age<35 & age>24, age_bin:='25-34']
global[age<45 & age>34, age_bin:='35-44']
global[age<55 & age>44, age_bin:='45-54']
global[age<65 & age>54, age_bin:='55-64']
global[age>64, age_bin:='65+']
setnames(global, c('age', 'age_bin'), c('age_single_yr', 'age'))

global$AgeGroup <- global$age

#education standardization
global[, edu_dummy:=education]
global[edu_dummy=='', edu_dummy:=NA]
global[edu_dummy=='Tertiary', edu_dummy:='Tertiary or higher']
global[edu_dummy=='Technical', edu_dummy:='Secondary']
global[edu_dummy=='No Education', edu_dummy:='None']

#make a higher education bin for main reg
global[edu_dummy %in% c('None', 'Primary', 'Secondary'), higher_edu:=0]
global[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#urbanicity standardization
global[, rural:=ifelse(urban_rural=='Rural', 1, ifelse(urban_rural=='Urban', 0, NA))]

#set weight variable
global[, weight:=weights_sample]

# (2) Create indicators, where available --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Increase in at least one domestic activity
global <- global %>%
  mutate(chores_increase = case_when(
    hh_resp == 'yes' ~ 1, 
    hh_resp == 'No' ~ 0 
  ))

## Increase in at least three domestic activity
global <- global %>%
  mutate(chores_increase3 = case_when(
    hh_resp3 == 'yes' ~ 1, 
    hh_resp3 == 'No' ~ 0 
  ))

## Increase in at least one child care activity or one adult care activity
global <- global %>%
  mutate(care_increase = case_when(
    child_rep == 'yes' | adult_care == 'yes' ~ 1, 
    child_rep == 'No' & adult_care == 'No' ~ 0, 
    child_rep == 'No' | is.na(adult_care) ~ 0,
    adult_care == 'No' | is.na(child_rep) ~ 0,
  )) 

## Increase in at least three child care activity or three adult care activity
global <- global %>%
  mutate(care_increase3 = case_when(
    child_rep3 == 'yes' | adult_care3 == 'yes' ~ 1, 
    child_rep3 == 'No' & adult_care3 == 'yes' ~ 0, 
    child_rep3 == 'No' | is.na(adult_care3) ~ 0,
    adult_care3 == 'No' | is.na(child_rep3) ~ 0,
  )) 

## Income loss (denominator: people currently working)
global <- global %>%
  mutate(c19_inc_loss_bn = case_when(
    income_loss == 'yes' & lost_job == 'No' ~ 1,
    income_loss == 'No' & lost_job == 'No' ~ 0
))

# (3) Clean up and save final file ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
global <- data.table(global)
final <- global[, .(location_name, ihme_loc_id, super_region_name, regional_file, weight, rural, higher_edu, edu_dummy,
                    age_single_yr, age, sex, female, lessthan35, age35to64, age65plus, Gender, AgeGroup, Country,
                    chores_increase, chores_increase3, care_increase, care_increase3, c19_inc_loss_bn)]

write.csv(final,'/FILEPATH/DATASET.csv', row.names=F)

# (4) Create final file with caf country-level cleaning included ------------------------------------------------------------------------------------------------------------------------------------------------

regional <- final[!ihme_loc_id %in% c('CAF', 'UGA', 'CHL')]
country <- fread( '/FILEPATH/DATASET.csv')

together <- rbind(regional, country, fill=T)
un_women_econ <- as.data.frame(together)

un_women_econ <- un_women_econ[, c("location_name", "ihme_loc_id", "super_region_name", "regional_file", "weight", "rural", "higher_edu", "edu_dummy",
                    "age_single_yr", "sex", "female", "lessthan35", "age35to64", "age65plus", "Gender", "AgeGroup", "Country",
                    "chores_increase", "chores_increase3", "care_increase", "care_increase3", "c19_inc_loss_bn")]

write.csv(un_women_econ, '/FILEPATH/DATASET.csv', row.names=F)