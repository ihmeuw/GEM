#############################################################################################################
#
# Author: USERNAME
# Purpose: Harmonize and analyze UN women RGA for secondary impact indicators
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
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

#set dirs, read in and clean files --------------------------------------------------------------------------------------------------------------------------------------------------------
in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'

#data is organized into regional data files
wc_africa <- fread(paste0(in.dir, 'RGA_WEST_CENTRAL_AFRICA_WCARO_2020_Y2021M08D13.CSV'))[, regional_file:='west_central_africa']
eur_asia <- fread(paste0(in.dir, 'RGA_EUROPE_CENTRAL_ASIA_ECARO_2020_Y2021M08D09.CSV'))[, regional_file:='europe_central_asia']
es_africa <- fread(paste0(in.dir, 'RGA_EAST_SOUTHERN_AFRICA_ESARO_2020_Y2021M08D13.CSV'))[, regional_file:='east_southern_africa']
arab <- fread(paste0(in.dir, 'RGA_ARAB_STATES_ROAS_2020_Y2021M08D09.CSV'))[, regional_file:='arab_states']
lac <- fread(paste0(in.dir, 'RGA_AMERICAS_CARIBBEAN_LACRO_2020_Y2021M08D13.CSV'))[, regional_file:='latin_america_caribbean']

# (1) Create global file, clean up and prep demographic variables to be used in regression -------------------------------------------------------------------------------------------------
global <- rbind(wc_africa, eur_asia, es_africa, arab, lac, fill=T)

#loc standardization
global[, location_name:=country]
global[location_name=='RCA', location_name:='Central African Republic']
global[location_name=='B&H', location_name:='Bosnia and Herzegovina']
global[location_name=="Cote D'Ivoire", location_name:="Côte d'Ivoire"]
global[location_name=='Moldova', location_name:='Republic of Moldova']
global <- merge(global, locs, by='location_name', all.x=T, allow.cartesian=T)

#drop kosovo and duplicated info for us state georgia
global <- global[location_name!='Kosovo']
global <- global[ihme_loc_id!='USA_533']

#merge on wc-africa urbanicity info from country-specific files (missing from regional combined file)
civ <- as.data.table(read.xlsx(paste0(in.dir, 'CIV/2020/CIV_RGA_2020_Y2021M05D27.XLSX')))[, .(IDI, Region)]
civ[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region == 3, 'Rural', NA))]
civ[, ihme_loc_id:='CIV']
setnames(civ, c('IDI', 'Region'), c('ID', 'urbanicity'))

gin <- as.data.table(read.dta13(paste0(in.dir, 'GIN/2020/GIN_RGA_2020_Y2021M05D27.DTA')))[, .(IDI, Region)]
gin[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region == 3, 'Rural', NA))]
gin[, ihme_loc_id:='GIN']
setnames(gin, c('IDI', 'Region'), c('ID', 'urbanicity'))

mli <- as.data.table(read.dta13(paste0(in.dir, 'MLI/2020/MLI_RGA_2020_Y2021M05D27.DTA')))[, .(IDI, Region)]
mli[, wc_africa_urban_rural:=ifelse(Region %in% c(1,2), 'Urban', ifelse(Region %in% c(3,4), 'Rural', NA))]
mli[, ihme_loc_id:='MLI']
setnames(mli, c('IDI', 'Region'), c('ID', 'urbanicity'))

sen <- as.data.table(read.dta13(paste0(in.dir, 'SEN/2020/SEN_RGA_2020_Y2021M05D27.DTA')))[, .(ID, Q4_Région)]
sen[, wc_africa_urban_rural:=ifelse(Q4_Région %in% c(1,2), 'Urban', ifelse(Q4_Région == 3, 'Rural', NA))]
setnames(sen, 'Q4_Région', 'urbanicity')
sen[, ihme_loc_id:='SEN']

#rbind
wc_africa_urb <- rbind(civ, gin, mli, sen, fill=T)[, .(ID, urbanicity, wc_africa_urban_rural, ihme_loc_id)]
wc_africa_urb[, ID:=as.integer(ID)]

#merge on urbanicity info and fill-in to main variable for west-central africa locs
global <- merge(global, wc_africa_urb, by=c('ID', 'ihme_loc_id'), all.x=T)
global[regional_file=='west_central_africa', urban_rural:=wc_africa_urban_rural]

#map urbanicity codes
if (4 %in% unique(global[regional_file=='west_central_africa']$urbanicity)){
  global[urbanicity==1, urban_bin:='Capital city']
  global[urbanicity==2, urban_bin:='Big city']
  global[urbanicity==3, urban_bin:='Small town']
  global[urbanicity==4, urban_bin:='Rural']
} else {
  global[urbanicity==1, urban_bin:='Capital city']
  global[urbanicity==2, urban_bin:='Other city']
  global[urbanicity==3, urban_bin:='Village/rural']
}

#gender standardization
global[, sex:=ifelse(sex=='Men', 'male', 'female')]
global[, female:=ifelse(sex=='female', 1, 0)]

#age standardization, first for main reg categs
global[, lessthan35:=ifelse(age<35, 1, 0)]
global[, age35to64:=ifelse(age<65 & age>=35, 1, 0)]
global[, age65plus:=ifelse(age>=65, 1, 0)]

#reproductive, broader categories (for srhc)
global[, age25less:=ifelse(age<25, 1, 0)]
global[, age25to45:=ifelse(age>=25 & age<45, 1, 0)]
global[, age45more:=ifelse(age>=45, 1, 0)]

#more granular categories for appendix regs
global[age<25, age_bin:='18-24']
global[age<35 & age>24, age_bin:='25-34']
global[age<45 & age>34, age_bin:='35-44']
global[age<55 & age>44, age_bin:='45-54']
global[age<65 & age>54, age_bin:='55-64']
global[age>64, age_bin:='65+']
setnames(global, c('age', 'age_bin'), c('age_single_yr', 'age'))

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

## safety at home
global[, unsafe_at_home:=ifelse(safe=='No', 1, ifelse(safe=='Yes', 0, NA))]

## perception of gbv increases
global[regional_file %in% c('arab_states', 'east_southern_africa'), community_gbv_change:=ifelse(increase_gbv==1, 1, ifelse(increase_gbv==0, 0, NA))]
global[!regional_file %in% c('arab_states', 'east_southern_africa'), community_gbv_change:=ifelse(violence=='Yes', 1, ifelse(violence=='No', 0, NA))]

## health products access (NEW)
global[regional_file %in% c('europe_central_asia', 'latin_america_caribbean'), health_products_covid:=ifelse(access_product=='yes', 1, ifelse(access_product=='No', 0, NA))]

## general healthcare access 
global[regional_file %in% c('east_southern_africa', 'west_central_africa'), healthcare_covid:=ifelse(access_care=='yes', 1, ifelse(access_care=='No', 0, NA))]

## sexual and reproductive healthcare access
global[regional_file %in% c('east_southern_africa'), srhc_covid_among_need:=ifelse(HC_unable_access_1=='Yes', 1, ifelse(HC_unable_access_1=='No', 0, NA))]
global[regional_file %in% c('east_southern_africa'), srhc_covid_among_need:=ifelse(access_healthcare=='Yes, we tried; able to access', 0, srhc_covid_among_need)]

global[regional_file %in% c('west_central_africa'), srhc_covid_among_need:=ifelse(access_product=='yes', 1, ifelse(access_product=='No', 0, NA))]

# (3) Clean up and save final file ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

final <- global[, .(location_name, ihme_loc_id, super_region_name, regional_file, weight, rural, higher_edu, edu_dummy,
                    age_single_yr, age, sex, female, age25less, age25to45, age45more, lessthan35, age35to64, age65plus,
                    unsafe_at_home, community_gbv_change, health_products_covid, healthcare_covid, srhc_covid_among_need, urban_bin)]
final[, source:='UN Women RGA']

write.csv(final, paste0(out.dir, 'rga_regional_cleaned.csv'), row.names=F)

# (4) Create final file with uganda and caf country-level cleaning included ------------------------------------------------------------------------------------------------------------------------------------------------

regional <- final[!ihme_loc_id %in% c('CAF', 'UGA', 'CHL')]
country <- fread(paste0(out.dir, 'rga_country_level_cleaned.csv'))[, source:='UN Women RGA']

together <- rbind(regional, country, fill=T)
write.csv(together, paste0(out.dir, 'rga_all_data_cleaned.csv'), row.names=F)

