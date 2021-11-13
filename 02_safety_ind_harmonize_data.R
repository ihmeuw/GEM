#############################################################################################################
#
# Author: USERNAME
# Purpose: Harmonize covariate values of safety indicator data across sources
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

vecSplit <- function(string, pattern, n) {
  sapply(strsplit(string, pattern), `[`, n)
}

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]
out.dir <- 'FILEPATH'
plot.dir <- 'FILEPATH'

# read in data sets, harmonize format and var naming -----------------------------------------------------------------------------------------------------------------

#1. Gender Equality at Home Survey; read in and initial format
geah <- fread('FILEPATH')[, V1:=NULL]
geah <- geah[, c('location_name', 'gender', 'weight', 'safety', 'urbanicity', 'education')]
geah[, unsafe_at_home:=ifelse(safety %in% 1:2, 1, ifelse(safety %in% 3:5, 0, NA))]
geah[, safety:=NULL]
setnames(geah, 'gender', 'sex')
geah[location_name=='Bolivia', location_name:='Bolivia (Plurinational State of)']
geah[location_name=='Vietnam', location_name:='Viet Nam']
geah[location_name=='Taiwan', location_name:='Taiwan (Province of China)']
geah[location_name=='Swaziland', location_name:='Eswatini']
geah[location_name=='Czech Republic', location_name:='Czechia']
geah[location_name=='Laos', location_name:="Lao People's Democratic Republic"]
geah[location_name=='Macedonia', location_name:='North Macedonia']
geah[location_name=='Moldova', location_name:='Republic of Moldova']
geah[location_name=='Russia', location_name:='Russian Federation']
geah <- merge(geah, locs[, c('location_name', 'location_id', 'super_region_name')], by='location_name', all.x=T, allow.cartesian = T)
geah <- geah[!(location_name=='Georgia' & super_region_name=='High-income')]
geah <- geah[!is.na(location_id)]
geah[, source:='FB GEAH']

#rename, subset to relevant columns
all <- copy(geah)
all <- all[, c('sex', 'age', 'weight', 'urbanicity', 'education', 'location_name', 'date', 'unsafe_at_home', 'source')]

# Harmonization of covariate values to be used in regression framework ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#create standard age bins
all <- all %>%
  mutate(age_bin = case_when(
    age %in% c('16-', '16-24', '16-25', '18-24', '25-34', '26-35') ~ "Less than 35", 
    age %in% c('35-44', '36-45', '45-54', '55-64') ~ "35 to 64", 
    age %in% c('65-74', '65+', '75+') ~ "More than 65"
  ))

all <- all %>% 
  mutate(
    lessthan35 = case_when(
      age_bin == "Less than 35" ~ 1,
      age_bin %in% c("35 to 64", "More than 65") ~0), 
    age35to64 = case_when(
      age_bin == "35 to 64" ~ 1,
      age_bin %in% c("Less than 35", "More than 65") ~0),
    age65plus = case_when(
      age_bin == "More than 65" ~ 1, 
      age_bin %in% c("Less than 35", "35 to 64") ~0)
  )

#education bins: primary/secondary/tertiary/none
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

#urbanicity: urban/rural
unique(all$urbanicity)
all[urbanicity %in% c('Urban [City or Town]', 'Urban',
                      'Urban area', 'Big city', 'Capital city/Big city', 
                      'Capital city', 'Other city', 'Big city', 'Suburban/Peri-urban',
                      'City', 'City center or metropolitan area'), urbanicity_dummy:='Urban']
all[urbanicity %in% c('Village/Rural', 'Small city/Small town', 'Village/rural',
                      'Rural', 'Small town', 'Rural [Village or Farm]', 'Rural area'), urbanicity_dummy:='Rural']
all[urbanicity %in% c("DON'T KNOW", "REFUSED"), urbanicity_dummy:=NA]
all[!is.na(urbanicity), rural:=ifelse(urbanicity_dummy=='Rural',1,0)]

#binary female indicator
all[, female:=ifelse(sex=='Female',1,ifelse(sex=='Male',0,NA))]

#save final file
write.csv(all, paste0(out.dir, 'safety_microdata_harmonized_for_regs.csv'))
