#############################################################################################################
#
# Author: USERNAME
# Purpose: Read in and harmonize data on perceptions of gbv changes in the community across sources
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

# 1. UN WOMEN RGA ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# available from males and females; covariates available in more detailed categories (to be used for appendix model)

#read in and initial format
rga_micro <- fread('FILEPATH')[, V1:=NULL]
rga <- rga_micro[, c('gender', 'age', 'weight', 'urbanicity', 'education', 'partner_status', 'ihme_loc_id', 'community_gbv_change')]
rga <- merge(rga, locs[, c('ihme_loc_id', 'location_name')], by='ihme_loc_id')
rga[, ihme_loc_id:=NULL]
rga[, source:='UN Women RGA']
all <- copy(rga)

#make simplest age bins to align with standard covariate values available from other sources
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

#create education bins by primary/secondar/tertiary/none
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

#make a simpler education bin: binary higher_edu or not 
all[edu_dummy %in% c('Secondary or less', 'None', 'Primary', 'Secondary'), higher_edu:=0]
all[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#map urbanicity into urban/rural categories
unique(all$urbanicity)
all[urbanicity %in% c('Urban [City or Town]', 'Urban',
                      'Urban area', 'Big city', 'Capital city/Big city', 
                      'Capital city', 'Other city', 'Big city', 'Suburban/Peri-urban',
                      'City', 'City center or metropolitan area'), urbanicity_dummy:='Urban']
all[urbanicity %in% c('Village/Rural', 'Small city/Small town', 'Village/rural',
                      'Rural', 'Small town', 'Rural [Village or Farm]', 'Rural area'), urbanicity_dummy:='Rural']
all[urbanicity %in% c("DON'T KNOW", "REFUSED"), urbanicity_dummy:=NA]

#map urbanicity into more detailed bins: capital city/non-capital city/rural
all[urbanicity %in% c('Big city', 'Capital city/Big city', 'Capital city', 'City center or metropolitan area', "Urban", "Urban [City or Town]"), urban_bin:='Capital city']
all[urbanicity %in% c('Small city/Small town', 'Other city', 'Small town', 'Suburban/Peri-urban'), urban_bin:='Non-capital city']
all[urbanicity %in% c('Village/Rural', 'Village/rural', 'Rural', "Rural [Village or Farm]"), urban_bin:='Rural']

#set rural as the referent
all[, urban_bin:=factor(urban_bin, levels=c('Rural', 'Non-capital city', 'Capital city'))]

#create binary rural indicator (1 if rural, 0 otherwise)
all[!is.na(urbanicity), rural:=ifelse(urbanicity=='Rural',1,0)]

#create partner status variable (partnered/not partnered)
all[partner_status %in% c('1', "Living with partner/Cohabiting", 'Married', 'Married (monogamous)', 'Married (polygamous)', 
                          'Married monogamous', 'Married or in union / monogamic', 'Married or in union/ polygamic',
                          'Married polygamous', 'Open Union/Monogamous', 'Open Union/Polygamy'), partnered:=1]
all[partner_status %in% c('0', 'Divorced', 'Divorced / Separated', "Divorced/separated", 'Married but separated', "Separated/divorced", 'Single',
                          'Widower', 'Widowed', 'Widow/widower', 'Single [never married]'), partnered:=0]

#create binary female covariate
all[, female:=ifelse(gender=='Female',1,ifelse(gender=='Male',0,NA))]

#ensure values are numeric
all[, community_gbv_change:=as.numeric(community_gbv_change)]

#save file with covariates cateogrized
write.csv(all, paste0(out.dir, 'un_women_males_females_community_gbv_change_harmonized_for_regs.csv'))

# 2. GOALKEEPERS 2021  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# available from females only; set up standard covariates for use with main paper regression framework

#read in and format
gk_micro <- fread(paste0('FILEPATH'))[, V1:=NULL]
gk <- gk_micro[, c('country', 'observation_id'):=NULL]
gk[age=='Not Available', age:=NA]
gk <- gk[gender=='Female']
gk <- merge(gk, locs[, c('location_id', 'location_name')], by='location_id')
setnames(gk, 'geography', 'urbanicity')
gk[, c('location_id', 'gender', 'religion', 'ethnicity', 'partner_alc', 'partner_alc_more_since_covid'):=NULL] #removing partner alc questions because only asked of partnered women which is only 538 respondents (30% of sample)

#melt
gk <- gk[, c('location_name', 'age', 'urbanicity','education', 'wmn_married', 'data_collection', 'community_gbv_change')]
setnames(gk, c('wmn_married'), c('partner_status'))
gk[, source:='Goalkeepers']
all <- copy(gk)

#create primary/secondary/tertiary/none education bins
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

#make simplest education bin: higher_edu or not
all[edu_dummy %in% c('Secondary or less', 'None', 'Primary', 'Secondary'), higher_edu:=0]
all[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#create binary rural indicator
all[urbanicity %in% c('Suburban/Peri-urban','City center or metropolitan area'), rural:=0]
all[urbanicity %in% c('Rural'), rural:=1]

#create partner status categories (partnered/not)
all[partner_status %in% c('1', "Living with partner/Cohabiting", 'Married', 'Married (monogamous)', 'Married (polygamous)', 
                          'Married monogamous', 'Married or in union / monogamic', 'Married or in union/ polygamic',
                          'Married polygamous', 'Open Union/Monogamous', 'Open Union/Polygamy'), partnered:=1]
all[partner_status %in% c('0', 'Divorced', 'Divorced / Separated', "Divorced/separated", 'Married but separated', "Separated/divorced", 'Single',
                          'Widower', 'Widowed', 'Widow/widower', 'Single [never married]'), partnered:=0]

#map age into standard bins to be comparable with other sources
all <- all %>%
  mutate(age_simple = case_when(
    age %in% c('16-', '16-24', '16-25', '18-24', '16 to 25 years old') ~ "Less than 25", 
    age %in% c('25-34', '26-35', '35-44', '36-45', '26 to 35 years old', '36 to 45 years old') ~ "25 to 45", 
    age %in% c('45-54', '46+', '55-64', '65-74', '65+', '75+', 'Over 45 years old') ~ "More than 45"
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

#write file
write.csv(all, paste0(out.dir, 'gk_community_gbv_change_harmonized_for_regs.csv'))

