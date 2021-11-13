#############################################################################################################
# Purpose: Read in country-specific files for UGA and CAF UN Women RGAs
#############################################################################################################


rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(stargazer)
invisible(sapply(list.files("/FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

run_date <- gsub('-', '_', Sys.Date())

#set dirs, read in and clean files --------------------------------------------------------------------------------------------------------------------------------------------------------
in.dir <- '/FILEPATH/'

#read in country modules confirmed to have econ data:
caf <- as.data.table(read.xlsx(paste0(in.dir, 'FILEPATH/DATASET.XLSX'), sheet='Data'))

#caf-specific mapping 
caf[Age<25, age_bin:='18-24']
caf[Age<35 & Age>24, age_bin:='25-34']
caf[Age<45 & Age>34, age_bin:='35-44']
caf[Age<55 & Age>44, age_bin:='45-54']
caf[Age<65 & Age>54, age_bin:='55-64']
caf[Age>64, age_bin:='65+']
caf[, age:=age_bin]
caf[, urbanicity:=UrbanRural]
caf[, gender:=Gender]
caf[, partner_status:=MaritalStatus]
caf[, education:=Education]
caf[, weight:=Weighting]

# HARMONIZE INDICATORS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

#CAF
caf[, ihme_loc_id:='CAF']
#Create econ and time use variables
caf <- caf %>% mutate(c19_inc_loss_bn = case_when(
  Incomechange == "Decrease of income" | Incomechange == "I have lost all my income " ~ 1, 
  Incomechange == "Increase of income" | Incomechange == "No changes" ~ 0))

for (var in c("FoodPreparation", "Cleaning", "FetchingWater", "HHShopping", "ElderlyCare")) {
  caf[[var]] <- factor(caf[[var]], levels = c("I don’t do it", "I didn’t do it before but now I’m spending time on it",
                                              "More time", "Same time as before", "Less time than before"), 
                       labels = c(1:5))
  
}

caf$YoungChildren <- factor(caf$YoungChildren, levels = c("I don’t do it", "I didn’t do it before but now I’m spending time on it",
                                                          "More time", "Same time as before", "Less time than before", "No children 0-4 years in household"), 
                            labels = c(1:6))

caf$OlderChildren <- factor(caf$OlderChildren, levels = c("I don’t do it", "I didn’t do it before but now I’m spending time on it",
                                                          "More time", "Same time as before", "Less time than before", "No children 5-17 in household"), 
                            labels = c(1:6))

  caf <- caf %>% mutate(c19_inc_loss_bn = case_when(
    Incomechange == "Decrease of income" | Incomechange == "I have lost all my income " ~ 1, 
    Incomechange == "Increase of income" | Incomechange == "No changes" ~ 0))

if ("FoodPreparation" %in% colnames(caf)) {
  for (v in c('FoodPreparation', 'Cleaning', 
              'FetchingWater', 'HHShopping', 'YoungChildren', 'OlderChildren', 'ElderlyCare')){
    
    caf[[v]] <- case_when( 
      caf[[v]] %in% c(2,3) ~ 1, 
      caf[[v]] == 5 ~ -1,
      caf[[v]] == 4 ~ 0
    )
  }
  
  caf$net_chores <- apply(caf[, c('FoodPreparation', 'Cleaning', 'FetchingWater', 'HHShopping')], 1, sum, na.rm = T) 
  caf$chores_increase <- if_else(caf$net_chores > 0, 1, 0)
  caf$net_care <- apply(caf[, c('YoungChildren', 'OlderChildren', 'ElderlyCare')], 1, sum, na.rm = T) 
  caf$care_increase <- if_else(caf$net_care > 0, 1, 0)
}

# CREATE REGRESSION VARS -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

caf <- data.table(caf)
#data file metadata
caf[, regional_file:='west_central_africa']

all <- caf
all <- merge(all, locs, by='ihme_loc_id', all.x=T, allow.cartesian=T)

#gender standardization
setnames(all, 'gender', 'sex')
all[, sex:=ifelse(sex=='Male', 'Male', 'Female')]
all[, female:=ifelse(sex=='Female', 1, 0)]

all$Gender <- all$sex

#Age standardization
all[, lessthan35:=ifelse(Age<35, 1, 0)]
all[, age35to64:=ifelse(Age<65 & Age>=35, 1, 0)]
all[, age65plus:=ifelse(Age>=65, 1, 0)]

all[Age<25, age_bin:='18-24']
all[Age<35 & Age>24, age_bin:='25-34']
all[Age<45 & Age>34, age_bin:='35-44']
all[Age<55 & Age>44, age_bin:='45-54']
all[Age<65 & Age>54, age_bin:='55-64']
all[Age>64, age_bin:='65+']
setnames(all, c('Age', 'age_bin'), c('age_single_yr', 'age'))

all$AgeGroup <- all$age

#education standardization
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
                     'Completed University/College', 'Complete tertiary or higher', '2'), edu_dummy:='Tertiary or higher'] 
all[education %in% c('No education', 'None', 'No formal education'), edu_dummy:='None']
all[education %in% c('Incomplete primary',
                     'Primary not complete', 'Some primary', 'Pre-primary / Grade R', 
                     'Religious', 'Pre-primary', 'Preschool', 'Some primary education'), edu_dummy:='None']
all[education %in% c('REFUSED', "DON'T KNOW", 'DK/Refused', 'NO ANSWER/DO NOT KNOW',
                     'NR/DK'), edu_dummy:=NA]

all[edu_dummy %in% c('None', 'Primary', 'Secondary'), higher_edu:=0]
all[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#urbanicity standardization
all[, rural:=ifelse(urbanicity %in% c('Rural', "Small city/Small town", "Village/Rural"), 1, 
                    ifelse(urbanicity %in% c("Capital city/Big city", 'Urban'), 0, NA))]

# SAVE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#write final file
write.csv(all,'/FILEPATH/DATASET.csv', row.names=F)