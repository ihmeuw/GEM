#############################################################################################################
#
# Author: USERNAME
# Purpose: Read in country-specific files for UGA and CAF UN Women RGAs, as issues in these locs in the combined regional microdata 
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

#read in country modules confirmed to have GBV, HC data:
uga <- as.data.table(read.xlsx(paste0(in.dir, 'UGA/2020/UGA_RGA_2020_Y2021M05D27.XLSX'), sheet='Data'))
caf <- as.data.table(read.xlsx(paste0(in.dir, 'CAF/2020/CAF_RGA_2020_Y2021M05D19.XLSX'), sheet='Data'))

#uganda-specific mapping 
uga[, gender:=ifelse(AO1=='MALE', 'Male', 'Female')]
uga[, Age:=as.numeric(A02a)]
#10yr age bins
uga[Age<25, age_bin:='18-24']
uga[Age<35 & Age>24, age_bin:='25-34']
uga[Age<45 & Age>34, age_bin:='35-44']
uga[Age<55 & Age>44, age_bin:='45-54']
uga[Age<65 & Age>54, age_bin:='55-64']
uga[Age>64, age_bin:='65+']
uga[, age:=age_bin]
uga[, weight:=1]
uga[, urbanicity:=A03_6]
uga[, partner_status:=A02b]
uga[, education:=A04]

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

# HARMONIZE HC, SRHC, SAFETY INDICATORS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
dem.vars <- c('gender', 'Age', 'weight', 'urbanicity', 'education', 'partner_status')

#UGA
setnames(uga, c('GBV12', 'CD7_Healthcare'), c('community_gbv_change', 'healthcare_covid'))
uga[!is.na(community_gbv_change), community_gbv_change:=ifelse(community_gbv_change=="Yes- increased",1, ifelse(community_gbv_change %in% c('No- stayed the same', 'Yes- decreased'), 0, NA))]
uga[!healthcare_covid %in% c("DON'T KNOW", "REFUSED"), healthcare_covid:=ifelse(healthcare_covid=='YES', 1, 0)]
uga[healthcare_covid %in% c("DON'T KNOW", "REFUSED"), healthcare_covid:=NA]
uga <- uga[, .SD, .SDcols=c(dem.vars, 'community_gbv_change', 'healthcare_covid')]
uga[, ihme_loc_id:='UGA']

#CAF
setnames(caf, c('PersonalSafety', 'ReasonsForNoPersonalSafety', 'ViolenceAwareness', 'Violence_gender', 'AffectedHealthAccess'),
         c('hh_unsafe_binary', 'reasons_hh_unsafe', 'community_gbv_change', 'persons_affected_by_viol', 'healthcare_covid'))
caf[!is.na(hh_unsafe_binary), hh_unsafe_binary:=ifelse(hh_unsafe_binary=='NO',1,0)]
caf[!is.na(community_gbv_change), community_gbv_change:=ifelse(community_gbv_change=='YES',1, ifelse(community_gbv_change=='NO',0,NA))]
caf[!is.na(healthcare_covid), healthcare_covid:=ifelse(healthcare_covid=='YES',1,0)]
caf[!is.na(AffectedSelfCare), srhc_covid:=ifelse(AffectedSelfCare=='YES',1,0)]
caf[AffectedSelfCare!="DID NOT SEEK/NEED SUCH SERVICES", srhc_covid_among_need:=ifelse(AffectedSelfCare=='YES',1,0)]
caf <- caf[, .SD, .SDcols=c(dem.vars, 'hh_unsafe_binary', 'reasons_hh_unsafe', 'community_gbv_change', 'persons_affected_by_viol', 'healthcare_covid', 'srhc_covid', 'srhc_covid_among_need')]
caf[, ihme_loc_id:='CAF']

# CREATE REGRESSION VARS -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#data file metadata
uga[, regional_file:='east_southern_africa']
caf[, regional_file:='west_central_africa']

all <- rbind(uga, caf, fill=T)
all <- merge(all, locs, by='ihme_loc_id', all.x=T, allow.cartesian=T)

#gender standardization
setnames(all, 'gender', 'sex')
all[, sex:=ifelse(sex=='Male', 'male', 'female')]
all[, female:=ifelse(sex=='female', 1, 0)]

#Age standardization, first for main reg categs
all[, lessthan35:=ifelse(Age<35, 1, 0)]
all[, age35to64:=ifelse(Age<65 & Age>=35, 1, 0)]
all[, age65plus:=ifelse(Age>=65, 1, 0)]

#reproductive, broader categories (for srhc)
all[, age25less:=ifelse(Age<25, 1, 0)]
all[, age25to45:=ifelse(Age>=25 & Age<45, 1, 0)]
all[, age45more:=ifelse(Age>=45, 1, 0)]

#more granular categories for appendix regs
all[Age<25, age_bin:='18-24']
all[Age<35 & Age>24, age_bin:='25-34']
all[Age<45 & Age>34, age_bin:='35-44']
all[Age<55 & Age>44, age_bin:='45-54']
all[Age<65 & Age>54, age_bin:='55-64']
all[Age>64, age_bin:='65+']
setnames(all, c('Age', 'age_bin'), c('age_single_yr', 'age'))

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
                     'Completed University/College', 'Complete tertiary or higher', '2'), edu_dummy:='Tertiary or higher'] #FB GEAH, 2=more than secondary
all[education %in% c('No education', 'None', 'No formal education'), edu_dummy:='None']
all[education %in% c('Incomplete primary',
                     'Primary not complete', 'Some primary', 'Pre-primary / Grade R', 
                     'Religious', 'Pre-primary', 'Preschool', 'Some primary education'), edu_dummy:='None']
all[education %in% c('REFUSED', "DON'T KNOW", 'DK/Refused', 'NO ANSWER/DO NOT KNOW',
                     'NR/DK'), edu_dummy:=NA]

#make a higher education bin for main reg
all[edu_dummy %in% c('None', 'Primary', 'Secondary'), higher_edu:=0]
all[edu_dummy %in% c('Tertiary or higher'), higher_edu:=1]

#urbanicity standardization
all[, rural:=ifelse(urbanicity %in% c('Rural', "Village/Rural"), 1, 
                    ifelse(urbanicity %in% c("Small city/Small town", "Capital city/Big city", 'Urban'), 0, NA))]

all[, urban_bin:=urbanicity]

# SAVE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#write final file
write.csv(all, paste0(out.dir, 'rga_country_level_cleaned.csv'), row.names=F)
