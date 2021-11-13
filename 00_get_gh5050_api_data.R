##################################################################################################################################################################################
# 
# Purpose: Pull, format, and save API data from Global Health 50/50
#          Calculate the % of females/males who are vaccinated and the gender breakdown of those vaccinated
#          This pulls and saves the cleaned data, so you can either source this script or read in the files it makes
#
##################################################################################################################################################################################

#rm(list=ls())
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')


##### 0. Set up ##################################################################################################################################################################

#libraries
pacman::p_load(data.table, readstata13, readr, dplyr, magrittr, openxlsx, stringr, httr, jsonlite, ggplot2)

#central function
source("/FILEPATH/get_location_metadata.R")

#location data
hier <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9) %>% 
  select(location_id, location_name, region_name, super_region_name, path_to_top_parent, parent_id, level) %>%
  as.data.table()

##### 1. GET + FORMAT DATA #######################################################################################################################################################

gh5050_temp <- fread('/FILEPATH/')
  
setnames(gh5050_temp, 'age_begin', 'age_start')
setnames(gh5050_temp, 'country', 'location_name')
setnames(gh5050_temp, 'vac_males', 'vacsM')
setnames(gh5050_temp, 'vac_females', 'vacsF')
setnames(gh5050_temp, 'vac2_males', 'vacs2M')
setnames(gh5050_temp, 'vac2_females', 'vacs2F')
setnames(gh5050_temp, 'popM', 'populationin1000sM')
setnames(gh5050_temp, 'popF', 'populationin1000sF')
  
gh5050_temp[age_end >= 99, age_end := 99]
gh5050_temp[, age_group := paste0(age_start, '-', age_end)]
  
#limit to adult populations
gh5050_temp <- gh5050_temp[age_start >= 18]

##### 2. AGE-SEX CALCULATIONS #####################################################################################################################################################

#drop columns we don't need
gh5050_csa <- copy(gh5050_temp[, c('date', 'location_name', 'age_group', 'age_start', 'age_end', 'vacsF', 'vacsM', 'vacs2F', 'vacs2M')])

#make columns numeric
gh5050_csa[, c('age_start', 'age_end', 'vacsF', 'vacsM', 'vacs2F', 'vacs2M')] <- lapply(gh5050_csa[, c('age_start', 'age_end', 'vacsF', 'vacsM', 'vacs2F', 'vacs2M')], as.numeric)

#calculate total >=1 dose + fully vaccinated
gh5050_csa[, all_dose1 := vacsF + vacsM]
gh5050_csa[, all_dose2 := vacs2F + vacs2M]

#lengthen table
gh5050_csa <- melt(data = gh5050_csa,
                   id = c('location_name', 'date', 'age_group', 'age_start', 'age_end', 'all_dose1', 'all_dose2')) %>% 
  as.data.table()

#assign sex_id
gh5050_csa[variable == 'vacsF' | variable == 'vacs2F', sex_id := 2]
gh5050_csa[variable == 'vacsM' | variable == 'vacs2M', sex_id := 1]

#label measures
gh5050_csa[variable == 'vacsF' | variable == 'vacsM', casedef := 'dose1']
gh5050_csa[variable == 'vacs2F' | variable == 'vacs2M', casedef := 'dose2']
gh5050_csa$variable <- NULL

#widen table
gh5050_csa <- dcast(gh5050_csa, location_name + date + age_group + age_start + age_end + sex_id + all_dose1 + all_dose2 ~ casedef, value.var = "value") %>% 
  as.data.table()

#label age groups and sex
gh5050_csa[sex_id == 1, sex := 'Male']
gh5050_csa[sex_id == 2, sex := 'Female']

#calculate gender breakdown (of those vaccinated, x% are male/female)
gh5050_csa[, uptake1 := dose1/all_dose1]
gh5050_csa[, uptake2 := dose2/all_dose2]


#add location metadata
gh5050_csa <- merge(gh5050_csa, hier[,.(location_id, location_name)], by = 'location_name', all.x = TRUE)
if(nrow(filter(gh5050_csa, is.na(location_id)))>1){
  gh5050_csa <- gh5050_csa[!is.na(location_id)]
  print('Not all locations could be matched to a location_id. Check that all location names are in standard GBD format!')
}

#add population data (use gh5050 estimates)
population <- copy(gh5050_temp[, c('date', 'location_name', 'age_group', 'populationin1000sF', 'populationin1000sM')])
population <- melt(population, id.vars = c('date', 'location_name', 'age_group'))
setnames(population, 'variable', 'sex')
setnames(population, 'value', 'population')
population$population <- as.numeric(population$population)
population[, population := population*1000]
population[sex %like% 'F', sex := 'Female']
population[sex %like% 'M', sex := 'Male']

#merge population sizes
gh5050_csa <- merge(gh5050_csa, population, by = c('date', 'location_name', 'age_group', 'sex'))

#calculate uptake
gh5050_csa[, vaccinated := dose1/population]
gh5050_csa[, full_vax := dose2/population]

#remove population
rm(population)


##### 3. Calculations by sex ######################################################################################################################################################
gh5050_cs <-copy(gh5050_csa)
gh5050_cs$population <- NULL #remove population; will re-calculate
gh5050_cs[, c('uptake1', 'uptake2', 'sex_id', 'age_start', 'age_end', 'age_group', 'all_dose1', 'all_dose2')] <- NULL

#sum 1 and 2 dose uptake by location-sex
overall_dose1_cs <- as.data.table(gh5050_cs)[, sum(dose1), by = .(location_id, location_name, sex, date)]
setnames(overall_dose1_cs, 'V1', 'dose1')

overall_dose2_cs <- as.data.table(gh5050_cs)[, sum(dose2), by = .(location_id, location_name, sex, date)]
setnames(overall_dose2_cs, 'V1', 'dose2')

#sum 1 and 2 dose uptake by location
overall_dose1_c <- as.data.table(gh5050_cs)[, sum(dose1), by = .(location_id, location_name, date)]
setnames(overall_dose1_c, 'V1', 'all_dose1')

overall_dose2_c <- as.data.table(gh5050_cs)[, sum(dose2), by = .(location_id, location_name, date)]
setnames(overall_dose2_c, 'V1', 'all_dose2')

#merge summations
gh5050_cs <- merge(overall_dose1_cs, overall_dose2_cs, by = c('location_id', 'location_name', 'sex', 'date')) %>%
  merge(overall_dose1_c, by = c('location_id', 'location_name', 'date')) %>%
  merge(overall_dose2_c, by = c('location_id', 'location_name', 'date'))

#recalculate uptake
gh5050_cs[, uptake1 := dose1/all_dose1]
gh5050_cs[, uptake2 := dose2/all_dose2]

#get population sizes
gh5050_temp$populationin1000sF <- as.numeric(gh5050_temp$populationin1000sF)
gh5050_temp$populationin1000sM <- as.numeric(gh5050_temp$populationin1000sM)

populationF <- as.data.table(gh5050_temp)[, sum(populationin1000sF), by = .(location_name, date)] %>%
  setnames('V1', 'population')
populationF[, sex := 'Female']

populationM <- as.data.table(gh5050_temp)[, sum(populationin1000sM), by = .(location_name, date)] %>%
  setnames('V1', 'population') 
populationM[, sex := 'Male']

population <- rbind(populationF, populationM)
population <- population[population != '']
population[, population := population*1000]

#merge
gh5050_cs <- merge(gh5050_cs, population, by = c('location_name', 'date', 'sex'))

#calculate
gh5050_cs[, vaccinated := dose1/population]
gh5050_cs[, full_vax := dose2/population]

##### 4. Label + clean environment #################################################################################################################################################
gh5050_csa[, data_source := 'gh5050']
gh5050_cs[, data_source := 'gh5050']

rm(gh5050_temp)
rm(overall_dose1_c)
rm(overall_dose1_cs)
rm(overall_dose2_c)
rm(overall_dose2_cs)

rm(populationM)
rm(populationF)
rm(population)
rm(hier)

#save
write.csv(gh5050_csa, paste0(output_dir, 'gh5050_csa.csv'), row.names = FALSE)
write.csv(gh5050_cs, paste0(output_dir, 'gh5050_cs.csv'), row.names = FALSE)
