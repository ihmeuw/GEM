####################################################################################################################################################################
# 
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Apply underreporting adjustment
#
####################################################################################################################################################################

rm(list=ls())

#manual inputs / set up
rid <- "RELEASE-ID"

#definition xwalk date and version
def_xw_date <- "MODEL-DATE"
def_xw_version <- "MODEL-NAME"

#underreport xwalk date and version
ur_xwalk_date <- 'MODEL-DATE'
ur_xwalk_version <- 'MODEL-NAME'

#general id
model_sex <- 'both_sex_csa'
bv <- "ID"
  
#root filepath
root <- 'FILEPATH'


### 0. SET UP #####################################################################################################################################################

#libraries + central functions
pacman::p_load(data.table, dplyr, openxlsx, stringr, data.table, ggplot2, tidyr, gridExtra, broom, magrittr, parallel, reticulate, DescTools)
library(dummies, lib.loc = paste0('FILEPATH', Sys.info()['user'], 'FILEPATH'))
invisible(sapply(list.files("FILEPATH", full.names = T), source))

#location + age metadata
locs <- get_location_metadata(22, release_id = rid)
ages <- get_age_metadata(19)
ages[, age_group_years_end := age_group_years_end-1]
setnames(ages, c('age_group_years_start', 'age_group_years_end'), c('age_start', 'age_end'))
age21 <- data.table(age_group_id = 21, age_start = 80, age_end = 99)
ages <- rbind(ages, age21, fill = T)
ages[age_group_id == 235, age_end := 99]

#for ui
z <- qnorm(0.975)



### 1. BEGIN ANALYSIS #####################################################################################################################################################

#get crosswalked data; confirm sex info
message('both sex')
data <- as.data.table(read.xlsx(paste0('FILEPATH', def_xw_date, '/both_sex_csa_bv', bv, '_xwalkadj_', def_xw_version, '.xlsx')))
data[sex == 'Female', sex_id := 2]
data[sex == 'Male', sex_id := 1]


#only keep model data
data <- data[is.na(group_review) | group_review==1]



### 2. ADJUST ALTNERATE DEFINITIONS #######################################################################################################################################

#read in underreporting data
ur_data <- fread("FILEPATH")

#read in underreporting xwalk model [it is the same for both sexes; stored in gbd folder for now]
csa_network <- readRDS(paste0('FILEPATH', ur_xwalk_date, '/', ur_xwalk_version, ".RDS"))

#split into data used for modeling that needs alternate definition adjustments (ie, anything not in the underreporting analysis)
#note: we assume that all the other data use some form of interview
data_to_adjust <- data[!nid %in% ur_data$nid] 
data_unadj <- data[nid %in% ur_data$nid]

#create data id for merge:
data_to_adjust[, data_id:=1:.N]

#rename mean
setnames(data_to_adjust, "mean", "mean_adj_without_underreporting")

#predict based on the starting value: apply underreporting adjustment to crosswalked values
# <= 25% -- full prediction, intercept and slope
# > 25% -- predict with slope only
data_to_adjust[mean_adj_without_underreporting < 0.25, mean := mean_adj_without_underreporting*csa_network$coefficients["alt_mean"] + csa_network$coefficients["(Intercept)"]] #note: could use predict() function, but keeping it consistent
data_to_adjust[mean_adj_without_underreporting >= 0.25, mean := mean_adj_without_underreporting*csa_network$coefficients["alt_mean"]]

#predict based on the starting value: apply underreporting adjustment to NOT crosswalked values [just as a check]
data_to_adjust[mean_unadj < 0.25, mean_only_underreporting := mean_unadj*csa_network$coefficients["alt_mean"] + csa_network$coefficients["(Intercept)"]] #note: could use predict() function, but keeping it consistent
data_to_adjust[mean_unadj >= 0.25, mean_only_underreporting := mean_unadj*csa_network$coefficients["alt_mean"]]

#add note to adjusted points
data_to_adjust[!is.na(note_modeler), note_modeler:=paste0(note_modeler, ' | applied xwalk for underreporting')]
data_to_adjust[is.na(note_modeler), note_modeler:=paste0('applied xwalk for underreporting')]

#set crosswalk parent seq and seq
data_to_adjust[, crosswalk_parent_seq := seq]

#re-calcualte uncetainty around the new mean
data_to_adjust[, c('upper', 'lower', 'variance') := NA]
data_to_adjust[, variance := standard_error^2]
data_to_adjust[, `:=` (lower =  mean - z*standard_error, upper = mean + z*standard_error)]
data_to_adjust[lower < 0, lower := 0]
data_to_adjust[upper > 1, upper := 1]

#re-calculate variance
data_to_adjust[, variance:=as.numeric(standard_error)^2]

#bind back to data with unadj data
data_unadj[, file_path := ''] #resulting in 'illegal xlm characters'
data_to_save <- rbind(data_to_adjust, data_unadj, fill=T)

#confirm sex info
data_to_save[sex == "Male", sex_id := 1]
data_to_save[sex == "Female", sex_id := 2]



### 3. SAVE ###############################################################################################################################################################

#create dir + save file
dir.create(paste0(root, 'FILEPATH', def_xw_date, '/'), recursive = T)
write.xlsx(data_to_save, paste0(root, 'FILEPATH', def_xw_date, '/', model_sex, '_bv', bv, '_xwalkadj_', def_xw_version, '_and_', ur_xwalk_version,'.xlsx'), sheetName='extraction')



