##########################################################################################################################################################################################
# 
# Purpose: Read in Facebook microdata and re-calculate fully vaccinated by country-sex
#
##########################################################################################################################################################################################

#rm(list=ls())

#create directory to save cleaned data
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

##### 0. SET UP ##########################################################################################################################################################################

#libraries
pacman::p_load(data.table, readstata13, readr, dplyr, magrittr, stringr, openxlsx, httr, jsonlite, ggplot2, gridExtra)

#location metadata
source(file.path("/FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)

#global and us file paths
fp <- paste0("/FILEPATH/")
fp_us <- paste0("/FILEPATH/")

##### 1. GET AND SUBSET DATA #############################################################################################################################################################

#read and subset global data
md <- fread(fp)
md <- md[, c("ISO_3", "sex", "age", "date", "weight", "V1", "V2", "V2a")]
md <- md[sex %in% c('Male', 'Female') & age != '']

#read and subset us data
md_us <- fread(fp_us)
md_us <- md_us[, c("sex", "age", "date", "weight", "V1", "V2", "V2a")]
md_us <- md_us[sex %in% c('Male', 'Female') & age != '']
md_us$ISO_3 <- 'USA'

#bind
md <- rbind(md, md_us)

##### 2. FULL VAX BY COUNTRY-SEX #########################################################################################################################################################

#copy data
dt <- copy(md)

#if valid answer to vaccinated, include in denominator
dt[V1 %in% c('Yes', 'No', "Don't know"), fullvax_denom := 1]

#if missing answer to vaccinated, do not include in denominator
dt[V1 %in% c('', 'SKIP', "MISSING") | V2 %in% c('', "MISSING"), fullvax_denom := 0]

#if 2-doses, then fully vaccinated; else not
dt[V2 %like% '2', fullvax := 1]
dt[!V2 %like% '2', fullvax := 0]

#if someone knows they recieved all required doses, then count as fully vaccinated
dt[!V2 %like% 'know' & V2a %like% 'Yes received all required doses', fullvax := 1]

#exclude the missing answers
dt <- dt[fullvax_denom != 0]

#calculate weighted values
dt[, fullvax_weighted := fullvax*weight]
dt[, fullvax_denom_weighted := fullvax_denom*weight]


##### 3. SUM VALUES ######################################################################################################################################################################

#sum original sample
den <- as.data.table(dt)[, sum(fullvax_denom), by = .(ISO_3, sex, date)] %>%
  setnames('V1','obs_nm')

#sum weighted number of fully vaccinated people
w_num <- as.data.table(dt)[, sum(fullvax_weighted), by = .(ISO_3, sex, date)] %>%
  setnames('V1','num')

#sum weighted sample
w_den <- as.data.table(dt)[, sum(fullvax_denom_weighted), by = .(ISO_3, sex, date)] %>%
  setnames('V1','denom_nm')

#merge
fb_2dose <- merge(w_num, w_den, by = c('ISO_3', 'sex', 'date')) %>%
  merge(den, by = c('ISO_3', 'sex', 'date'))

#exclude small samples
fb_2dose <- fb_2dose[obs_nm >= 30]

#calculate prop fully vaccinated
fb_2dose[, full_vax := num/denom_nm]
fb_2dose[, data_source := 'facebook']

#rename
fb_fullvax_cs <- copy(fb_2dose)
setnames(fb_fullvax_cs, 'obs_nm', 'sample')

#remove extra
rm(md_us)
rm(den)
rm(w_num)
rm(w_den)
rm(fb_2dose)
rm(dt)
rm(md)

#save
write.csv(fb_fullvax_cs, paste0(output_dir, 'fb_fullvax_cs.csv'), row.names = FALSE)