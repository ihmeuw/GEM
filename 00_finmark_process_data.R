#############################################################################################################
#
# Author: USERNAME
# Purpose: Load FinMark Trust Survey modules and create standard variables
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
locs <- get_location_metadata(location_set_id=35, gbd_round_id=7)

#create one large file
read_draw <- function(c.file){
  print(c.file)
  if (c.file %like% '.XLSX'){
    as.data.table(read.xlsx(c.file)) -> dat
  } else if (c.file %like% '.DTA') {
    as.data.table(read.dta13(c.file, convert.factors=TRUE, nonint.factors=TRUE)) -> dat
  } else {
    fread(c.file) -> dat
  }
  names(dat) <- tolower(names(dat))
  dat[, ihme_loc_id:=str_sub(c.file, 46, 48)]
  return(dat)
}

#set dirs, read in and clean files --------------------------------------------------------------------------
in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'

#list all finmark modules from directory, subset to raw csvs/xlsxs
file.list <- list.files(in.dir, recursive=T)
file.list <- file.list[file.list %like% '.CSV' | file.list %like% '.XLSX']

#two modules need dta's instead, replace in list
file.list <- file.list[!file.list %in% c('ZAF/ZAF_FINMARK_COVID_19_TRACKING_SURVEY_2020_WAVE_1_Y2021M01D07.XLSX',
                                         'NGA/NGA_FINMARK_COVID_19_TRACKING_SURVEY_2020_WAVE_1_Y2021M01D07.XLSX')]
file.list <- c(file.list, 'ZAF/ZAF_FINMARK_COVID_19_TRACKING_SURVEY_2020_WAVE_1_Y2021M01D07.DTA', 'NGA/NGA_FINMARK_COVID_19_TRACKING_SURVEY_2020_WAVE_1_Y2021M01D07.DTA')

#read in, concatenate files in list
paste0(in.dir, '/', file.list) %>% lapply(read_draw) %>% rbindlist(fill=T) -> all

#set standard variables
all[, wave:=data_collection_period]
all[is.na(wave), wave:=update_date] #for remaining surveys, wave is coded in update_date variable
all[, Gender:=ifelse(sex=='WOMAN', 'Female', 'Male')] #all original answers are either MAN or WOMAN
all[, AgeGroup:=ps5_age_group] 
all[, weight_ind:=str_replace(weight_ind, ',', '.')] #some weight values use commas for decimal notation, replace here
all[, pweight:=as.numeric(weight_ind)]
all <- all[!is.na(pweight)] #remove observations without weight 

write.csv(all, 'FILEPATH')
