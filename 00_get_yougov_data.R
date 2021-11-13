####################################################################################################################################################################
# 
# Purpose: Read and collapse all YouGov files; save as one CSV file (will read in and edit microdata in individual scripts)
#
####################################################################################################################################################################

#rm(list=ls())

#create directory to save cleaned data
dir.create(paste0('/FILEPATH/', Sys.Date(), '/input_data/'), recursive = T, showWarnings = F)
output_dir_clean <- paste0('/FILEPATH/', Sys.Date(), '/input_data/')

#set file path to data
fp <- '/FILEPATH/'

##### 0. SET UP ####################################################################################################################################################################

#libraries
pacman::p_load(data.table, readstata13, readr, magrittr, stringr)

#function to loop through and merge files
read_draw <- function(c.file){
  print(c.file)
  dat <- fread(c.file)
  dat[, country := str_replace(c.file, fp, '')]
  dat[, country := str_replace(country, '.csv', '')]
  dat[, country := gsub('-', ' ', country)]
  dat[, country := str_to_title(country)]
  dat$V1 <- NULL
  return(dat)
}

##### 1. PULL + SAVE DATA ####################################################################################################################################################################

files <- list.files(fp)
files <- files[files %like% 'csv']
file.list <- paste0(fp, files)
file.list %>% lapply(read_draw) %>% rbindlist(fill = T) -> yougov_csa

#subset to variables of interest
yougov_csa <- yougov_csa[, c('qweek',
                             'endtime',
                             'gender', 
                             'age', 
                             'weight', 
                             'state',
                             'country', 
                             'i12_health_7', 
                             'v1', 
                             'v2_1', 
                             'v3', 
                             'v3_open',
                             'vac',
                             'vac_1',   
                             'vac_2', 
                             'vac5',
                             'vac_booster')]

yougov_csa[i12_health_7 == '', i12_health_7 := -1]
yougov_csa[v1 == '', v1 := -1]
yougov_csa[v2_1 == '', v2_1 := -1]
yougov_csa[v3 == '', v3 := -1]
yougov_csa[v3_open == '', v3_open := -1]
yougov_csa[vac_1 == '', vac_1 := -1]
yougov_csa[vac_2 == '', vac_2 := -1]
yougov_csa[vac5 == '', vac5 := -1]
yougov_csa[vac_booster == '', vac_booster := -1]

#save
write.csv(yougov_csa, paste0(output_dir_clean,  'cleaned_yougov.csv'), row.names = FALSE)