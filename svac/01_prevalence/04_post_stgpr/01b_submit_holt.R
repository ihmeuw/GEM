########################################################################################################################################################
#
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Launch script for fitting ARIMA forecast/backcast from STGPR draws
#
########################################################################################################################################################
rm(list=ls())

#set directory
root <- "FILEPATH"

#set year cutoff
yr_end <- 2023

#toggle model
female <- T

#set model version
version <- "MODEL-NAME"

#get run id
if (female==T){
  run_id <- "FEMALE-RUN-ID"
  model <- 'fem_csa'
} else {
  run_id <- "MALE-RUN-ID"
  model <- 'male_csa'
}


## 0. SET UP -------------------------------------------------------------------------------------------------------------------------------------------

#packages and central functions
pacman::p_load(readstata13, data.table, ggplot2, gridExtra, dplyr, mgcv, boot, splines)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/get_model_results.R")
source("FILEPATH/utility.r")
source("FILEPATH/public.R")
library(gamm4)
library('forecast')
library('tseries')
library(msm)

#location metadata
locs <- get_location_metadata(22, release_id = 16)

#initiate draws
vars <- paste0("draw_", seq(0, 999, 1))

#cluster proj
project <- "PROJECT-NAME"


## 1. INITIALIZE ARIMA DATA ----------------------------------------------------------------------------------------------------------------------------

#set arima_input file path
arima_input <- paste0(root, "/FILEPATH/", model, '_', run_id, "_arima_input.csv")

if (file.exists(arima_input) == F) {
  #read in draws from model run
  initial_model <- lapply(paste0("FILEPATH/", run_id, "/FILEPATH/",locs[level>=3,location_id],".csv"),fread) %>% rbindlist(use.names=T)
  
  #read in model data
  input_data <- get_input_data(run_id, data_stage_name = "original")
  
  #note: we run the stgpr model to yr_end + 1, so we need to truncate the results
  initial_model <- initial_model[year_id <= yr_end]
  input_data <- input_data[year_id <= yr_end]
  
  #subset to only non-outliered data
  input_data <- input_data[is_outlier == 0]
  input_data$has_data <- 1
  
  #check how many unique nids (need at least two observations for holt)
  input_data[, num_sources := length(unique(nid)), by=c('location_id')]
  input_data <- input_data[num_sources > 1]
  
  #identify which locations have data
  initial_model_with_data <- merge(initial_model,unique(input_data[, .(location_id,sex_id,age_group_id,year_id,has_data)]), by=c("location_id","sex_id","age_group_id","year_id"),all=T)
  initial_model_with_data[is.na(has_data), has_data := 0]
  
  #get min and max years for each location with data
  initial_model_with_data[,min_year := min(year_id), by=c("location_id","has_data")]
  initial_model_with_data[,max_year := max(year_id), by=c("location_id","has_data")]
  initial_model_with_data[has_data == 0, c("min_year","max_year") := list(NA,NA)]
  isolated_min_max <- unique(initial_model_with_data[, .(location_id,min_year,max_year)])
  isolated_min_max <- isolated_min_max[!is.na(min_year) & !is.na(max_year)]
  initial_model_with_data[, c("min_year","max_year") := NULL]
  initial_model_with_data <- merge(initial_model_with_data, isolated_min_max, by=c("location_id"), all=T)
  
  #prepare data for estimation
  #merge on the data again to see which locations have no data:
  initial_model_with_data[, training := ifelse(year_id >= min_year & year_id <= max_year, 1, 0)]
  initial_model_with_data_copy <- copy(initial_model_with_data)
  
  #try in logit space:
  initial_model_with_data <- initial_model_with_data[, (vars) := lapply(.SD, function(t) logit(t)), .SDcols=vars,by=c("location_id","year_id","sex_id","age_group_id")]
  
  #write out the dataset so we can pass into the qsub:
  write.csv(initial_model_with_data, arima_input)
  
}  


## 2. RUN ON THE CLUSTER -------------------------------------------------------------------------------------------------------------------------------

#set up args for the cluster
user <- Sys.getenv(x='USER')
script <- paste0(root, 'FILEPATH/01_holt.R')
shell  <- 'FILEPATH/execRscript.sh'
proj   <- 'PROJECT-NAME'
output_info <- paste0('FILEPATH') 
error_info <- paste0('FILEPATH')
time_alloc <- "TIME"
mem_alloc <- "MEMORY"
queue <- "QUEUE"
threads <- "THREADS"


#forecast/backcast each locations
for(l in unique(locs[level>=3]$location_id)) {
  
  #set job name
  job_name <- paste0(model, l)
  
  #sbatch
  command <- paste0("SBATCH-CLUSTER-INFORMATION")
  
  #launch job
  system(command)
  
  
}

