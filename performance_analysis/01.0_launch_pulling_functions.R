## HEADER #################################################################
# Date: 6/26/2024
# Purpose: Pulling most granular results
#          
# source("filepath/01.0_launch_pulling_functions.R", echo=T)

## SET-UP #################################################################
rm(list = ls())
source("filepath/get_location_metadata.R")
source("filepath/get_cause_metadata.R")
source("filepath/get_population.R")
library(data.table)
library(tidyverse)

release <- 16

age_child_ids <- c(27) 
years <- seq(1990, 2023, 1)
sex_ids <- c(2)
loc_levels <- 3
cause_levels <- 3

# For pulling summaries
measures <- c(2)
metrics <- c(3)
country_subset <- "none" 
cause_subset <- "top_and_sexspecific"

# For pulling predictor
covariate_id <- 881
covariate_name <- "sdi"

# Filepaths
data_dir <- "filepath"
param_fp <- paste0(data_dir, "performance_analysis_parameters.csv")
output_fp <- paste0(data_dir, "summaries/")

# Setting the cluster characteristics
shell  <- "filepath/execRscript.sh"
proj   <- "proj_gem"
output_info <- "filepath"
error_info <- "filepath"

## SCRIPT ##################################################################
# Reading in all of the metadata
locations <- get_location_metadata(location_set_id = 1, release_id = release) %>% .[level <= loc_levels & ihme_loc_id != "", .(location_id, location_name, ihme_loc_id, region_name, super_region_name)]
causes <- get_cause_metadata(cause_set_id = 3, release_id = release) 

### CREATING THE PARAMETER FILE TO PULL GBD RESULTS ########################
# Subsetting causes to create the parameter file
if(cause_subset == "sex-specific"){
  causes <- causes[level == cause_levels & ((female == 1 & male == 0) | cause_name == "Breast cancer") & cause_name != "Gynecological diseases"]
} else if(cause_subset == "top_and_sexspecific"){
    causes <- causes[level == cause_levels & ((female == 1 & male == 0) | cause_name %in% c("Breast cancer", "Cardiovascular diseases", "All causes")) & cause_name != "Gynecological diseases"]
}
causes <- causes[, .(cause_id, cause_name, cause_outline)]
causes[, options := 1:.N]

# Creating the parameter file
sexes <- data.table(sex_id = sex_ids)
ages <- data.table(age_group_id = age_child_ids)
locations <- cross_join(locations, sexes)
locations <- cross_join(locations, ages)
subset_name <- "all"

years <- data.table(year_id = years)
parameters <- cross_join(causes, locations)
parameters <- cross_join(parameters, years)

if(country_subset != "none"){
  param_fp <- gsub(".csv", paste0("_", subset_name, ".csv"), param_fp)
} 
if(cause_subset != "none"){
  param_fp <- gsub(".csv", paste0("_", cause_subset, ".csv"), param_fp)
} 

fwrite(parameters, param_fp)

### PULLING GBD RESULTS ############################################################
script <- "filepath/01.1_pulling_summaries.R"
mem_alloc <- 50
threads <- 5
time_alloc <- "02:00:00"
queue <- "long.q"

n_jobs <- max(parameters$options)

for(measure_option in measures){
  for(metric_option in metrics){
    
    if(measure_option == 1) {
      measure_name <- "deaths"
    } else if(measure_option == 2){
      measure_name <- "dalys"
    } else if(measure_option == 4){
      measure_name <- "ylls"
    } else if(measure_option == 3){
      measure_name <- "ylds"
    } else {
      stop("Measure option not defined!")
    }

    if(metric_option == 2){
      metric_name <- "percent"
    } else if(metric_option == 3){
      metric_name <- "rate"
    } else if(metric_option == 1){
      metric_name <- "count"
    } else {
      stop("Metric option not defined!")
    }
    
    output_dir <- paste0(output_fp, measure_name, "_", metric_name)
    if(country_subset != "none"){
      output_dir <- paste0(output_dir, "_", subset_name)
    }
    
    output_dir <- paste0(output_dir, "/")
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    job_name <- paste0("pulling_", measure_name, "_", metric_name)
    
    print(job_name)
    
    other_variables <- paste(param_fp, measure_option, metric_option, release, output_dir) 
    
    ##Launches job
    command   <- paste0("sbatch --mem ", mem_alloc,
                        "G -C archive",  
                        " -c ", threads, 
                        " -t ", time_alloc,
                        " -p ", queue, 
                        " -a ", paste0("1-", n_jobs, "%50"),
                        " -e ", paste0(error_info, "/%x.e%j"),
                        " -o ", paste0(output_info, "/%x.o%j"),
                        " -A ", proj, 
                        " -J ", job_name,
                        " --parsable ",
                        " ", shell, 
                        " -s ", script, 
                        " ", other_variables)
    
    system(command)
  }
}

### PULLING COVARIATE ############################################################
script <- "filepath/01.2_pulling_predictor.R"
job_name <- "pulling_predictor"

mem_alloc <- 10
threads <- 4
time_alloc <- "00:30:00"
queue <- "long.q"

other_variables <- paste(release, covariate_id, covariate_name, output_fp) 

##Launches job
command   <- paste0("sbatch --mem ", mem_alloc,
                    "G -C archive", 
                    " -c", threads, 
                    " -t", time_alloc,
                    " -p", queue, 
                    " -e", paste0(error_info, "/%x.e%j"),  
                    " -o", paste0(output_info, "/%x.e%j"), 
                    " -A", proj, 
                    " -J", job_name,
                    " --parsable",
                    " ", shell, 
                    " -s", script, 
                    " ", other_variables)
    
  system(command)

