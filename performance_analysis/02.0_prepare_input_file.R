## HEADER #################################################################
# Date: 4/22/2025
# Purpose: Prepping data for the analysis
#          
# source("filepath/02.0_prepare_input_file.R", echo=T)
#
## SET-UP #################################################################
rm(list = ls())
library(data.table)
library(tidyverse)
source("filepath/get_location_metadata.R")

## PARAMETERS TO MANUALLY CHECK ##################################################################
release <- 16
measure <- 2 
country_subset <- "none" 
cause_subset <- "top_and_sexspecific" 
covariate_name <- "sdi"

## AUTOMATIC MANIPULATION OF PARAMETERS ##################################################################
# Filepaths
data_dir <- paste0("filepath")
output_dir <- paste0(data_dir, "performance_analysis/")
param_fp <- paste0(data_dir, "performance_analysis_parameters.csv")
data_dir <- paste0(data_dir, "summaries/")
output_name <- paste0(covariate_name)

# Getting the right parameters
if(country_subset != "none"){
  if("by_vr" %in% country_subset & "by_pop" %in% country_subset){
    subset_name <- "by_vrpop"
  } else if(country_subset == "by_vr"){
    subset_name <- "by_vr"
  } else if(country_subset == "by_pop"){
    subset_name <- "by_pop"
  } else {
    subset_name <- "all"
  }
  param_fp <- gsub(".csv", paste0("_", subset_name, ".csv"), param_fp)
  output_name <- paste0(output_name, "_", subset_name)
} 

if(cause_subset != "none"){
  param_fp <- gsub(".csv", paste0("_", cause_subset, ".csv"), param_fp)
  output_name <- paste0(output_name, "_", cause_subset)
} 

output_dir <- paste0(output_dir, output_name, "/")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

if(measure == 1 | measure == "1") {
    measure_name <- "deaths"
} else if(measure == 2 | measure == "2"){
    measure_name <- "dalys"
} else if(measure == 4 | measure == "4"){
    measure_name <- "ylls"
} else if(measure == 3 | measure == "3"){
    measure_name <- "ylds"
} else if(measure == "life_exp"){
    measure_name <- "life_exp"
} else {
    stop("Measure option not defined!")
}

if(measure == "life_exp"){
data_fp <- paste0(data_dir, "summary_", measure_name, ".csv")
param_fp <- gsub(".csv", paste0("_life_exp.csv"), param_fp)
} else {
data_fp <- paste0(data_dir, measure_name, "_rate")
}

if(country_subset != "none"){
data_fp <- paste0(data_fp, "_", subset_name, "/")
} else {
data_fp <- paste0(data_fp, "/")
}

covariate_fp <- paste0(data_dir, "summary_", covariate_name, ".csv")

## FORMATTING THE FILES ##################################################################
# Read in input data
if(measure_name == "life_exp"){
  df <- fread(data_fp)
} else {
  df <- list.files(path = data_fp, pattern = "summary_\\d+", full.names = T) %>% lapply(fread) %>% rbindlist()
}
message("input data read in")

# Read in predictive variable
predictor <- fread(covariate_fp)
message("predictive variable read in")

# Combine predictor and input data
setnames(predictor, c(paste0("mean_", covariate_name), paste0("lower_", covariate_name), paste0("upper_", covariate_name)), c("mean_predictor", "lower_predictor", "upper_predictor"), skip_absent = TRUE)
if(covariate_name != "sdi"){
  predictor[, `:=` (lower_predictor = mean_predictor, upper_predictor = mean_predictor)]
}
df <- merge(df, predictor, by = c("year_id", "location_id"))
if(measure_name != "life_exp"){
  df <- df[, .(year_id, location_id, location_name, age_group_id, age_group_name, cause_id, cause_name, sex_id, sex, measure_name, metric_name, val, upper, lower, mean_predictor, upper_predictor, lower_predictor)]
}

if(cause_subset != "none"){
  print("Subsetting causes!")
  parameters <- fread(param_fp)
  
  causes_of_interest <- unique(parameters$cause_name)
  df <- df[cause_name %in% causes_of_interest]
}

if(cause_subset == "sex-specific" | cause_subset == "top_and_sexspecific"){
  df <- df[sex == "Female" & age_group_id == 27]
}

fwrite(df, file = paste0(output_dir, "compiled_full_values_", output_name, ".csv"))

# Establish subsets if relevant
if(country_subset != "none"){
  print("Subsetting!")
  parameters <- fread(param_fp)
  
  df <- merge(df, unique(parameters[, .(location_id, age_group_id)]), by = c("location_id", "age_group_id"))
} else {
  locations <- get_location_metadata(location_set_id = 1, release_id = release) %>% .[level == 3, .(location_id, location_name, ihme_loc_id, region_name, super_region_name)]
  df <- merge(df, locations, by = c("location_id", "location_name"))
}

## SAVING THE FILES ##################################################################
for(i in unique(df$cause_name)){
    df_i <- df[cause_name == i]
    output_name <- paste0(measure_name, "_", tolower(gsub(" ", "_", i)), "_compiled.csv")
    
    fwrite(df_i, file = paste0(output_dir, output_name))
    message(paste0("Output file written: ", output_name))
    message(paste0("Number of rows: ", nrow(df_i)))
}
