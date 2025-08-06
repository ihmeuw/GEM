## HEADER #################################################################
# Date: 6/26/2024
# Purpose: Pulling granular summaries
#          
# source("filepath/01.1_pulling_summaries.R", echo=T)

## SET-UP #################################################################
rm(list = ls())
library(data.table)
library(tidyverse)

source("filepath/get_outputs.R")

## SCRIPT ##################################################################
task_id <- Sys.getenv('address')
param_fp <- commandArgs(trailingOnly = T)[1]
measures <- commandArgs(trailingOnly = T)[2]
metrics <- commandArgs(trailingOnly = T)[3]
release <- commandArgs(trailingOnly = T)[4]
output_fp <- commandArgs(trailingOnly = T)[5]

parameters <- fread(param_fp)
print(task_id)
cause_of_interest <- unique(parameters[options == as.numeric(task_id)]$cause_id)

df <- get_outputs("cause", 
                  release_id = release, 
                  cause_id = cause_of_interest, 
                  year_id = unique(parameters[cause_id == cause_of_interest]$year_id), 
                  location_id = unique(parameters[cause_id == cause_of_interest]$location_id), 
                  age_group_id = unique(parameters[cause_id == cause_of_interest]$age_group_id), 
                  sex_id = unique(parameters[cause_id == cause_of_interest]$sex_id), 
                  measure_id = measures, 
                  compare_version_id = 8305,
                  metric_id = metrics)

fwrite(df, paste0(output_fp, "summary_", cause_of_interest, ".csv"))