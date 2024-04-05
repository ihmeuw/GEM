## HEADER #################################################################
# Date: 9/8/2023
# Purpose: Pulling results and aggregating
#          

## SET-UP #################################################################
rm(list=ls())

# Load packages, and install if missing ========================================================================
packages <- c("data.table","tidyverse","ggplot2", "haven", "parallel", "gridExtra", "stringr")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

# Read in shared functions =================================================
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

# Set parameters ===========================================================
gbd.version <- 7    
gbd.step <- "iterative"
release <- 9
compare_version <- 7987
daly_version <- 76
yld_version <- 1471
yll_version <- 363

ages <- c(1099, 1024, 2549, 5069, 7099)
years <- seq(1990, 2021, 1)
metric <- 3 # Rates
measures <- c(2, 3, 4) 
cause_level <- 3
save_data <- T # Set to true if you want to save a new version of the data. False if you just want to load and examine the data in R.

# Set filepaths to save files ==============================================
data_fp <- "FILEPATH"
out_fp <- "FILEPATH"

## SCRIPT ##################################################################
#Getting locations 
locations <- get_location_metadata(location_set_id = 35, release_id = release) 
locs <- c(1, paste0(unique(locations[level == 1]$location_id)))

# Getting list of GBD causes
causes <- get_cause_metadata(cause_set_id = 3, release_id = release)

# CAUSES
list_causes <-c(unique(causes[level == cause_level]$cause_id))

# Age groupings
full_dataset <- data.table("location_id" = NA, "year_id" = NA, "metric_id" = NA, "sex" = NA, "sex_id" = NA, "cause_id" = NA)
for(a in ages){
  if(a == 1099){
    child_ids <- c(7:20, 30:32, 235)
    age_name <- "10+ years old (standardized)"
  } else if(a == 1024){
    child_ids <- c(7:9)
    age_name <- "10-24 years old"
  } else if(a == 2549){
    child_ids <- c(10:14)
    age_name <- "25-49 years old"
  } else if(a == 5069){
    child_ids <- c(15:18)
    age_name <- "50-69 years old"
  } else if(a == 7099){
    child_ids <- c(19, 20, 30:32, 235)
    age_name <- "70+ years old"
  } else {
    stop("You haven't specified this age group!")
  }
  
  allmeasures <- data.table("location_id" = NA, "year_id" = NA, "metric_id" = NA, "sex" = NA, "sex_id" = NA, "cause_id" = NA, "age_group_name" = NA)
  
  for(i in measures){
    if(i == 2){
      measure_name <- "DALYs"
      machinery <- "dalynator"
      version <- daly_version
    } else if(i == 3){
      measure_name <- "YLDs"
      machinery <- "como"
      version <- yld_version
    } else if(i == 4){
      measure_name <- "YLLs"
      machinery <- "codcorrect"
      version <- yll_version
    } else {
      stop("You haven't specified this measure!")
    }
    print(measure_name)
    if(a == 1099){ # Age standardizing 10+ age group
      temp <- make_aggregates(source = machinery, 
                              location_id = locs, 
                              year_id = years, 
                              sex_id = c(1,2,3), 
                              age_group_id = a,
                              cause_id = list_causes, 
                              measure_id = i, 
                              metric_id = metric, 
                              age_standardized_child_ids = child_ids, 
                              release_id = release, 
                              version_id = version)
    } else {
      temp <- make_aggregates(source = machinery, 
                              location_id = locs, 
                              year_id = years, 
                              sex_id = c(1,2,3), 
                              age_group_id = a,
                              cause_id = list_causes, 
                              measure_id = i, 
                              metric_id = metric, 
                              age_group_child_ids = child_ids, 
                              release_id = release, 
                              version_id = version)
    }
    
    setnames(temp, c("lower", "upper", "mean"), c(paste0(measure_name, "_lower"), paste0(measure_name, "_upper"), paste0(measure_name, "_mean")))
    temp[, `:=` (median = NULL, age_group_name = age_name, age_group_id = NULL, measure_id = NULL, sex = ifelse(sex_id == 1, "male", ifelse(sex_id == 2, "female", "both")))]
    
    allmeasures <- merge(allmeasures, temp, all = T, by = c("location_id", "year_id", "metric_id", "sex", "sex_id", "cause_id", "age_group_name"))
  }
  
  full_dataset <- rbindlist(list(full_dataset, allmeasures), fill = T)
}

full_dataset <- full_dataset[!is.na(location_id)]
long_dataset <- melt(full_dataset, id.vars = c("location_id", "year_id", "metric_id", "sex", "sex_id", "cause_id", "age_group_name"), 
                     measure.vars = c("DALYs_lower", "DALYs_upper", "DALYs_mean", "YLDs_lower", "YLDs_upper", "YLDs_mean", "YLLs_upper", "YLLs_lower", "YLLs_mean"), 
                     variable.name = "measure")
long_dataset <- long_dataset %>%
  separate(measure, c("measure", "variable"), "_")

long_dataset <- dcast.data.table(long_dataset, location_id + year_id + metric_id + sex + sex_id + cause_id + age_group_name + measure ~ variable, value.var = "value")

# Turn rates into rate per 100,000
long_dataset[metric_id == 3, `:=` (mean = mean*100000, lower = lower*100000, upper = upper*100000)]
full_dataset[metric_id == 3, `:=` (DALYs_mean = DALYs_mean*100000, DALYs_lower = DALYs_lower*100000, DALYs_upper = DALYs_upper*100000, 
                                   YLDs_mean = YLDs_mean*100000, YLDs_lower = YLDs_lower*100000, YLDs_upper = YLDs_upper*100000, 
                                   YLLs_mean = YLLs_mean*100000, YLLs_lower = YLLs_lower*100000, YLLs_upper = YLLs_upper*100000)]

# Adding in human-friendly name columns
long_dataset <- merge(long_dataset, causes[level == cause_level, .(cause_name, acause, cause_id, level)], by = c("cause_id"), all.x = T)
long_dataset$level <- NULL
long_dataset <- merge(long_dataset, locations[level == 1 | level == 0, .(location_id, location_name, level)], by = c("location_id"), all.x = T)
long_dataset$level <- NULL

full_dataset <- merge(full_dataset, causes[level == cause_level, .(cause_name, acause, cause_id, level)], by = c("cause_id"), all.x = T)
full_dataset$level <- NULL
full_dataset <- merge(full_dataset, locations[level == 1 | level == 0, .(location_id, location_name, level)], by = c("location_id"), all.x = T)
full_dataset$level <- NULL

# Get top causes of burden, globally for both sexes in 2021
top_causes <- long_dataset[sex_id == 3 & year_id == 2021 & location_id == 1 & measure == "DALYs" & age_group_name == "10+ years old (standardized)"] %>% arrange(-mean)
top_causes <- top_causes[1:20]

### SAVE DATASET
if(save_data){
  fwrite(long_dataset, paste0(data_fp, "long_all_burden_estimates_version_", compare_version, ".csv"))
  fwrite(full_dataset, paste0(data_fp, "wide_all_burden_estimates_version_", compare_version, ".csv"))
  fwrite(top_causes, paste0(data_fp, "long_top20_burden_estimates_version_", compare_version, ".csv"))
}














