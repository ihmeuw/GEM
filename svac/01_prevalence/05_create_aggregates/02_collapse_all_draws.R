############################################################################################################
## 
## Cleaned Code for: Sexual violence against children before age 18
## Purpose: Collapse draws to get final estimates
## 
###########################################################################################################
rm(list=ls())

female <- T

## SET-UP -------------------------------------------------------------------------------------------------

# universal parameters
current_round <- 2023
gbd_release_id <- 16
v <- "MODEL-VERSION"

# get sex info
if (female) {
  #general
  risk <- 'fem_csa'
  sex_to_pull <- 2
  
  #for holt
  xwalk_id <- "FEMALE-ID"
  run_id <- "FEMALE-RUN-ID"
  
} else {
  #general
  risk <- 'male_csa'
  sex_to_pull <- 1
  
  #for holt
  xwalk_id <- "MALE-ID"
  run_id <- "MALE-RUN-ID"
  
}

# create input and output directories [read in final draws, save as custom aggregates]
in.dir <- paste0("FILEPATH/", risk, "/", run_id, "/", v,"/")
out.dir <- paste0("FILEPATH/", risk, "/", run_id, "/", v,"/")
dir.create(out.dir, recursive = T, showWarnings = F)

# libraries + shared functions
pacman::p_load(data.table, magrittr, ggplot2, haven, stringr, parallel, readxl, gridExtra, ggrepel, dplyr)
invisible(sapply(list.files("FILEPATH", full.names = T), source))
source('FILEPATH/utility.r')

# get location + age metadata
locs <- get_location_metadata(22, release_id = gbd_release_id) %>% .[,.(location_id,ihme_loc_id,location_name,level,region_name,region_id,super_region_id,super_region_name)]
ages <- get_age_metadata(24, release_id = gbd_release_id)
ages[, age_group := paste0(age_group_years_start, "-", age_group_years_end)]

# get population data
pops <- get_population(release_id = gbd_release_id,
                       location_id = locs$location_id,
                       age_group_id = c(7:20, 30:32, 235),
                       sex_id = sex_to_pull,
                       year_id = c(1980:current_round))
pops$run_id <- NULL


## GET ALL DRAWS -----------------------------------------------------------------------------------------

# Pull draws
draw_dir <- paste0(in.dir)
files <- list.files(draw_dir)
draws <- rbindlist(lapply(files, function(l) fread(paste0(draw_dir, "/", l))), fill = TRUE)
draws$V1 <- NULL

# Get available draws
draw_cols <- names(draws)[grepl("draw_[0-9]*", names(draws))]

# Drop region and super region information (this is stored as location_id)
draws[, c("region_id", "region_name", "super_region_name", "super_region_id") := NULL]

# Confirm that sex information is there
if (female) {
  draws[, sex_id := 2]
} else {
  draws[, sex_id := 1]
}

# Subset all draws to those 20+
age_draws <- copy(draws[age_group_id >= 9])
std_draws <- copy(draws[age_group_id >= 9])
all_draws <- copy(draws[age_group_id >= 9])



## CALCULATE AGE-SPECIFIC ESTIMATES ----------------------------------------------------------------------

# Collapse age-specific draws 
age_draws[, mean := rowMeans(.SD), .SDcols = draw_cols]
age_draws[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = draw_cols]
age_draws[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = draw_cols]

# Drop draw columns
age_draws[, (draw_cols) := NULL]

# Clean up age information
age_draws <- merge(age_draws, ages[, .(age_group_id, age_group_name)], by = 'age_group_id', all.x = T)



## CALCULATE AGE-STANDARDIZED ESTIMATES ------------------------------------------------------------------

# Add population metadata + age weights
std_draws <- merge(std_draws, pops, by = c("location_id", "year_id", "sex_id", "age_group_id"), all.x = T)
std_draws <- merge(std_draws, ages[, .(age_group_id, age_group_name, age_group_weight_value)], by = "age_group_id", all.x = T)

# Check that everything is there
if(nrow(std_draws[is.na(population)]) > 0) stop("Missing populations") #missing populations are changed subnats
if(nrow(std_draws[is.na(age_group_id)]) > 0) stop("Missing age groups")
if(nrow(std_draws[is.na(age_group_weight_value)]) > 0) stop("Missing age weights")

# Calculate the age-standardized prevalence for each individual draw
std_draws[, (draw_cols) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = draw_cols, by = c("location_id", "year_id", "sex_id")]

# Using all the age-standardized draws, calculate the mean and UI
std_draws[, mean := rowMeans(.SD), .SD = draw_cols]
std_draws[, lower := apply(.SD, 1, quantile, c(.025)), .SDcols = draw_cols]
std_draws[, upper := apply(.SD, 1, quantile, c(.975)), .SDcols = draw_cols]

# Drop draws and other extra columns
std_draws[, (draw_cols) := NULL]
std_draws[, c("V1", "age_group_weight_value", "population") := NULL]

# Since we age standardized every row, each "age-specific row estimate" is now the same; subset to any age group id
# Overwrite the age and population information before subsetting to unique combos
std_draws <- std_draws[age_group_id == 10]
std_draws$age_group_id <- 27
std_draws$age_group_name <- 'Age-standardized'
std_draws <- as.data.table(unique(std_draws))

# Make sure everything is still there
nrow(std_draws[is.na(age_group_id)])
locs[!location_id %in% std_draws$location_id][, .(location_id, ihme_loc_id, location_name)]
sum(!1980:current_round %in% unique(std_draws$year_id))



## CALCULATE ALL AGE ESTIMATES ---------------------------------------------------------------------------

# Add population metadata
all_draws <- merge(all_draws, pops, by = c("location_id", "year_id", "sex_id", "age_group_id"), all.x = T)

# Check that everything is there
if(nrow(all_draws[is.na(population)]) > 0) stop("Missing populations")

# Calculate cases by row
all_draws[, (draw_cols) := .SD * population, .SDcols = draw_cols]

# Sum cases and population by location-year-sex (ie, by everything except age)
all_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "location_id", "sex_id"), .SDcols = c(draw_cols, "population")]

# Diving cases by population to get prevalence draws
all_draws[, (draw_cols) := .SD / population, .SDcols = draw_cols]

# Collapse
all_draws[, mean := rowMeans(.SD), .SDcols = draw_cols]
all_draws[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = draw_cols]
all_draws[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = draw_cols]

# Drop draws and other extra columns
all_draws[, (draw_cols) := NULL]
all_draws$population <- NULL

# Add age
all_draws$age_group_id <- 22
all_draws$age_group_name <- 'All age'



## BIND + SAVE ESTIMATES ---------------------------------------------------------------------------------

# Bind results
final_estimates <- rbind(age_draws, std_draws) %>% rbind(all_draws)

# Add sex information
if (female & sex_to_pull == 2) {
  final_estimates[, sex_id := 2]
  final_estimates[, sex := 'Female']
} else {
  final_estimates[, sex_id := 1]
  final_estimates[, sex := 'Male'] 
}

# Save estimates
write.csv(final_estimates, paste0(out.dir, 'all_estimates.csv'), row.names = F)



