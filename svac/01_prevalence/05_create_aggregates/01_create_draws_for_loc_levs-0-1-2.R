############################################################################################################
## 
# Cleaned Code for: Sexual violence against children before age 18
## Purpose: Aggregate national SVAC draws to region, super region, and global level
## 
###########################################################################################################
rm(list=ls())

female <- F
root <- "FILEPATH"

## SET-UP -----------------------------------------------------------------

#universal parameters
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
in.dir <- paste0(root, "final_draws/", risk, "/", run_id, "/", v,"/")

# libraries + shared functions
pacman::p_load(data.table, magrittr, ggplot2, haven, stringr, parallel, readxl, gridExtra, ggrepel, dplyr)
invisible(sapply(list.files("FILEPATH", full.names = T), source))
source("FILEPATH/utility.r")

# id variables for aggregation
id.vars <- c('location_id', 'year_id', 'sex_id', 'age_group_id')

# get location + age metadata
locs <- get_location_metadata(22, release_id = gbd_release_id) %>% .[,.(location_id,ihme_loc_id,location_name,level,region_name,region_id,super_region_id,super_region_name)]
age_map <- get_ids(table = "age_group")
age_map <- age_map[age_group_id %in% c(7:20, 30:32, 235)]
age_map[,age_group := gsub(" to ","-",age_group_name)]
age_map[,age_group_name := NULL]

# get population data
pops <- get_population(release_id = gbd_release_id,
                       location_id = "all",
                       age_group_id = c(7:20, 30:32, 235),
                       sex_id = sex_to_pull,
                       year_id = c(1980:current_round))


## PREP DATA -------------------------------------------------------------

# 1. Pull draws
draw_dir <- paste0(in.dir)
files <- list.files(draw_dir)
draws <- rbindlist(lapply(files, function(l) fread(paste0(draw_dir, "/", l))), fill = TRUE) #this could be made faster by only reading in national draws
if ('V1' %in% colnames(draws)) {
  draws$V1 <- NULL
}

# 2. Subset all draws to national draws
all_draws <- draws[location_id %in% unique(locs[level==3]$location_id)]

# 3. Merge on population info
all_draws <- merge(all_draws, subset(pops, select = c("location_id", "year_id", "age_group_id", "population")), all.x = T, by = c("location_id", "year_id", "age_group_id"))

# 4. Merge on location info
all_draws <- merge(all_draws, locs, by = "location_id", all.x = T)

# 5. Calculate draws of aggregate locations (of regions, super regions, and globe) 
draw_cols <- names(all_draws)[grepl("draw_[0-9]*", names(all_draws))]
non_draw_cols <- setdiff(names(all_draws), draw_cols)

all_draws[, (draw_cols) := .SD * population, .SDcols = draw_cols] #calculate cases = draw (prevalence) * populations
global_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "age_group_id"), .SDcols = c(draw_cols, "population")] #sum cases and population by age and year (global cases and pop)
super_region_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "age_group_id", "super_region_name", "super_region_id"), .SDcols = c(draw_cols, "population")] #sum cases and population by age, year, and super region (super regional cases and pop)
region_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "age_group_id", "super_region_name", "region_name", "region_id"), .SDcols = c(draw_cols, "population")] #sum cases and population by age, year, and region (regional cases and pop)

global_draws[, (draw_cols) := .SD / population, .SDcols = draw_cols] #calculate prevalence draws == cases / population
super_region_draws[, (draw_cols) := .SD / population, .SDcols = draw_cols] #calculate prevalence draws == cases / population
region_draws[, (draw_cols) := .SD / population, .SDcols = draw_cols] #calculate prevalence draws == cases / population

# 6. Drop extra columns
global_draws$population <- NULL
super_region_draws$population <- NULL
region_draws$population <- NULL

# 7. Add location_id information
global_draws[, location_id := 1]
super_region_draws <- merge(super_region_draws, locs[level == 1, .(location_id, super_region_id, super_region_name)], by = c("super_region_id", "super_region_name"), all.x = T)
region_draws <- merge(region_draws, locs[level == 2, .(location_id, region_id, region_name)], by = c("region_id", "region_name"), all.x = T)

# 8. Save draws
write.csv(global_draws, paste0(in.dir,'global_draws.csv'), row.names = F)
write.csv(super_region_draws, paste0(in.dir,'super_region_draws.csv'), row.names = F)
write.csv(region_draws, paste0(in.dir,'region_draws.csv'), row.names = F)





