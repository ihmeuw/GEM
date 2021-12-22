####################################################################################################################################################################
#
# Creating time series proportions for FB and YouGov econ indicators
#
# USERNAME
#
######################################################################################################################################################################

# (1) Setup ------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())

# user arguments
run_date <- gsub('-', '_', Sys.Date())
lsid <- 111
lsvid <- 771
loc_strat <- 'location_name' 

# directories
output_dir <- paste0('FILEPATH', run_date)
dir.create(output_dir, recursive = TRUE)

# libraries
library(ggplot2)
library(data.table)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(ggthemes)

#functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/econ_functions_datatable.R")

#lookup tables
hier <- get_location_metadata(location_set_id = lsid, location_set_version_id = lsvid, gbd_round_id = 7, release_id = 9) %>% 
  select(3:6,10,18,20,21,30) %>%
  filter(level == 3) %>%
  as.data.table()

# (2) Harmonize FB and yougov ---------------------------------------------------------------------------------------------

# symptoms <- data
symptoms <- readRDS(paste0('FILEPATH'))
input_dir <- paste0('FILEPATH')
yougov <- fread(paste0(input_dir, '/yougov/microdata_econ.csv'))

# Harmonize datasets to bind them
symptoms <- rename(symptoms, ihme_loc_id = ISO_3, gender = sex, age_bin = age)
symptoms <- select(symptoms, gender, ihme_loc_id, age_bin, weight, date, location_name, super_region_name, region_name, 
                   emp_lost, notworking_careothers, higher_educ, unique_id, lessthan35, age35to64,
                   age65plus, yearmonth, urban)

symptoms$source <- "FB symptoms"

yougov$V1 <- NULL
yougov$line_id <- NULL
yougov$source <- "Yougov"

#merge wasn't working bc symptoms date was character; all else are date format
symptoms$date <- as.Date(symptoms$date)

econ_timeuse <- bind_rows(symptoms, yougov)

#Create emp loss indicator:
econ_timeuse$c19_emp_loss_perm <- as.numeric(econ_timeuse$c19_emp_loss_perm)

# Loss employment restrictive denominator
econ_timeuse <- econ_timeuse %>%
  mutate(emp_lost_combined = case_when(
    source == "FB symptoms" ~ emp_lost,
    source == "Yougov" ~ c19_emp_loss_perm))

#Loss income all denominator
econ_timeuse <- econ_timeuse %>%
  mutate(income_lost_combined_all = case_when(
    source == "Yougov" ~ c19_inc_loss_bn_all))

#Get ihme_loc_id for all observations
econ_timeuse$ihme_loc_id <- NULL
econ_timeuse <- left_join(econ_timeuse, locs[, c("ihme_loc_id", "location_name")], by = "location_name")

saveRDS(econ_timeuse, paste0('FILEPATH'))

# (2) Loading FB symptoms microdata ------------------------------------------------------------------------------------
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
econ_timeuse <- readRDS(paste0('FILEPATH')) 

# (3) FB symptoms and Yougov props for time trends model--------------------------------------------------------------------------

econ_time_symptoms <- filter(econ_timeuse, source == "FB symptoms")
econ_time_yougov <- filter(econ_timeuse, source == "Yougov")

prop_econ_symptoms <- prop(econ_time_symptoms,
                           wt = 'weight', 
                           var_list = c('emp_lost_combined', 
                                        'notworking_careothers'),
                           group_vec = c('gender', 'source', 'ihme_loc_id', 'location_name', 'yearmonth'))

prop_econ_yougov <- prop(econ_time_yougov,
                         wt = 'weight', 
                         var_list = c('emp_lost_combined', 'income_lost_combined_all'),
                         group_vec = c('gender', 'source', 'ihme_loc_id', 'location_name', 'yearmonth'))

econ_timetrends <- bind_rows(filter(prop_econ_symptoms$emp_lost_combined, location_name!='Afghanistan'),
                             filter(prop_econ_symptoms$notworking_careothers, location_name!='Afghanistan'),
                             prop_econ_yougov$emp_lost_combined, 
                             prop_econ_yougov$income_lost_combined_all)

econ_timetrends <- rename(econ_timetrends, sex = gender, 
                          indicator = type, 
                          proportion = prop, 
                          data_source = source, 
                          numerator = n)

#save
saveRDS(econ_timetrends, paste0(input_dir,"/econ_timetrends_", gsub('-', '_', Sys.Date()), ".Rds")) #This line has changed since first submission
