#################################################################################################
#
# Harmonize FB symptoms data prior to running through regression framework
#
# USERNAME
# 
#################################################################################################

# (1) Setup -------------------------------------------------------------------------------------
rm(list = ls())

library(data.table)
library(tidyverse)
library(ggplot2)

# Important cluster functions
invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
locs <- filter(locs, level == 3) #Keep only country data
run_date <- gsub('-', '_', Sys.Date())

# directories
input_dir <- paste0('FILEPATH')

# (2) Merge all microdatasets together ---------------------------------------------------------------------------------------------

symptoms <- readRDS(paste0(input_dir, '/FILEPATH/fb_econvaxhealth_microdata.Rds'))

# Harmonize datasets to bind them
symptoms <- rename(symptoms, ihme_loc_id = ISO_3, gender = sex, age_bin = age)
symptoms <- select(symptoms, gender, ihme_loc_id, age_bin, weight, date, getvax, location_name, super_region_name, region_name, emp_2, emp_1,
                   emp_lost, food_worried, income_worried, notworking_careothers, fullvax, higher_educ, unique_id, lessthan35, age35to64,
                   age65plus,healthcare,healthcare_covid, healthcare_avoidance, medication, medication_covid, preventative_health,
                   preventative_health_covid, health_products, health_products_covid, yearmonth, V1, urban)

symptoms$source <- "FB symptoms"
symptoms$date <- as.Date(symptoms$date)

econ_timeuse <- symptoms

# Rename emp_lost
econ_timeuse <- econ_timeuse %>%
  mutate(emp_lost_combined = case_when(
    source == "FB symptoms" ~ emp_lost))

# Get ihme_loc_id for all observations
econ_timeuse$ihme_loc_id <- NULL
econ_timeuse <- left_join(econ_timeuse, locs[, c("ihme_loc_id", "location_name")], by = "location_name")

# (2) Final clean up of variables for logistic regression  ---------------------------------------------------------------------------------

####Covariates for Fb symptoms data
econ_time_fb <- filter(econ_timeuse, source == "FB symptoms")

#Remove AFG according to FB symptoms guidance
econ_time_fb <- filter(econ_time_fb, location_name!='Afghanistan')

econ_time_fb <- econ_time_fb %>%
  mutate(female = case_when(
    gender == 'Female' ~ 1,
    gender == 'Male' ~ 0
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(rural = case_when(
    urban == 'Rural' ~ 1,
    urban == 'Urban' ~ 0
  ))

econ_time_fb$factortime <- as.factor(econ_time_fb$yearmonth)

#Making all outcomes numeric
econ_time_fb <- econ_time_fb %>%
  mutate(medication = case_when(
    medication == '1' ~ 1,
    medication == '0' ~ 0
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(preventative_health = case_when(
    preventative_health == '1' ~ 1,
    preventative_health == '0' ~ 0
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(health_products = case_when(
    health_products == '1' ~ 1,
    health_products == '0' ~ 0
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(emp_lost_combined  = case_when(
    emp_lost_combined  == '1' ~ 1,
    emp_lost_combined  == '0' ~ 0
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(hesitancy  = case_when(
    getvax  == 'Definitely or Probably Yes' ~ 0,
    getvax  == 'Definitely or Probably Not' ~ 1
  ))

econ_time_fb <- econ_time_fb %>%
  mutate(V1  = case_when(
    V1  == 'Yes' ~ 1,
    V1  %in% c("Don't know", "I don't know", "No") ~ 0
  ))

#Checking all outcomes are numeric
mapply(function(x) class(econ_time_fb[[x]]), c('female', 'factortime', 'age35to64', 'age65plus', 'higher_educ',
                                               'emp_lost_combined', 'notworking_careothers',
                                               'V1', 'fullvax', 'hesitancy', 'healthcare', 'healthcare_covid',
                                               'healthcare_avoidance', 'medication', 'medication_covid',
                                               'preventative_health', 'preventative_health_covid', 'health_products', 'health_products_covid'))

keepvars_fb <- c('female', 'factortime', 'age35to64', 'age65plus', 'higher_educ', 'rural',
                 'emp_lost_combined', 'notworking_careothers',
                 'fullvax', 'hesitancy', 'V1', 'healthcare', 'healthcare_covid',
                 'healthcare_avoidance', 'medication', 'medication_covid',
                 'preventative_health', 'preventative_health_covid', 'health_products', 'health_products_covid', 'location_name',
                 'weight', 'source', 'date')

econ_time_fb_reg <- econ_time_fb[, keepvars_fb]

#Weights
econ_time_fb_reg <- rename(econ_time_fb_reg, weight_og = weight)

econ_time_fb_reg <- econ_time_fb_reg %>%
  mutate(weight  = case_when(
    !is.na(weight_og) ~ weight_og/1000000
  ))

saveRDS(econ_time_fb_reg, "FILEPATH") 