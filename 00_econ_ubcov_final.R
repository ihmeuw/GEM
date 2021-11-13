#################################################################################################
# econ_ubcov_cleaning.R
# Cleaning of goalkeepers and WB economic indicators data
#################################################################################################

# (1) Setup -------------------------------------------------------------------------------------
rm(list = ls())

# Important cluster functions
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
source("/FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]
run_date <- gsub('-', '_', Sys.Date())

library(data.table)
library(tidyverse)
library(ggplot2)
source('/FILEPATH/econ_functions.R')

# (1) Prepare microdata-----------------------------------------------------------------------------------------------------

###WB_COVID19_HIGH_FREQUENCY_PHONE_SURVEY_HFPS

wbf <- '/FILEPATH/'

read_draw <- function(c.file){
  print(c.file)
  dat <- fread(c.file)
  dat[, country := str_replace(c.file, wbf, '')]
  dat[, country := str_replace(country, '.csv', '')]
  dat[, country := gsub('_', ' ', country)]
  dat[, country := gsub('hfps', ' ', country)]
  dat[, country := gsub('monitor', ' ', country)]
  dat[, country := str_to_title(country)]
  return(dat)
}

files <- list.files(wbf)
files <- files[files %like% 'hfps.csv'] #no indicators in georgia_monitor
file.list <- paste0(wbf, files)
file.list %>% lapply(read_draw) %>% rbindlist(fill = T) -> wb_hfps

#Correct country names
wb_hfps$country <- trimws(wb_hfps$country)
wb_hfps <- wb_hfps %>% mutate(country_new = case_when(
  country == "Armenia" ~ "Armenia", 
  country == "Geo" ~ "Georgia",
  country == "India" ~ "India",
  country == "Iraq" ~ "Iraq",
  country == "Khm" ~ "Cambodia",
  country == "Myanmar" ~ "Myanmar",
  country == "Sao Tome Principe" ~ "Sao Tome and Principe",
  country == "Zambia" ~ "Zambia"
  ))

wb_hfps$country <- NULL
wb_hfps <-  rename(wb_hfps, country = country_new)
  
wb_hfps$yearmonth <- format(as.Date(wb_hfps$date), "%Y-%m")

wb_hfps <- wb_hfps %>% mutate(gender= case_when(
  sex == "Female"  ~ "Female", 
  sex == "Male" ~ "Male"
))

wb_hfps$urban <- wb_hfps$urban_rural

wb_hfps <- wb_hfps %>% mutate(education_bin= case_when(
  education_bin == 1  ~ "More than high school", 
  education_bin == 0 ~ "Less than high school"
))

#Merge locs
wb_hfps <- left_join(wb_hfps, locs[, c("location_name", "super_region_name", "ihme_loc_id")], by = "ihme_loc_id")

vars <- c("urban", "education_bin", "age_simple", "emp_lost", 
          "ihme_loc_id", "c19_inc_loss_bn",  "weight", "country", "yearmonth", 
          "super_region_name", "location_id", "gender", "age", "location_name")
vars[vars %ni% colnames(wb_hfps)]

wb_hfps <- wb_hfps[, c("urban", "education_bin", "age_simple", "emp_lost",
                      "ihme_loc_id", "c19_inc_loss_bn",  "weight", "country", "yearmonth", 
                      "super_region_name", "gender", "age", "location_name")]

write.csv(wb_hfps, "/FILEPATH/DATASET.csv")
  
###GOALKEEPERS 2020 and 2021
  goalkeepers_2020 <- fread("/FILEPATH/DATASET.csv")
  goalkeepers_2021 <- fread("/FILEPATH/DATASET.csv")
  
  #Goalkeepers 2020
  goalkeepers_2020 <- goalkeepers_2020[, c("emp_2", "emp_lost", "notworking_careothers", "inc_loss", "weight", "country", "geography", 
                                           "location_id", "gender", "age", "education")]
  
  goalkeepers_2020 <- goalkeepers_2020 %>%
    rename(age_bin = age, education_bin = education)
  
  #Get right location_name for goalkeepers_2020
  goalkeepers_2020 <- left_join(goalkeepers_2020, locs[, c("location_name", "location_id", "super_region_name")],  by = "location_id" )
  
  goalkeepers_2020 <- goalkeepers_2020 %>% mutate(urban= case_when(
    geography %in% c("City center or metropolitan area", "Suburban/Peri-urban")  ~ 'Urban', 
    geography == "Rural" ~ 'Rural',
  ))
  goalkeepers_2020$geography <- NULL
  goalkeepers_2020$country <- NULL
  goalkeepers_2020$source <- "Goalkeepers 2020"
  
  goalkeepers_2020$date <- "2020-07-24"
  goalkeepers_2020$yearmonth <- format(as.Date(goalkeepers_2020$date), "%Y-%m")
  
  goalkeepers_2020 <- goalkeepers_2020 %>% mutate(gender= case_when(
    gender == "Female"  ~ "Female", 
    gender == "Male" ~ "Male",
  ))
  
  #Drop if location_name is missing
  goalkeepers_2020 <- filter(goalkeepers_2020, !is.na(location_name))
  goalkeepers_2020 <- filter(goalkeepers_2020, !is.na(gender))
  
  goalkeepers_2020 <- goalkeepers_2020 %>% mutate(inc_loss= case_when(
    inc_loss == 1 & emp_2 == 1 ~ 1,
    inc_loss == 0 & emp_2 == 1  ~ 0))
  
  #Goalkeepers 2021
  goalkeepers_2021 <- goalkeepers_2021[, c("emp_2", "emp_lost", "notworking_careothers", "weight", "location_name", "geography", 
                                           "location_id", "gender", "age", "education")]
  
  goalkeepers_2021 <- goalkeepers_2021 %>%
    rename(age_bin = age, education_bin = education)
  
  #Get right location_name for goalkeepers_2020
  goalkeepers_2021 <- left_join(goalkeepers_2021, locs[, c("location_name", "location_id", "super_region_name")],  by = "location_id" )
  goalkeepers_2021$location_name.x <- NULL
  goalkeepers_2021 <- goalkeepers_2021 %>%
    rename(location_name = location_name.y)
  
  goalkeepers_2021 <- goalkeepers_2021 %>% mutate(urban= case_when(
    geography %in% c("City center or metropolitan area", "Suburban/Peri-urban")  ~ 'Urban', 
    geography == "Rural" ~ 'Rural',
  ))
  
  goalkeepers_2021$geography <- NULL
  goalkeepers_2021$source <- "Goalkeepers 2021"
  
  goalkeepers_2021$date <- "2020-05-24"
  goalkeepers_2021$yearmonth <- format(as.Date(goalkeepers_2021$date), "%Y-%m")
  
  goalkeepers_2021 <- goalkeepers_2021 %>% mutate(gender= case_when(
    gender == "Female"  ~ "Female", 
    gender == "Male" ~ "Male",
  ))
  
  #Drop if location_name is missing
  goalkeepers_2021 <- filter(goalkeepers_2021, !is.na(location_name))
  goalkeepers_2021 <- filter(goalkeepers_2021, !is.na(gender))
  
  write.csv(goalkeepers_2020, "/FILEPATH/DATASET.csv")
  write.csv(goalkeepers_2021, "/FILEPATH/DATASET.csv")
  
  #Bind goalkeepers
  goalkeepers_2020$wave <- 1
  goalkeepers_2021$wave <- 2
  
  goalkeepers <- bind_rows(goalkeepers_2020, goalkeepers_2021)