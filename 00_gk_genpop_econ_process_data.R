#############################################################################################################
#
# Author: USERNAME
# Purpose: Clean Goalkeeper's premise survey data from 2020 and 2021 for economic indicators
#
#############################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]
out.dir <- 'FILEPATH'

#read in final datasets and merge on weights created by GK evaluation team -------------------------------------------------------------------------------------
gp_2021 <- fread('FILEPATH')

# Read in education mapping file
edu_mapping <- read.csv("FILEPATH")

# Merge the education mapping values onto the data - this is necessary for proper merge onto weights
gp_2021 <- merge(gp_2021, edu_mapping, by.x = "education", by.y = "edu_premise_bin", all.x = T)
gp_2021$sex <- gp_2021$gender

# Read in 2021 weights
weights_2021 <- read.csv("FILEPATH") %>% 
  mutate(country = ifelse(country == "United States of America", "United States", country))
gp_2021<- left_join(gp_2021, weights_2021, by = c("country","sex", "edu_gbd_bin", "age"))

##DROP 7 observations under age 16 - not eligible for survey, have no weights, do not want to keep them
gp_2021 <- filter(gp_2021, age!="Under 16") #drops 7
weight_sum <- data.frame(dplyr::summarise(group_by(gp_2021,country), mean_country_weight = mean(weight, na.rm = T)))
gp_2021 <- merge(gp_2021, weight_sum, by = "country", all.x = T)
gp_2021$weight <- ifelse(is.na(gp_2021$weight), gp_2021$mean_country_weight, gp_2021$weight)
write.csv(gp_2021, paste0(out.dir, 'goalkeepers_2021_microdata_with_weights.csv'))

#read in 2020 module
gp_2020 <- fread('FILEPATH')

#read in 2020 weighting data
weights_2020 <- read.csv("FILEPATH") %>% 
  mutate(country = ifelse(country == "United States of America", "United States", country))

# Format data for merge and merge onto weights
gp_2020 <- gp_2020  %>% 
  mutate(edu = education) %>% 
  mutate(sex = gender) %>% 
  mutate(edu = ifelse(education == "College or university" | education == "Technical school" | education == "Post graduate", "College or more", edu)) %>% 
  mutate(edu = ifelse(edu == "edu_prop_15_or_more", "College or more", edu)) %>% 
  mutate(edu = ifelse(edu == "edu_prop_12_or_more", "Secondary/high school", edu)) %>% 
  mutate(edu = ifelse(edu == "edu_prop_6_or_more", "Primary school", edu)) %>% 
  left_join(weights_2020) #%>% 

weight_sum <- data.frame(dplyr::summarise(group_by(gp_2020,country), mean_country_weight = mean(weight, na.rm = T)))
gp_2020 <- merge(gp_2020, weight_sum, by = "country", all.x = T)
gp_2020$weight <- ifelse(is.na(gp_2020$weight), gp_2020$mean_country_weight, gp_2020$weight)

#drop under 16 year olds for 2020 too
gp_2020 <- gp_2020[age!='Under 16']

#write raw data file, now with weights
write.csv(gp_2020, paste0(out.dir, 'goalkeepers_2020_microdata_with_weights.csv'))

#clean general population data for economic indicators -----------------------------------------------------------------------------------------------------------------------------------------

dem.vars <- c("weight", "location_id", "country","observation_id","gender","age","geography","financial_situation","education","ethnicity", "religion", "gp_hh")

#2021 module; read in and format
dt <- fread('FILEPATH')
dt <- dt[,c("weight", "location_id", "country","observation_id","gender","age","geography","financial_situation","education","ethnicity", "religion", 
            "gp_hh","gp_labor_force_prepandemic", "gp_labor_force", "gp_unemployment_why", "gp_unemployed_when", "gp_income", "gp_job_timeframe")]
dt[, data_collection:='2021']
setnames(dt, c("gp_hh","gp_labor_force_prepandemic", "gp_labor_force", "gp_unemployment_why", "gp_unemployed_when", "gp_income", "gp_job_timeframe", "country"), 
         c('hh_size', 'precovid_emp', 'emp_current', 'unemployment_why', 'unemployment_when', 'income', 'job_type', "location_name"))

#create variables of interest 
dt[,emp_1:=precovid_emp]
dt[,emp_2:=emp_current]
dt[unemployment_why%in%c(1,2,3,4,5,7,8,9,10,99),notworking_careothers:=0]
dt[unemployment_why==6,notworking_careothers:=1]
dt[(emp_1==1 & emp_2 ==0), emp_lost :=1 ]
dt[emp_2 ==1, emp_lost :=0 ]

#write cleaned file
write.csv(dt, 'FILEPATH')

#2020 module; read in data and format -----------------------------------
premise_2020 <- fread('FILEPATH')
premise_2020 <- premise_2020[, c("weight", "location_id", "country","observation_id","gender","age","geography","financial_situation","education","ethnicity", "religion", 
                                 "gp_unemployment_why", "gp_pre_labor_force", "gp_post_labor_force", 
                                 "gp_post_income", "gp_pre_income")]

#create variables of interest
premise_2020 <- premise_2020 %>% mutate(notworking_careothers = case_when(
  gp_unemployment_why %in% c("Business / office closed", "Laid off while business continues", "Furlough (temporarily laid off)", 
                             "Vacation", "Ill or quarantined",
                             "Seasonal worker", "Retired", 
                             "Not able to go to work due to government mandated movement restrictions", "To avoid exposure to COVID-19",
                             "Other") ~ 0, 
  gp_unemployment_why == "Need to care for ill relative" ~ 1))
premise_2020 <- premise_2020 %>% mutate(emp_1 = case_when(
  gp_pre_labor_force == "Yes" ~ 1, 
  gp_pre_labor_force == "No" ~ 0,
))
premise_2020 <- premise_2020 %>% mutate(emp_2 = case_when(
  gp_post_labor_force == "Yes" ~ 1, 
  gp_post_labor_force == "No" ~ 0,
))
premise_2020 <- premise_2020 %>% mutate(emp_lost = case_when( 
  emp_1 == 1 & emp_2 == 0 ~ 1,
  emp_2 == 1 ~ 0))
premise_2020 <- premise_2020 %>% mutate(inc_loss= case_when(
  (gp_post_income < gp_pre_income) & emp_2 == 1  ~ 1,  #income post covid less than pre covid
  (gp_pre_income < gp_post_income) & emp_2 == 1 ~ 0)) #income post covid more than pre covid

#write cleaned file
write.csv(premise_2020, 'FILEPATH')
