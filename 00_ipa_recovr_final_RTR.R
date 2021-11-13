####################################################################################################################################################################
# Purpose: Extraction of economic variables from IPA RECOVR panel survey 
####################################################################################################################################################################

# (1) Setup --------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
source('/FILEPATH/econ_functions.R')
source("/FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
run_date <- gsub('-', '_', Sys.Date())

#Set directories
in.dir <- '/FILEPATH/'

# (2) Prepare microdata---------------------------------------------------------------------------------------------------------------------------------------------

# Read in data
zam1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA')) 
zam2 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA')) 
col1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA')) 
col2 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
col3 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
civ1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
civ2 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
rwa1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
rwa2 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
phl1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
mex1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
sle1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
sle2 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA'))
gha1 <- haven::read_dta(paste0(in.dir, 'FILEPATH/DATASET.DTA')) 

# Microdata: Create variables in each dataset and bind them together 

#Set yearmonth for waves
zam1$date<- "2020-06-01"
zam2$date <- "2020-12-01"
col1$date <- "2020-05-01"
col2$date <- "2020-08-01"
col3$date <- "2020-11-01"
civ1$date <- "2020-06-01"
civ2$date <- "2020-10-01"
rwa1$date <- "2020-06-01"
rwa2$date <- "2020-10-01"
phl1$date <- "2020-06-01"
mex1$date <- "2020-06-01"
sle1$date <- "2020-05-01"
sle2$date <- "2020-09-01"
gha1$date <- "2020-05-01"

#Set country for waves
zam1$location_name<- "Zambia"
zam2$location_name <- "Zambia"
col1$location_name <- "Colombia"
col2$location_name <- "Colombia"
col3$location_name <- "Colombia"
civ1$location_name <- "Côte d'Ivoire"
civ2$location_name <- "Côte d'Ivoire"
rwa1$location_name <- "Rwanda"
rwa2$location_name <- "Rwanda"
phl1$location_name <- "Philippines"
mex1$location_name <- "Mexico"
sle1$location_name <- "Sierra Leone"
sle2$location_name <- "Sierra Leone"
gha1$location_name <- "Ghana"

#Harmonizing names of common variables
  zam2 <- dplyr::rename(zam2, dem2 = dem2r)
  zam2 <- dplyr::rename(zam2, dem1 = dem1_age)
  col1 <- dplyr::rename(col1, dem1_old = dem1)
  col1 <- dplyr::rename(col1, dem1 = dem14)
  sle1 <- dplyr::rename(sle1, caseid = id)
  sle2 <- dplyr::rename(sle2, caseid = id)
  
  col1<- col1 %>% mutate(inc7_new = case_when(
    inc7 == 2 ~0, 
    inc7 == 1 ~ 1
    ))
  
  sle2<- sle2 %>% mutate(inc7_new = case_when(
    inc7 == 2 ~0, 
    inc7 == 1 ~ 1
  ))
  
  col1 <- dplyr::rename(col1, inc7_old = inc7)
  col1 <- dplyr::rename(col1, inc7 = inc7_new)
  sle2 <- dplyr::rename(sle2, inc7_old = inc7)
  sle2 <- dplyr::rename(sle2, inc7 = inc7_new)
  
  civ1 <- civ1 %>% mutate(inc8_new = case_when(
    inc8 == 0 ~ 4, 
    inc8 == 1 ~ 1, 
    inc8 == 2 ~ 2, 
    inc8 == 3 ~ 3,
  ))
  
  civ2 <- civ2 %>% mutate(inc8_new = case_when(
    inc8 == 0 ~ 4, 
    inc8 == 1 ~ 1, 
    inc8 == 2 ~ 2, 
    inc8 == 3 ~ 3,
  ))
  
  sle2 <- sle2 %>% mutate(inc8_new = case_when(
    inc8 == 0 ~ 4, 
    inc8 == 1 ~ 1, 
    inc8 == 2 ~ 2, 
    inc8 == 3 ~ 3,
  ))
  
  civ1 <- dplyr::rename(civ1, inc8_old = inc8)
  civ1 <- dplyr::rename(civ1, inc8 = inc8_new)
  civ2 <- dplyr::rename(civ2, inc8_old = inc8)
  civ2 <- dplyr::rename(civ2, inc8 = inc8_new)
  sle2 <- dplyr::rename(sle2, inc8_old = inc8)
  sle2 <- dplyr::rename(sle2, inc8 = inc8_new)
  
  civ1 <- civ1 %>% mutate(inc9_new = case_when(
    inc9 == 0 ~ 4, 
    inc9 == 1 ~ 1, 
    inc9 == 2 ~ 2, 
    inc9 == 3 ~ 3,
  ))
  
  civ2 <- civ2 %>% mutate(inc9_new = case_when(
    inc9 == 0 ~ 4, 
    inc9 == 1 ~ 1, 
    inc9 == 2 ~ 2, 
    inc9 == 3 ~ 3,
  ))
  
  sle2 <- sle2 %>% mutate(inc9_new = case_when(
    inc9 == 0 ~ 4, 
    inc9 == 1 ~ 1, 
    inc9 == 2 ~ 2, 
    inc9 == 3 ~ 3,
  ))
  
  civ1 <- dplyr::rename(civ1, inc9_old = inc9)
  civ1 <- dplyr::rename(civ1, inc9 = inc9_new)
  civ2 <- dplyr::rename(civ2, inc9_old = inc9)
  civ2 <- dplyr::rename(civ2, inc9 = inc9_new)
  sle2 <- dplyr::rename(sle2, inc9_old = inc9)
  sle2 <- dplyr::rename(sle2, inc9 = inc9_new)

#Function to create variables of interest
  var_mapping <- function(dt) {
    dt <- as.data.table(dt)
    setnames(dt, c('dem1', 'dem2'),
             c('age', 'sex'),
             skip_absent = T)
    #gender and age group 
    if ('age' %in% names(dt)){
      dt <- as.data.frame(dt)
      dt$age <- as.numeric(dt$age)
      dt <- as.data.table(dt)
      dt[age<25, age_bin:='18-24']
      dt[age<35 & age>24, age_bin:='25-34']
      dt[age<45 & age>34, age_bin:='35-44']
      dt[age<55 & age>44, age_bin:='45-54']
      dt[age<65 & age>54, age_bin:='55-64']
      dt[age>64, age_bin:='65+']
    }
    
    dt <- as.data.frame(dt)
    
    dt$caseid <- as.character(dt$caseid)
    
    if ('sex' %in% names(dt)){
      dt <- dt %>% mutate(gender= case_when(
        sex == 1 ~ "Female", 
        sex == 2 ~ "Male"))
    }
    
    #Create econ variables
    if ('inc7' %in% names(dt)){
    dt <- dt %>% mutate(emp_current= case_when(
      inc7 == 1 ~ 1, 
      inc7 == 0 ~ 0))
    }
    
    if ('inc7' %in% names(dt)){
    dt <- dt %>% mutate(emp_2= case_when( 
      inc7 == 1 ~ 1, 
      inc7 == 0 ~ 0)) 
    }
    
    if ('inc8' %in% names(dt)){
    dt <- dt %>% mutate(c19_emp_hr_red= case_when(
      inc8 == 3 ~ 1, 
      inc8 == 1 ~ 0, 
      inc8 == 2 ~ 0,
      inc8 == 4 ~ 0,
      ))  
    }
    
    if ('inc9' %in% names(dt)){
    dt <- dt %>% mutate(c19_inc_loss_bn= case_when(
      inc9 == 3 ~ 1, 
      inc9 == 1 ~ 0, 
      inc9 == 2 ~ 0,
      inc9 == 4 ~ 0,
    )) 
    }
    
    dt$yearmonth <- format(as.Date(dt$date), "%Y-%m") 

    #Keep variables of interest
    dt <- dt[, c("caseid", "location_name", "date", colnames(dt)[colnames(dt) %in% c("age", "age_bin", "gender", "emp_current", 
                                                            "emp_2", "c19_emp_hr_red", "c19_inc_loss_bn", "dem11", "dem13")])]
    
    return(dt)
  }

  zam1_final <- var_mapping(zam1)
  zam2_final <- var_mapping(zam2)
  col1_final <- var_mapping(col1)
  col2_final <- var_mapping(col2)
  col3_final <- var_mapping(col3)
  civ1_final <- var_mapping(civ1)
  civ2_final <- var_mapping(civ2)
  rwa1_final <- var_mapping(rwa1)
  rwa2_final <- var_mapping(rwa2)
  phl1_final <- var_mapping(phl1)
  mex1_final <- var_mapping(mex1)
  sle1_final <- var_mapping(sle1)
  sle2_final <- var_mapping(sle2)
  gha1_final <- var_mapping(gha1)
  
#Create education variable:
  zam1_final <- zam1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,2,3)  ~ "Less than high school", 
    dem11  ==4 ~ "High school graduate", 
    dem11 == 5 ~ "More than high school"))
  
  col1_final <- col1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,2)  ~ "Less than high school", 
    dem11  ==3 ~ "High school graduate", 
    dem11 %in% c(4,5,6,7) ~ "More than high school"))
  
  civ1_final <- civ1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,3)  ~ "Less than high school", 
    dem11  %in% c(2,5)~ "High school graduate", 
    dem11 %in% c(4,6) ~ "More than high school"))
  
  rwa1_final <- rwa1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,2)  ~ "Less than high school", 
    dem11  == 3~ "High school graduate", 
    dem11 == 4 ~ "More than high school"))
  
  phl1_final <- phl1_final %>% mutate(education_bin = case_when(
    dem13  %in% c(0,1,2)  ~ "Less than high school", 
    dem13  == 3~ "High school graduate", 
    dem13 %in% c(4,5) ~ "More than high school"))
  
  sle1_final <- sle1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,2,3)  ~ "Less than high school", 
    dem11  == 4 ~ "High school graduate", 
    dem11 %in% c(5,6) ~ "More than high school"))
  
  gha1_final <- gha1_final %>% mutate(education_bin = case_when(
    dem11  %in% c(0,1,2)  ~ "Less than high school", 
    dem11  == 3 ~ "High school graduate", 
    dem11 == 4 ~ "More than high school"))
  
#Get gender, age and education in different rounds
  col3_final <- left_join(col3_final, col1_final[, c('caseid', 'age', 'age_bin', 'education_bin', 'gender')], by = 'caseid')
  civ2_final <- left_join(civ2_final, civ1_final[, c('caseid', 'age', 'age_bin', 'education_bin', 'gender')], by = 'caseid')
  sle2_final <- left_join(sle2_final, sle1_final[, c('caseid', 'age', 'age_bin', 'education_bin', 'gender')], by = 'caseid')
  
#Bind datasets
ipa_recovr <- bind_rows(zam1_final, zam2_final, col1_final, col3_final, civ1_final, civ2_final, rwa1_final,
                        phl1_final, mex1_final, sle1_final, sle2_final, gha1_final)

#Year-month variable
ipa_recovr$yearmonth <- format(as.Date(ipa_recovr$date), "%Y-%m")

ipa_recovr <- filter(ipa_recovr, !is.na(gender))
ipa_recovr$weight <- 1

#Get super regions
ipa_recovr <- left_join(ipa_recovr, locs[, c("location_name", "region_name", "super_region_name", "ihme_loc_id")], by = "location_name")

#Save microdata
write.csv(ipa_recovr,'/FILEPATH/DATASET.csv')