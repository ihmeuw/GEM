#############################################################################################################
# Purpose: YouGov economic questions
#############################################################################################################

rm(list=ls())

# (1) Setup ------------------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)

source('/FILEPATH/econ_functions.R')

invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
run_date <- gsub('-', '_', Sys.Date())

# (1) Extract econ data-------------------------------------------------------------------------------------------------------------------------------

fp <- '/FILEPATH/'

#create one large unraked file -----------------------------------------------------------------------------------------------------------------------------------------------
read_draw <- function(c.file){
  print(c.file)
  dat <- fread(c.file)
  dat[, country := str_replace(c.file, fp, '')]
  dat[, country := str_replace(country, '.csv', '')]
  dat[, country := gsub('-', ' ', country)]
  dat[, country := str_to_title(country)]
  dat$V1 <- NULL
  return(dat)
}

files <- list.files(fp)
files <- files[files %like% 'csv']
file.list <- paste0(fp, files)
file.list %>% lapply(read_draw) %>% rbindlist(fill = T) -> yougov_econ

#save merged data
write.csv(yougov_econ,'/FILEPATH/DATASET.csv')

#Date
yougov_econ[, date := as.Date(substr(endtime, 1, 10), "%d/%m/%Y")]

#subset
yougov_econ <- yougov_econ[, c('qweek','gender', 'age', 'weight', 'state', 'country', 'employment_status', 'employment_status_1', 'employment_status_2', 
                               'employment_status_3', 'employment_status_4', 'employment_status_5', 'employment_status_6', 'employment_status_7',
                               'work1',  'work2', 'work3', 'work5', 'work7_5', 'date', 'vac', 'vac5')]

yougov_econ$yearmonth <- format(yougov_econ$date, "%Y-%m")

#Harmonizing the two different ways employment_status was collected:
yougov_econ <- yougov_econ %>% mutate(employment_status_har = case_when(
  yougov_econ$employment_status == "Full time employment" | yougov_econ$employment_status_1 == "Yes" ~ "Full time employment", 
  yougov_econ$employment_status == "Part time employment" | yougov_econ$employment_status_2 == "Yes" ~ "Part time employment",
  yougov_econ$employment_status == "Full time student" | yougov_econ$employment_status_3 == "Yes" ~ "Full time student", 
  yougov_econ$employment_status == "Retired" | yougov_econ$employment_status_4 == "Yes" ~ "Retired",
  yougov_econ$employment_status == "Unemployed" | yougov_econ$employment_status_5 == "Yes" ~ "Unemployed",
  yougov_econ$employment_status == "Not working" | yougov_econ$employment_status_6 == "Yes" ~ "Not working",
  yougov_econ$employment_status == "Other" | yougov_econ$employment_status_7 == "Yes" ~ "Other",
))

#replace missing values with -1; NAs will represent a variable missing from that survey response
yougov_econ <- yougov_econ %>% as.data.table()
yougov_econ[is.na(employment_status_har), employment_status_har := -1]
yougov_econ[work1 == '', work1 := -1]
yougov_econ[work2 == '', work2 := -1]
yougov_econ[work3 == '', work3 := -1]
yougov_econ[work5 == '', work5 := -1]
yougov_econ[work7_5 == '', work7_5 := -1]

#Clean employment_status
yougov_econ$employment_status <- NULL
yougov_econ$employment_status_1 <- NULL
yougov_econ$employment_status_2 <- NULL
yougov_econ$employment_status_3 <- NULL
yougov_econ$employment_status_4 <- NULL
yougov_econ$employment_status_5 <- NULL
yougov_econ$employment_status_6 <- NULL
yougov_econ$employment_status_7 <- NULL

#save cleaned data
write.csv(yougov_econ,'/FILEPATH/DATASET.csv')

saveRDS(yougov_econ, '/FILEPATH/DATASET.Rds')

# (2) Prepare microdata-------------------------------------------------------------------------------------------------------------------------------

data <- readRDS("/FILEPATH/DATASET.Rds")

data[country=='United States', country:='United States of America']
data[country=='Vietnam', country:='Viet Nam']
data[country=='Taiwan', country:='Taiwan (Province of China)']
data[country=='South Korea', country:='Republic of Korea']
data[country=='Hong Kong', country:='Hong Kong Special Administrative Region of China']

setnames(data, "country", "location_name")
data <- data %>% 
  rename(location_name = country) %>% 
  as.data.frame()
# Merge in location data
data <- left_join(data, locs[, c("location_name", "region_name", "super_region_name", "ihme_loc_id")], by = "location_name") %>% as.data.table()

#Add NID
data[, nid:='462696']

#set up age bins
data[age<25, age_bin:='18-24']
data[age<35 & age>24, age_bin:='25-34']
data[age<45 & age>34, age_bin:='35-44']
data[age<55 & age>44, age_bin:='45-54']
data[age<65 & age>54, age_bin:='55-64']
data[age>64, age_bin:='65+']

setnames(data, "age", "age_year")

data[, line_id:=1:.N]

data[, qweek:=factor(qweek, levels=paste0('week ', 1:49))]

###Create variables of interest
#Econ variables
data <- data %>% mutate(emp_current = case_when(
  employment_status_har == "Full time employment" | employment_status_har == "Part time employment" ~ 1,
  employment_status_har == "Full time student" | employment_status_har == "Retired" | 
    employment_status_har == "Unemployed" | employment_status_har == "Not working" |
    employment_status_har == "Other" ~ 0))

#Employment loss  
data <- data %>% mutate(c19_emp_loss_perm = case_when( 
  ((work2 == "Yes, between 2 weeks and 2 months ago" | work2 == "Yes, more than two months ago but since 1st February 2020" | work2 == "Yes, within the past 2 weeks")) & emp_current == 0 ~ 1, 
    work2 == "No" & emp_current == 1 ~ 0)) 

#Income loss
data <- data %>% mutate(c19_inc_loss_bn = case_when( 
  (work3 == "Yes, between 2 weeks and 2 months ago" | work3 == "Yes, more than two months ago but since 1st February 2020" |
    work3 == "Yes, that started within the past 2 weeks") & emp_current == 1  ~ 1,
  work3 == "No" & emp_current == 1 ~ 0)) 

#Vaccination and hesitancy variables
data <- as.data.table(data)

data[vac %like% 'No',  vaccinated := 0]
data[vac %like% 'Yes',  vaccinated := 1]

data[vac %like% 'No', fullvax := 0]
data[vac %like% 'one', fullvax := 0]
data[vac %like% 'two', fullvax := 1]

data[vac %like% 'Yes', hesitant := 0] 
data[vac5 == 'Yes', hesitant := 0] 
data[vac5 == 'Not sure', hesitant := 1] 
data[vac5 == 'No', hesitant := 1] 


#format and print microdata
microdata <- data[, c('line_id', 'ihme_loc_id', 'location_name', 'region_name', 'super_region_name', 'nid', 'gender', 'age_year', 
                      'age_bin', 'emp_current', 'c19_emp_loss_perm', 
                      'c19_inc_loss_bn', 'weight', 'qweek',
                      'date', 'yearmonth', 'vaccinated', 'fullvax', 'hesitant')]

write.csv(microdata,'/FILEPATH/DATASET.csv')