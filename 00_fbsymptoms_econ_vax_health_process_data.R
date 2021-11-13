####################################################################################################################################################################
# 
# Analysis of indicators from FB: economic and other work-related concerns, vaccine hesitancy and updake, and health care disruptions
#
# AUTHORS
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

#lookup tables
hier <- get_location_metadata(location_set_id = lsid, location_set_version_id = lsvid, gbd_round_id = 7, release_id = 9) %>% 
  select(3:6,10,18,20,21,30) %>%
  filter(level == 3) %>%
  as.data.table()

# (2) Loading FB symptoms microdata ------------------------------------------------------------------------------------
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)

microdata_dir_global = 'FILEPATH'
microdata_dir_us ='FILEPATH'

daily_microdata_files_global <- list.files(microdata_dir_global)
daily_microdata_files_us <- list.files(microdata_dir_us)

subcols_global = c("ISO_3", "country_agg", "sex", "age", 'weight', "date", "D4", "D5", "D7", "D7_a", "D8", "D9", "educ_e6", "educ_e8", "urban",
            "V1", "V2", "V2a", "getvax", "B13_1", "B13_2", "B13_3", "B13_4", "B13_5", "B13_6", "B13_7", "B14_1", "B14_2", "B14_3", "B14_4", "B14_5")
subcols_us = c("ISO_3", "country_agg", "sex", "age", 'weight', "date", "D9", "educ_d8",
            "V1", "V2", "V2a", "getvax")

data_global <- data.frame()

for (i in 1:length(daily_microdata_files_global)){
   print(i)
   # read in data
   df <- read.csv(paste0(microdata_dir_global, daily_microdata_files_global[i]))
   cols <- colnames(df)
   if(any(subcols_global %in% cols)){
     print("pulling")
     subcols_each <- subcols_global[subcols_global %in% cols]
     df <- subset(df, select=subcols_each)
     data_global <- bind_rows(data_global, df)
   }
}

data_us <- data.frame()

for (i in 1:length(daily_microdata_files_us)){
  print(i)
  # read in data
  df <- read.csv(paste0(microdata_dir_us, daily_microdata_files_us[i]))
  cols <- colnames(df)
  if(any(subcols_us %in% cols)){
    print("pulling")
    subcols_each <- subcols_us[subcols_us %in% cols]
    df <- subset(df, select=subcols_each)
    data_us <- bind_rows(data_us, df)
  }
}

dim(data_global)#, 52183840 observations, 53332182 observations on June 25th
dim(data_us)#, 21646076 observations

# group dates that are close together, small sample sizes in some off dates
data_global$date[(data_global$date == "2020-12-09")] <- "2020-12-10"
data_global$date[(data_global$date == "2021-01-01")] <- "2020-12-31"
data_global$date[(data_global$date == "2021-02-01")] <- "2021-01-31"
data_global$date[(data_global$date == "2021-03-01")] <- "2021-02-28"
data_global$date[(data_global$date == "2021-04-01")] <- "2021-03-31"
data_global$date[(data_global$date == "2021-05-01")] <- "2021-04-30"
data_global$date[(data_global$date == "2021-06-01")] <- "2021-05-31"

#Adding country and ISO_3 to the US
data_us$ISO_3 <- "USA"
data_us$location_name <- "United States of America"

data <- bind_rows(data_global, data_us)

saveRDS(data, 'FILEPATH')

# (3) Cleaning FB symptoms microdata ------------------------------------------------------------------------------------
data <- readRDS('FILEPATH')

#Economic variables
data <- as.data.table(data)

#filter to non-missing sex/age
data <- data[sex!='' & age!='']

#get regional/super_regional info
locs <- get_location_metadata(location_set_id=35, gbd_round_id=7)
data <- merge(data, locs[, c('location_name', 'region_name', 'super_region_name', 'ihme_loc_id')], 
              by.x='ISO_3', by.y='ihme_loc_id')

#D8 is only asked if D7 == "No"
data <- data %>% mutate(emp_lost = case_when(
  D7 == 'No' & D8 == 'Yes' ~ 1, #not currently working but were working before feb 2020
  D7 == 'Yes' ~ 0)) #currently working

data <- data %>% mutate(notworking_careothers = case_when(
  D9 == "I am a seasonal worker" | D9 == "I was ill or quarantined" |
    D9 == "I was laid off or furloughed" | D9 == "My employer closed for another reason" |
    D9 == "My employer closed for coronavirus-related reasons" |
    D9 == "Other" ~ 0,
  D9 == "I needed to care for someone" ~ 1))

saveRDS(data, paste0('FILEPATH'))

#Vaccine variables:
data <- as.data.table(data)

#assign NAs
data[V1 %in% c('', 'SKIP', "MISSING"), fullvax := NA] #if they did not answer the first vaccine question
data[V2 %in% c('', "MISSING"), fullvax := NA] #if they are missing the num. of doses question (does not include SKIPPED values, which are assigned to non-vaccinated people)
#assign not fully vaccinated statuses
data[V1 %like% 'No' | V1 %like% 'know', fullvax := 0] #if they have not been vaccinated
data[V1 %like% 'Yes', fullvax := 0] #if they have been vaccinated, assign to 0; overwrite later
data[!V2 %like% '2', fullvax := 0] #if they have not received two doses
#assign fully vaccinated statuses
data[V2 %like% '2', fullvax := 1] #if they have received two doses
data[V2 %like% '1' & V2a %like% 'received all required doses', fullvax := 1] #if they received one dose AND say that's all that's required

#fullvax is a logical, replacing to numerical
data <- data %>% mutate(fullvax = case_when(
  fullvax == TRUE ~ 1, 
  fullvax == FALSE ~ 0))

#Create education, and age stratifiers
data <- data %>% mutate(higher_educ = case_when(
  !is.na(educ_e6) & educ_e6 %in% c('13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
                                   '26', '27', '28', '29', '30') ~ 1,
  !is.na(educ_e6) & educ_e6 %in% c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12') ~ 0,
  !is.na(educ_e8) & educ_e8 %in% c('College pre-university University completed','University post-graduate degree completed') ~ 1,
  !is.na(educ_e8) & educ_e8 %in% c('Less than primary school','No formal schooling','Primary school completed',
                                   'Secondary school completed', 'High school or equivalent completed') ~ 0,
  !is.na(educ_d8) & educ_d8 %in% c('2 year degree','4 year degree', 'Doctorate', 'Some college', 'Professional degree') ~ 1,
  !is.na(educ_d8) & educ_d8 %in% c('High school graduate or equivalent GED', 'Less than high school') ~ 0
))

data <- data %>% 
  mutate(
    lessthan35 = case_when(
      age %in% c('18-24', '25-34') ~ 1,
      age %in% c('35-44', '45-54', '55-64', '65-74', '75+') ~0), 
    age35to64 = case_when(
      age %in% c('35-44', '45-54', '55-64') ~ 1,
      age %in% c('18-24', '25-34', '65-74', '75+') ~ 0),
    age65plus = case_when(
      age %in% c('65-74', '75+') ~ 1, 
      age %in% c('18-24', '25-34', '35-44', '45-54', '55-64') ~0)
  )

#Create unique id per row to save datasets (econ+vax and healthcare) and then merge them
data$unique_id <- rownames(data)

saveRDS(data, paste0('FILEPATH'))

# Healthcare variables:

# drop where all answers were skipped
df <- copy(data)
df <- as.data.table(df)
df <- df[!((df$B13_1 == 'SKIP') & (df$B13_2 == 'SKIP') & (df$B13_3 == 'SKIP') & (df$B13_4 == 'SKIP') & (df$B13_5 == 'SKIP') & (df$B13_6 == 'SKIP') & (df$B13_7 == 'SKIP')), ]
# drop where all answers are missing too
df <- df[!((df$B13_1 == 'MISSING') & (df$B13_2 == 'MISSING') & (df$B13_3 == 'MISSING') & (df$B13_4 == 'MISSING') & (df$B13_5 == 'MISSING') & (df$B13_6 == 'MISSING') & (df$B13_7 == 'MISSING')), ]

#rename and code b13 vars
b13_names <- c('emergency_transport', 'inpatient_care', 'outpatient_care', 'preventative_health', 'medication', 'ppe', 'health_products')
setnames(df, names(df)[names(df) %like% 'B13_'], b13_names)
df[, preventative_health_1:=preventative_health]
df[, outpatient_care_1:=outpatient_care]
df[, inpatient_care_1:=inpatient_care]
df[!emergency_transport%in%c('SKIP', 'MISSING'), emergency_transport:=ifelse(emergency_transport=='Yes',1,0)]
df[!inpatient_care%in%c('SKIP', 'MISSING'), inpatient_care:=ifelse(inpatient_care=='Yes',1,0)]
df[!outpatient_care%in%c('SKIP', 'MISSING'), outpatient_care:=ifelse(outpatient_care=='Yes',1,0)]
df[!preventative_health%in%c('SKIP', 'MISSING'), preventative_health:=ifelse(preventative_health=='Yes',1,0)]
df[!ppe%in%c('SKIP', 'MISSING'), ppe:=ifelse(ppe=='Yes',1,0)]
df[!health_products%in%c('SKIP', 'MISSING'), health_products:=ifelse(health_products=='Yes',1,0)]
df[!medication%in%c('SKIP', 'MISSING'), medication:=ifelse(medication=='Yes',1,0)]
df[!preventative_health_1%in%c('SKIP', 'MISSING') & !outpatient_care_1%in%c('SKIP', 'MISSING'), 
     outpatient_or_preventative:=ifelse(preventative_health_1=='Yes' | outpatient_care_1=='Yes',1,0)]
df[!preventative_health_1%in%c('SKIP', 'MISSING') & !outpatient_care_1%in%c('SKIP', 'MISSING') | !inpatient_care_1%in%c('SKIP', 'MISSING'), 
     healthcare:=ifelse(preventative_health_1=='Yes' | outpatient_care_1=='Yes' | inpatient_care_1=='Yes',1,0)]
#rename and code b14 vars
b14_names <- c('did_not_know_where', 'could_not_afford', 'unable_to_travel', 'afraid_of_infection', 'not_available')
setnames(df, names(df)[names(df) %like% 'B14_'], b14_names)
for (b in b14_names){
  df[!get(b)%in%c('SKIP', 'MISSING'), paste0(b):=ifelse(get(b)=='Yes',1,0)]
}
#if any of disruptions were yes, then respondent should have been asked fup for reason. If skip or missing, code as missing observation
df[, afraid_of_infection_fup:=ifelse((health_products==1 | emergency_transport==1 | outpatient_care==1 | preventative_health==1 | medication==1 | ppe==1) & afraid_of_infection %in% c('SKIP', 'MISSING'), 'missing', 0)]
df[, unable_to_travel_fup:=ifelse((health_products==1 | emergency_transport==1 | outpatient_care==1 | preventative_health==1 | medication==1 | ppe==1) & unable_to_travel %in% c('SKIP', 'MISSING'), 'missing', 0)]
#make due to covid and avoidance variables
for (b in c(b13_names, 'healthcare')){
  df[!get(b)%in%c('SKIP', 'MISSING') & !afraid_of_infection_fup=='missing' & !unable_to_travel_fup=='missing', paste0(b, '_covid'):=ifelse(get(b)==1 & (afraid_of_infection==1 | unable_to_travel==1),1,0)]
  df[!get(b)%in%c('SKIP', 'MISSING') & !afraid_of_infection_fup=='missing', paste0(b, '_avoidance'):=ifelse(get(b)==1 & afraid_of_infection==1,1,0)]
}

df <- select(df, healthcare, healthcare_covid, healthcare_avoidance, medication, medication_covid, 
             preventative_health, preventative_health_covid, health_products, health_products_covid, unique_id)

saveRDS(df, 'FILEPATH')

#Merge econ, vax and health variables together:
fb_symptoms <- left_join(data, df, by = "unique_id")

#Year-month variable
fb_symptoms$yearmonth <- format(as.Date(fb_symptoms$date), "%Y-%m")
fb_symptoms$location_name <- fb_symptoms$location_name.y

#Save microdata
saveRDS(fb_symptoms, 'FILEPATH')

