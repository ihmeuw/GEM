rm(list=ls())
run_date <- '2021-10-06'

##### 0. SET UP ######################################################################################################################################################################
pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4,boot)

source(file.path("/ihme/cc_resources/libraries/current/r/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)

"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text =element_text(size=13,face='bold'),plot.title =element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')

##### 1. GET + MERGE DATA #############################################################################################################################################################

#get vaccine hesitancy by age-sex --------
df_hes_csa <- fread(paste0('/FILEPATH/', run_date, '/monthly_hesitancy_by_age_sex.csv'))

#rename age
setnames(df_hes_csa, 'age_bin', 'age')

#format date
df_hes_csa[, date := zoo::as.yearmon(paste(year, month), "%Y %m")]

#drop weighted sample
df_hes_csa$denom_nm <- NULL


#get any vaccine by sex ------------------
df_vax <- fread(paste0('/FILEPATH/', run_date, '/monthly_any_vaccine_by_sex.csv'))

#drop CDC from sex-specific regressions
df_vax <- df_vax[toupper(data_source) != 'CDC']

#format date
df_vax[, date := zoo::as.yearmon(paste(year, month), "%Y %m")]

#drop weighted sample
df_vax$denom_nm <- NULL


#get fully vaccinated by sex  ------------
df_full_vax <- fread(paste0('/FILEPATH/', run_date, '/monthly_fully_vaccinated_by_sex.csv'))
df_full_vax <- df_full_vax[!is.na(proportion)]

#drop weighted sample
df_full_vax$denom_nm <- NULL

#drop CDC from sex-specific regressions
df_full_vax <- df_full_vax[toupper(data_source) != 'CDC'] 

#format date
df_full_vax[, date := zoo::as.yearmon(paste(year, month), "%Y %m")]


#get healthcare by sex  ------------------
hc <- fread('/mnt/team/gem/data/corycns/covid_secondary/RTR/time_series_props/all_healthcare_microdata_formatted_for_timeseries_2021_09_30.csv')
hc <- hc[data_source != 'YouGov']

#drop row names
hc$V1 <- NULL
hc$V1 <- NULL

#standardize age
hc[, age := '18+']

#assign sex_id
hc[toupper(sex) == 'MALE', sex := 'Male']
hc[toupper(sex) == 'FEMALE', sex := 'Female']

hc[sex == 'Male', sex_id := 1]
hc[sex == 'Female', sex_id := 2]

#add ihme_loc_id
hc <- merge(hc, hierarchy[, .(location_id, ihme_loc_id)], by = 'location_id')

#format date
hc[, date := zoo::as.yearmon(date)]
hc[, month := month(date)]
hc[, year := year(date)]


#get economic by sex ---------------------
econ <- readRDS('/mnt/team/gem/data/RTR//econ_timetrends_2021_10_21.Rds')

#drop extra columns
econ[, c('prop_ans', 'se', 'prop_lower', 'prop_upper', 'numerator', 'denom')] <- NULL

#assign sex_id
econ[toupper(sex) == 'MALE', sex := 'Male']
econ[toupper(sex) == 'FEMALE', sex := 'Female']

econ[sex == 'Male', sex_id := 1]
econ[sex == 'Female', sex_id := 2]

#format date
setnames(econ, 'yearmonth', 'date')
econ[, date := zoo::as.yearmon(date)]
econ[, month := month(date)]
econ[, year := year(date)]

#format data source
econ[data_source %like% 'FB', data_source := 'Facebook Global Symptoms Survey']
econ[data_source %like% 'You', data_source := 'YouGov']

#add age limits
econ[, age := '18+']

#rename sample
setnames(econ, 'num_obs', 'sample')

#add location_id
econ <- merge(econ, hierarchy[, .(location_id, ihme_loc_id, location_name)], by = c('ihme_loc_id', 'location_name'))

#get our world in data -------------------
owid <- fread(paste0('/FILEPATH/', run_date, '/monthly_owid.csv'))

#drop row names
owid$V1 <- NULL

#rename
setnames(owid, 'full_vax', 'fully_vaccinated')

#lengthen data
owid <- melt(owid, id.vars = c("location_id", "location_name", "data_source", "month", "age", "year", "sample"))

#rename
setnames(owid, 'variable', 'indicator')
setnames(owid, 'value', 'proportion')

#drop missing values
owid <- owid[!is.na(proportion)]

#format dates
owid[, date := zoo::as.yearmon(paste(year, month), "%Y %m")]

#add locations
owid <- merge(owid, hierarchy[, .(location_id, ihme_loc_id)], by = 'location_id')

#add sex id
owid[, sex := 'Both']
owid[, sex_id := 3]

#drop nauru - not representative
owid <- owid[location_id != 369]

#get cdc data-----------------------------
cdc <- fread(paste0('/FILEPATH/', run_date, '/monthly_cdc.csv'))

#rename variable
setnames(cdc, 'full_vax', 'fully_vaccinated')

#lengthen
cdc <- melt(cdc, id.vars = c("location_id", "location_name", "data_source", "month", "age", "year", "sample"))

#rename
setnames(cdc, 'variable', 'indicator')
setnames(cdc, 'value', 'proportion')

#drop missing values
cdc <- cdc[!is.na(proportion)]

#add date
cdc[, date := zoo::as.yearmon(paste(year, month), "%Y %m")]

#add location data
cdc <- merge(cdc, hierarchy[, .(location_id, ihme_loc_id)], by = 'location_id')

#add sex id
cdc[, sex := 'Both']
cdc[, sex_id := 3]

#bind all --------------------------------
all_data <- rbind(df_vax, df_full_vax) %>%
  rbind(df_hes_csa) %>%
  rbind(hc) %>%
  rbind(econ) %>%
  rbind(owid) %>%
  rbind(cdc) %>%
  as.data.table()


##### 2. PREP FOR REGRESSION #########################################################################################################################################################

#set max value
all_data[proportion > 0.949, proportion := 0.949]

#calculate ci
all_data[, lower := proportion - (1.96 * sqrt((proportion*(1-proportion))/sample))]
all_data[, upper := proportion + (1.96 * sqrt((proportion*(1-proportion))/sample))]

#trim ci
all_data[lower <= 0, lower := 0]
all_data[upper >= 1, upper := 1]

#divide
all_data[, proportion := proportion/0.95]
all_data[, lower := lower/0.95]
all_data[, upper := upper/0.95]

#fix props for logit
all_data[proportion > .999, proportion := .999]
all_data[proportion < .001, proportion := .001]

#calculate logit value
all_data[, logit_value := logit(proportion)]

#edit months (months since Jan. 2020 - add 12 for all months in 2021)
all_data[year == 2021, month := month+12]

#remove
all_data <- all_data[indicator != 'income_lost_combined']
all_data <- all_data[indicator != 'income_lost_combined_all']

#remove small sample sizes
table(all_data[sample<30,indicator])

all_data <- all_data[sample>29]

#make + merge prediction template
template1 <- data.table(expand.grid(month = 1:24, ihme_loc_id = unique(all_data$ihme_loc_id), indicator = unique(all_data[indicator %not in% c('hesitancy', 'vaccinated', 'fully_vaccinated'), indicator]), sex = c('Male', 'Female'), age = '18+'))
template2 <- data.table(expand.grid(month = 1:24, ihme_loc_id = unique(all_data$ihme_loc_id), indicator = unique(all_data[indicator %in% c('vaccinated', 'fully_vaccinated'), indicator]), sex = c('Male', 'Female', 'Both'), age = '18+'))
template3 <- data.table(expand.grid(month = 1:24, ihme_loc_id = unique(all_data$ihme_loc_id), indicator = 'hesitancy', sex = c('Male', 'Female'), age = unique(all_data[indicator == 'hesitancy', age])))

template <- rbind(template1, template2) %>%
   rbind(template3)

all_data <- merge(all_data, template, all = T, by = c('month', 'ihme_loc_id', 'indicator', 'sex', 'age'))

#add location
all_data <- merge(all_data, hierarchy[,.(ihme_loc_id, super_region_name, lancet_label)], by = 'ihme_loc_id')


##### 3. LINEAR REGRESSION ###########################################################################################################################################################

for (i in unique(all_data$indicator)) {
  print(i)
  if (i == 'hesitancy') {
    for (s in unique(all_data[indicator == i, sex])) {
      print(s)
      print(i)
      mod <- lmer(logit_value ~ month + factor(age) + (1|ihme_loc_id), 
                  data = all_data[indicator == i & sex == s])
      
      all_data[indicator == i & sex == s,
               logit_value_pred := predict(mod, newdata = all_data[indicator == i & sex == s], allow.new.levels=T)]
      }
  
    } else {
    for (s in unique(all_data[indicator == i, sex])) {
      print(s)
      print(i)
      mod <- lmer(logit_value ~ month + (1|ihme_loc_id), 
                  data = all_data[indicator == i & sex == s])
      
      all_data[indicator == i & sex == s,
               logit_value_pred := predict(mod, newdata = all_data[indicator == i & sex == s], allow.new.levels=T)]
    }
  }

  }

#calculate predictions
all_data[, prop_pred := inv.logit(logit_value_pred)]

#drop year (using months)
all_data$year <- NULL

#calculate mad, dev, variation, etc.
all_data[,abs.dev := abs(logit_value - logit_value_pred)]
all_data[,mad := median(abs.dev,na.rm=T), by=.(indicator, super_region_name, sex)]
all_data[,variance:=(((proportion*(1-proportion))/sample)^2)]
all_data[,logit_variance:=variance * (1/((proportion)*(1-(proportion))))^2+mad]
all_data[proportion < .01,logit_variance:=variance * (1/((.01)*(1-(.01))))^2+mad]


#keep vars we are going to use
all_data <- all_data[indicator%in%c("emp_lost_combined","fully_vaccinated","health_products_covid",
                                    "healthcare_covid", "hesitancy", "medication_covid", "notworking_careothers",
                                    "preventative_health_covid", "vaccinated")]

#save data for gpr
write.csv(all_data, '/FILEPATH/monthly_linear_priors2.csv', row.names = F)

#save a backup copy
dir.create(paste0('/FILEPATH/', Sys.Date(), '/'), recursive = T, showWarnings = F)
write.csv(all_data, paste0('/FILEPATH/', Sys.Date(), 'monthly_linear_priors.csv'), row.names = F)

