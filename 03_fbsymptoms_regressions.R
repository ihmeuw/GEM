#################################################################################################
# 
# Logistic regressions for indicators available from FB symptoms data
#
# USERNAME
# 
#
#################################################################################################

# (1) Setup -------------------------------------------------------------------------------------
rm(list = ls())

start_run <- Sys.time()

library(data.table)
library(tidyverse)
library(ggplot2)
library(lme4)
library(stargazer)
library(gtools)
source('FILEPATH/econ_functions_datatable.R')

# Important cluster functions
invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
run_date <- gsub('-', '_', Sys.Date())

out.dir <- 'FILEPATH'

# (1) Read in already-cleaned files & get population weights ---------------------------------------------------------------------------------
fb_microdata <- readRDS('FILEPATH')

# (2) Logistic regressions at the individual level by time period ---------------------------------------------------------------------------------

fb_microdata <- fb_microdata %>%
  mutate(time_period = case_when(
    as.Date(date) < '2020-12-01' ~ 'July2020_Nov2020', 
    as.Date(date) < '2021-04-01' & as.Date(date) > '2020-11-30' ~ 'Dec2020_Mar2021', 
    as.Date(date) > '2021-03-31' ~ 'Apr2021+', 
  ))

fb_microdata <- fb_microdata %>%
  mutate(time_period_vax = case_when( 
    as.Date(date) < '2021-04-01' & as.Date(date) > '2020-12-31' ~ 'Jan2021_Mar2021', 
    as.Date(date) > '2021-03-31' ~ 'Apr2021+', 
  ))

econhealth_outcomes <- c('emp_lost_combined', 'notworking_careothers','healthcare_covid', 'medication_covid', 
                         'preventative_health_covid','health_products_covid')
vax_outcomes <- c('fullvax', 'hesitancy', 'V1')

#econhealth outcomes in 3 groups
for (t in c('July2020_Nov2020', 'Dec2020_Mar2021', 'Apr2021+')){
  data_temp <- filter(fb_microdata, time_period==t)
  estimations <- doRegs(dat = data_temp, covs = c('female + age35to64 + age65plus + higher_educ + rural'),
                        outcomes = econhealth_outcomes, 
                        source = "FB symptoms", weighted = F)
  saveRDS(estimations, paste0(out.dir, 'econhealth_estimations_', t, '.Rds')) #This line was changed since 1st submission
  write.csv(estimations$cofs, paste0(out.dir, 'econhealth_estimations_', t, '_coefs_unweighted.csv')) #This line was changed since 1st submission
}

#vax outcomes in 2 groups
for (t in c('Jan2021_Mar2021', 'Apr2021+')){
  print(t)
  data_temp <- fb_microdata[time_period_vax==t]
  estimations <- doRegs(dat = data_temp, covs = c('female + age35to64 + age65plus + higher_educ + rural'),
                        outcomes = vax_outcomes, 
                        source = "FB symptoms", weighted = F)
  saveRDS(estimations, paste0(out.dir, 'vax_estimations_', t, '.Rds')) #This line was changed since 1st submission
  write.csv(estimations$cofs, paste0(out.dir, 'vax_estimations_', t, '_coefs_unweighted.csv')) #This line was changed since 1st submission
}
