#################################################################################################
# econ_regressions_weigthed.R
# Logistic regressions for economic indicators
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
source('/FILEPATH/econ_functions.R')

# Important cluster functions
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
run_date <- gsub('-', '_', Sys.Date())

# (2) Final clean up of variables for logistic regression  ---------------------------------------------------------------------------------
econ_time_nofb <- readRDS("/FILEPATH/nofb_microdata_emp_lost_updates.Rds")

# #### Covariates for Non Fb symptoms data
 
econ_time_nofb <- econ_time_nofb %>%
  mutate(female = case_when(
    gender == 'Female' ~ 1,
    gender == 'Male' ~ 0
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(higher_educ = case_when(
    education_bin %in% c('More than high school', 'College or university', 'Post graduate',
                         'Technical school', 'Post-graduate education',
                         'Some technical education (e.g polytechnic school)', 'Some university or college',
                         'Technical school diploma or degree completed', 'University or college degree completed') ~ 1,
    education_bin %in% c('Less than high school', 'High school graduate', 'High school graduate or less',
                         'No formal education', 'Primary school', 'Primary school completed', 'Secondary/high school',
                         'Secondary school/ high school completed', 'Some secondary school / high school',
                         'Some primary education') ~ 0
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(rural = case_when(
    urban == 'Rural' ~ 1,
    urban == 'Urban' ~ 0
  ))

econ_time_nofb$factortime <- as.factor(econ_time_nofb$yearmonth)

#Age bins when including goalkeepers data
econ_time_nofb <- econ_time_nofb %>%
  mutate(age_simple = case_when(
    age_bin %in% c('16-', '16-24', '16-25', '18-24', '16 to 25 years old', 'Less than 25') ~ "Less than 25",
    age_bin %in% c('25-34', '26-35', '35-44', '36-45', '26 to 35 years old', '36 to 45 years old', '25 to 45') ~ "25 to 45",
    age_bin %in% c('45-54', '46+', '55-64', '65-74', '65+', '75+', 'Over 45 years old', 'More than 45') ~ "More than 45"
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(age25less = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~1,
    age_simple == 'More than 45' ~0
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(age25to45 = case_when(
    age_simple == '25 to 45' ~ 1,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~0
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(age45more = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~1
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(age_nogoalkeepers = case_when(
    age_bin %in% c('16-', '16-24', '16-25', '18-24', '25-34', '26-35') ~ "Less than 35",
    age_bin %in% c('35-44', '36-45', '45-54', '55-64') ~ "35 to 64",
    age_bin %in% c('65-74', '65+', '75+') ~ "More than 65"
  ))

econ_time_nofb <- econ_time_nofb %>%
  mutate(
    lessthan35 = case_when(
      age_nogoalkeepers == "Less than 35" ~ 1,
      age_nogoalkeepers %in% c("35 to 64", "More than 65") ~0),
    age35to64 = case_when(
      age_nogoalkeepers == "35 to 64" ~ 1,
      age_nogoalkeepers %in% c("Less than 35", "More than 65") ~0),
    age65plus = case_when(
      age_nogoalkeepers == "More than 65" ~ 1,
      age_nogoalkeepers %in% c("Less than 35", "35 to 64") ~0)
  )

econ_time_nofb <- rename(econ_time_nofb, weight_og = weight)

econ_time_nofb <- econ_time_nofb %>%
  mutate(weight  = case_when(
    !is.na(weight_og) ~ weight_og/1000000
  ))

econ_time_nofb$weight_og <- NULL

econ_time_nofb <- econ_time_nofb %>%
  mutate(source = case_when(
    source %in% c('Goalkeepers 2020', 'Goalkeepers 2021') ~ 'Goalkeepers',
    TRUE ~ source
  ))

#Get location_id
econ_time_nofb$location_id <- NULL
econ_time_nofb <- left_join(econ_time_nofb, locs[, c("location_name", "location_id")], by = "location_name")

saveRDS(econ_time_nofb, "/FILEPATH/nonFBsymptoms_regressions_micro.Rds")

#Estimate results for non FB symptoms data at the individual level

econ_time_nofb <- econ_time_nofb %>%
  mutate(weight  = case_when(
    !is.na(weight) ~ weight/10000
  ))
# (3) Final regressions  ---------------------------------------------------------------------------------
# #Regional regressions 
  
  #Define set of covariates
  covs_chorecare <- c('female + higher_educ + rural')
  covs_un_chorecare <- c('female + age35to64 + age65plus + higher_educ + rural')
  covs_income_goalkeepers <- c('female + age25to45 + age45more + higher_educ')
  covs_income <- c('female + age35to64 + age65plus + higher_educ')
  covs_income_time <- c('female + factortime + age35to64 + age65plus + higher_educ')
  covs_income_wb <- c('female + age25to45 + age45more + higher_educ')
  covs_notworking_time <- c('female + factortime + age25to45 + age45more + higher_educ + rural')
  covs_emploss <- c('female + age35to64 + age65plus + higher_educ')
  covs_emploss_time_goalkeepers <- c('female + factortime + age25to45 + age45more + higher_educ + rural')
  covs_emploss_time <- c('female + factortime + age35to64 + age65plus + higher_educ')
  fb_emp_covs <- c('female + higher_educ + rural')
  yougov_emp_covs <- c('female + age35to64 + age65plus')
  
  #Define outcomes
  chores_both <- c('chores_increase', 'care_increase')
  inc_lost_all <- c('income_lost_combined')
  care_nw <- c('notworking_careothers')
  emp_lost <- c('emp_lost_combined')
  
  econ_time_nofb <- econ_time_nofb %>% 
    mutate(source = case_when(
      source == "IPA recovr" & location_name %ni% c("Colombia", "Côte d'Ivoire", "Sierra Leone", "Zambia") ~ "IPA recovr no trend",
      source == "IPA recovr" & location_name %in% c("Colombia", "Côte d'Ivoire", "Sierra Leone", "Zambia") ~ "IPA recovr trend",
      TRUE ~ source
    ))
  
  
  # Create list of outcomes by source
  sourceout <- list()
  sourceout[["Goalkeepers"]] <- c("inc_lost_all", "emp_lost")
  sourceout[["FB GE"]] <- c("emp_lost", "chores_both")
  sourceout[["Finmark"]] <- "inc_lost_all"
  sourceout[["UN women"]] <- c("inc_lost_all", "chores_both")
  sourceout[["Yougov"]] <- c("inc_lost_all", "emp_lost")
  sourceout[["IPA recovr no trend"]] <- c("inc_lost_all", "emp_lost")
  sourceout[["IPA recovr trend"]] <- c("inc_lost_all", "emp_lost")
  sourceout[["WB HFPS"]] <- c("emp_lost")
  
  # Create reference dataframe with source, covs and outcomes
  df_ref <- data.frame("source_ref" ="", "outcome_ref" = "")
  for (nm in names(sourceout)) {
    l <- length(sourceout[[nm]])
    for (i in 1:l) {
      r <- c("source_ref" = nm, "outcome_ref" = sourceout[[nm]][i])
      df_ref <- bind_rows(df_ref, r)
    }
  }
  df_ref <- df_ref[2:nrow(df_ref), ]
  
  # Add in covs for each source, outcome combo
  df_ref <- df_ref %>% 
    mutate(covs_ref = case_when(
      source_ref == "Goalkeepers" & outcome_ref == "inc_lost_all" ~ "covs_income_goalkeepers" ,
      source_ref == "Goalkeepers" & outcome_ref == "emp_lost" ~ "covs_emploss_time_goalkeepers" ,
      source_ref ==  "FB GE" & outcome_ref == "emp_lost" ~ "fb_emp_covs",
      source_ref ==  "FB GE" & outcome_ref == "chores_both" ~ "covs_chorecare",
      source_ref == "Finmark" & outcome_ref == "inc_lost_all" ~ "covs_income_time",
      source_ref == "UN women" & outcome_ref == "inc_lost_all" ~ "covs_income",
      source_ref == "UN women" & outcome_ref == "chores_both" ~ "covs_un_chorecare",
      source_ref == "Yougov" & outcome_ref == "inc_lost_all" ~ "yougov_emp_covs" ,
      source_ref ==  "Yougov" & outcome_ref == "emp_lost"~ "yougov_emp_covs",
      source_ref == "IPA recovr no trend" & outcome_ref == "emp_lost" ~ "covs_emploss",
      source_ref == "IPA recovr trend" & outcome_ref == "emp_lost" ~ "covs_emploss_time",
      source_ref == "IPA recovr no trend" & outcome_ref == "inc_lost_all" ~ "covs_emploss",
      source_ref == "IPA recovr trend" & outcome_ref == "inc_lost_all" ~ "covs_emploss_time",
      source_ref == "WB HFPS" & outcome_ref == "emp_lost" ~ "covs_income_wb"
    )) 
  
  #Super region regressions
  outlist <- list()
  for (src in unique(econ_time_nofb$source)) {
    ipdat <- econ_time_nofb %>%
      filter(source == src)
    for (reg in unique(ipdat$super_region_name)) {
      ipreg <- ipdat %>%
        filter(super_region_name == reg)
      if (nrow(ipreg) > 0) {
        ref <- df_ref %>%
          filter(source_ref == src)
        for (out in unique(ref$outcome_ref)) {
          cat <- vecSplit(out, "_", 1)
          cv <- ref[ref$outcome_ref == out, ]$covs_ref
          outlist[[paste0(src, "_", reg, "_", cat)]] <- doRegs(dat = ipreg, 
                                                               covs = get(cv),
                                                               outcomes = get(out),
                                                               source = src,
                                                               weighted = F)
        }
      }
      
    }
  }
  
  for (nm in names(outlist)) {
    sup_reg <- vecSplit(nm, "_", 2)
    outlist[[nm]][[2]]$super_region <- sup_reg
  }
  ip <- lapply(names(outlist), FUN = function(x) rbind(outlist[[x]][[2]]))
  outdf <- bind_rows(ip)
  
  #Regression global
  
  outlist_global <- list()
  for (src in unique(econ_time_nofb$source)) {
    ipdat <- econ_time_nofb %>%
      filter(source == src)
      ref <- df_ref %>%
        filter(source_ref == src)
    for (out in unique(ref$outcome_ref)) {
      cat <- vecSplit(out, "_", 1)
      cv <- ref[ref$outcome_ref == out, ]$covs_ref
      outlist_global[[paste0(src, "_", cat)]] <- doRegs(dat = ipdat, 
                                            covs = get(cv),
                                            outcomes = get(out),
                                            source = src,
                                            weighted = F)
    }
  }
  
  for (nm in names(outlist_global)) {
    sup_reg <- vecSplit(nm, "_", 2)
    outlist_global[[nm]][[2]]$super_region <- "All"
  }    
  ip_global <- lapply(names(outlist_global), FUN = function(x) rbind(outlist_global[[x]][[2]]))
  outdf_global <- bind_rows(ip_global)
  
  final_coefs <- bind_rows(outdf, outdf_global) %>% filter(!is.na(outcome))
  
  write.csv(final_coefs,"/FILEPATH/estimations_all_regions.csv")
