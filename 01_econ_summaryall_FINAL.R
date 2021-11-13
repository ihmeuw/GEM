#################################################################################################
# econ_summary_plots.R
# Summary of economic variables with exception of FB symptoms data
#################################################################################################

# (1) Setup -------------------------------------------------------------------------------------
rm(list = ls())

library(data.table)
library(tidyverse)
library(ggplot2)
source('/FILEPATH/econ_functions.R')

# Important cluster functions
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
locs <- filter(locs, level == 3) #Keep only country data
run_date <- gsub('-', '_', Sys.Date())

# (2) Merge all microdatasets together ---------------------------------------------------------------------------------------------

un_women <- fread("/FILEPATH/DATASET.csv")
wb <- fread('/FILEPATH/DATASET.csv')
goalkeepers_2020 <- fread("/FILEPATH/DATASET.csv")
goalkeepers_2021 <- fread("/FILEPATH/DATASET.csv")
fbgeh <- fread('/FILEPATH/DATASET.csv')
yougov <- fread('/FILEPATH/DATASET.csv')
ipa_recovr <- fread('/FILEPATH/DATASET.csv')

#Harmonize datasets to bind them

un_women <- rename(un_women, gender = Gender, age_bin = AgeGroup)
un_women$source <- "UN women"
un_women <- un_women %>% mutate(urban= case_when(
  rural == 1 ~ "Rural",
  rural == 0 ~ "Urban"
  ))

un_women <- un_women %>% mutate(education_bin= case_when(
  higher_edu == 1  ~ "More than high school", 
  higher_edu == 0 ~ "Less than high school"
))

fbgeh$V1 <- NULL
fbgeh$line_id <- NULL
fbgeh$sex_id <- NULL
fbgeh <- rename(fbgeh, urban = urbanicity)
fbgeh$source <- "FB GE"
fbgeh <- filter(fbgeh, !is.na(ihme_loc_id))

yougov$V1 <- NULL
yougov$line_id <- NULL
yougov$source <- "Yougov"

ipa_recovr$V1 <- NULL
ipa_recovr$age <- NULL
ipa_recovr$source <- "IPA recovr"

wb$V1 <- NULL
wb$age <- NULL
wb$source <- "WB HFPS"
wb <- rename(wb, age_bin = age_simple)

goalkeepers_2020$V1 <- NULL
goalkeepers_2021$V1 <- NULL
goalkeepers_2020$observation_id <- as.integer(goalkeepers_2020$observation_id)
goalkeepers_2021$observation_id <- as.integer(goalkeepers_2021$observation_id)

non_fb_yougov <- bind_rows(un_women, fbgeh, yougov, wb, ipa_recovr, goalkeepers_2020, goalkeepers_2021)

#Create combined indicators:
#Loss income
non_fb_yougov <- non_fb_yougov %>%
  mutate(income_lost_combined = case_when(
    source == "UN women" ~ c19_inc_loss_bn,
    source == "Yougov" ~ c19_inc_loss_bn,
    source == "IPA recovr" ~ c19_inc_loss_bn,
    source == "Goalkeepers 2020" ~ inc_loss))

# Loss employment restrictive denominator
non_fb_yougov <- non_fb_yougov %>%
  mutate(emp_lost_combined = case_when(
    source == "FB GE" ~ as.integer(c19_emp_loss_perm),
    source == "Yougov" ~ as.integer(c19_emp_loss_perm),
    source == "IPA recovr" ~ as.integer(c19_emp_hr_red),
    source == "Goalkeepers 2020" ~ as.integer(emp_lost),
    source == "Goalkeepers 2021" ~ as.integer(emp_lost),
    source == "WB HFPS" ~ as.integer(emp_lost)))

 non_fb_yougov$ihme_loc_id <- NULL 
 non_fb_yougov <- left_join(non_fb_yougov, locs[, c("ihme_loc_id", "location_name")], by = "location_name")

saveRDS(non_fb_yougov, "/FILEPATH/nofb_microdata_emp_lost_updates.Rds")

# (3) Non FB or Yougov data for cross-sectional proportions --------------------------------------------------------------------------

chores_increase <- prop(non_fb_yougov,
                           wt = 'weight', 
                           var_list = c('chores_increase'),
                           group_vec = c('gender', 'source', 'location_name'))

chores_increase <- chores_increase$chores_increase
chores_increase <- rename(chores_increase, sex = gender, 
                          indicator = type, 
                          proportion = prop, 
                          data_source = source, 
                          numerator = n,
                          sample = num_obs, 
                          proportion_lower = prop_lower,
                          proportion_upper = prop_upper)

write.csv(chores_increase, "/FILEPATH/chores_increase.csv")

care_increase <- prop(non_fb_yougov,
                        wt = 'weight', 
                        var_list = c('care_increase'),
                        group_vec = c('gender', 'source', 'location_name'))

care_increase <- care_increase$care_increase
care_increase <- rename(care_increase, sex = gender, 
                          indicator = type, 
                          proportion = prop, 
                          data_source = source, 
                          numerator = n,
                          sample = num_obs, 
                          proportion_lower = prop_lower,
                          proportion_upper = prop_upper)

write.csv(care_increase, "/FILEPATH/care_increase.csv")

#World bank data has no weigths
non_fb_yougov <- non_fb_yougov %>% 
  mutate(weight = case_when(
    source != "WB HFPS" ~ weight,
    source == "WB HFPS" ~ 1))

income_lost <- prop(non_fb_yougov,
                        wt = 'weight', 
                        var_list = c('income_lost_combined'),
                        group_vec = c('gender', 'source', 'location_name'))


income_lost_combined <- income_lost$income_lost_combined
income_lost_combined <- rename(income_lost_combined, sex = gender, 
                               indicator = type, 
                               proportion = prop, 
                               data_source = source, 
                               numerator = n,
                               sample = num_obs,
                               proportion_lower = prop_lower,
                               proportion_upper = prop_upper)

write.csv(income_lost_combined, "/FILEPATH/income_lost_combined.csv")

emp_lost <- prop(non_fb_yougov,
                             wt = 'weight', 
                             var_list = c('emp_lost_combined'),
                             group_vec = c('gender', 'source', 'location_name'))

emp_lost_combined <- emp_lost$emp_lost_combined
emp_lost_combined <- rename(emp_lost_combined, sex = gender, 
                               indicator = type, 
                               proportion = prop, 
                               data_source = source, 
                               numerator = n,
                               sample = num_obs,
                               proportion_lower = prop_lower,
                               proportion_upper = prop_upper)

write.csv(emp_lost_combined, "/FILEPATH/emp_lost_combined.csv")
