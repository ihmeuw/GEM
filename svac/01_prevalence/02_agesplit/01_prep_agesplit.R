####################################################################################################################################################################
# 
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Prepare SVAC data for age-splitting by (1) creating training and testing data sets and (2) running an ST-GPR model on the test set
#
####################################################################################################################################################################

rm(list=ls())
pacman::p_load(data.table, dplyr, openxlsx)



# Set male/female options ------------------------------------------------------------------------------------------------------------------------------------------

female <- T
release_id_num <- 16
date <- "Model-Date"

if (female==T){
  bv <- "FEMALE-ID"
  model_name <- "fem_csa"
  id <- 2
} else {
  bv <-  "MALE-ID"
  model_name <- "male_csa"
  id <- 1
}


# Functions and metadata -------------------------------------------------------------------------------------------------------------------------------------------

#central functions
invisible(sapply(list.files("FILEPATH", full.names = T), source))

#custom function to fill out mean/cases/sample sizes
get_cases_sample_size <- function(raw_dt){
  dt <- copy(raw_dt)
  dt[is.na(mean), mean := cases/sample_size]
  dt[is.na(cases) & !is.na(sample_size), cases := mean * sample_size]
  dt[is.na(sample_size) & !is.na(cases), sample_size := cases / mean]
  return(dt)
}

#custom function to get cases if they are missing
calculate_cases_fromse <- function(raw_dt){
  dt <- copy(raw_dt)
  dt[is.na(cases) & is.na(sample_size), sample_size := (mean*(1-mean)/standard_error^2)]
  dt[is.na(cases), cases := mean * sample_size]
  return(dt)
}

#location metadata
locs <- get_location_metadata(22, release_id = release_id_num)

#age metadata
ages <- get_age_metadata(24, release_id = release_id_num)
ages[, age_group_years_end:=age_group_years_end-1]
ages[age_group_id==235, age_group_years_end:=99]
setnames(ages, c('age_group_years_start', 'age_group_years_end'), c('age_start', 'age_end'))



# Read in + format input data -------------------------------------------------------------------------------------------------------------------------------------

#get crosswalked data
bv_data <- as.data.table(read.xlsx(paste0("FILEPATH", xw_date, '/', model_name, '_bv', bv, '_2022xwalkadj_sexneutraladj_norperp_noagelimit_double_adjust.xlsx')))

#sex info
bv_data[sex=='Female', sex_id:=2]
bv_data[sex=='Male', sex_id:=1]
bv_data <- bv_data[sex_id==id]

#copy to edit
data <- copy(bv_data)

#drop data that is only used for xwalking definitions (group_review==0)
data <- data[is.na(group_review) | group_review==1]

#only keep 10+ year olds
data <- data[age_start > 9]

#divide data into test and train to be used for ST-GPR 
train <- data[age_group_id != 999]
test <- fsetdiff(data,train)


# Aggregate older ages --------------------------------------------------------------------------------------------------------------------------------------------

#aggregate age bins greater than 80; reassign to age_group_id 21
combine <- train[age_group_id>20]

#subset to nids that have multiple age bands
combine <- combine[, num_age_bins:=length(unique(age_group_id)), by='nid']

#aggregate cases/ss 
combine <- calculate_cases_fromse(combine)
combine <- get_cases_sample_size(combine)
combine[, `:=` (comb_cases=sum(cases), comb_sample_size=sum(sample_size)), by='nid']
combine[, comb_mean:=comb_cases/comb_sample_size, by='nid'] #calculate combined mean
z <- qnorm(0.975)
combine[, comb_se:=sqrt(comb_mean*(1-comb_mean)/comb_sample_size + z^2/(4*comb_sample_size^2))] #calculate combined se
combine[, `:=` (comb_age_start=min(age_start), comb_age_end=max(age_end)), by='nid'] #create correct age_start and age_end of the combined band
combine <- combine[!duplicated(combine[,c('nid', 'comb_age_start', 'comb_age_end', 'comb_mean')]),] #remove what are now duplicates, should have the same number of rows as combine_nids

#format
combine[, c('mean', 'standard_error', 'variance' ,'cases', 'sample_size', 'age_start', 'age_end'):=NULL] #drop old fields
setnames(combine, c('comb_mean', 'comb_se', 'comb_cases', 'comb_sample_size', 'comb_age_start', 'comb_age_end'), 
         c('mean', 'standard_error', 'cases', 'sample_size', 'age_start', 'age_end')) #rename created fields to standard
combine[, variance:=standard_error^2]
combine[, age_group_id:=21]

#drop these nids from train, and rbind the combine dt in their place
train <- train[!(nid %in% unique(combine$nid) & age_group_id>20)]
train <- rbind(train, combine, fill=T)



# Outliers and final prep -----------------------------------------------------------------------------------------------------------------------------------------

#outlier error var
train[var=='resp.rperp', is_outlier := 1]

#format train data for stgpr
col.stgpr <- c("nid","location_id","year_id","age_group_id","sex_id","val","sample_size","variance","measure","is_outlier", "measure_id")

#copy and rename
train.prep <- copy(train)
setnames(train.prep, 'mean', 'val')

#calculate variance
train.prep[!is.na(standard_error), variance:=standard_error^2]

#create measure id
train.prep[, measure_id := 18]
train.in.stgpr <- train.prep[, col.stgpr, with = F]

#apply historical outliers
train.in.stgpr[nid %in% c("STUDYIDS"), is_outlier := 1]

#sex-specific outliers
if (female) {
  train.in.stgpr[nid %in% c("STUDYIDS"), is_outlier := 1]
  
} else {
  train.in.stgpr[nid %in% c("STUDYIDS"), is_outlier := 1]
  
}

#re-assert sex information
train.in.stgpr[, sex := ifelse(sex_id==1, 'Male', 'Female')]

#save training and test data sets
output_dir <- paste0("FILEPATH")
dir.create(output_dir, recursive = T)
write.csv(train.in.stgpr, paste0("FILEPATH", 'bv_', bv, '_', model_name, '_stgpr_train_model_data80plus_sexneutralxwalk_norperp_noagelimit.csv'))
write.csv(test, paste0("FILEPATH", 'bv_', bv, '_', model_name, '_STGPRagesplit_2020testdata_norperp_noagelimit.csv'))


# Run ST-GPR model on the train data -----------------------------------------------------------------------------------------------------------------------------

source("FILEPATH/public.R")
run_id <- register_stgpr_model(paste0("FILEPATH", model_name, '_stgpr_agesplit_config.csv'))
stgpr_sendoff(run_id, 'PROJECT-NAME')
 


