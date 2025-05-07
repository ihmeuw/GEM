####################################################################################################################################################################
# 
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Crosswalking Network Meta-analysis for SVAC alternate definitions
#
####################################################################################################################################################################

rm(list=ls())

#bundle and data versions 
f_bvid <- "Female-ID"
m_bvid <- "Male-ID"
xw_date <- "Model-Date"



##### 0. SET UP ENVIRONMENT ########################################################################################################################################

#libraries + central functions
pacman::p_load(data.table, openxlsx, dplyr, ggplot2, stringr, locfit)
library(reticulate)
reticulate::use_python("FILEPATH")
cw <- import("crosswalk")
library(crosswalk002)
library(logitnorm, lib.loc = paste0("FILEPATH"))
invisible(sapply(list.files("FILEPATH", full.names = T), source))

#directories
xwalk_output_fp <- paste0("FILEPATH", xw_date, '/')
dir.create(xwalk_output_fp, recursive = T)

#location metadata 
locs <- get_location_metadata(22, release_id = 16)



##### 1. GET + FORMAT DATA #########################################################################################################################################

#get bundle data
fdata <- get_bundle_version(f_bvid, fetch='all')
mdata <- get_bundle_version(m_bvid, fetch='all')

#bind
data <- rbind(fdata, mdata, fill=T)
bv_data <- copy(data)

#format sex information
data[sex == 'Female', sex_id := 2]
data[sex == 'Male', sex_id := 1]
setnames(data, 'val', 'mean')

#subset to vacs only
vacs_nids <- fread("FILEPATH/vacs_nids.csv")
data <- data[nid %in% vacs_nids$x]

#copy var and backfill for tabulated data
data[vargroup == '', vargroup := var]
data[, `:=` (og_var = var, og_vargroup = vargroup)]

#copy to edit
data_var <- copy(data)


# Rename and trim variable names ----------------------------------------------------------

#any csa
data_var[vargroup %in% c('any_csa_age1stexp', 'any_csa'), vargroup:='any_csa']
data_var[vargroup %in% c('any_csa_age1stexp.rperp', 'any_csa.rperp'), vargroup:='any_csa.rperp']
data_var[vargroup %in% c('any_csa_age1stexp.under18', 'any_csa.under18', 'anysex_lifetime_before18_anyperp_any_force'), vargroup:='any_csa.under18']
data_var[vargroup %in% c('any_csa_age1stexp.under18.rperp', 'any_csa.under18.rperp'), vargroup:='any_csa.under18.rperp']
data_var[vargroup %in% c('any_csa_age1stexp.under12', 'any_csa.under12'), vargroup:='any_csa.under12']
data_var[vargroup %in% c('any_csa_age1stexp.under12.rperp', 'any_csa.under12.rperp'), vargroup:='any_csa.under12.rperp']

#non contact only
data_var[vargroup %in% c('noncontact_csa_age1stexp', 'noncontact_age1stexp', 'non_contact_age1stexp', 'noncontact_only'), vargroup:='noncontact_only']
data_var[vargroup %in% c('noncontact_csa_age1stexp.rperp', 'noncontact_only.rperp'), vargroup:='noncontact_only.rperp']
data_var[vargroup %in% c('non_contact_age1stexp.under18', 'noncontact_csa_age1stexp.under18', 'noncontact_csa.under18', 'noncontact_only.under18'), vargroup:='noncontact_only.under18']
data_var[vargroup %in% c('noncontact_csa_age1stexp.under18.rperp', 'noncontact_only.under18.rperp'), vargroup:='noncontact_only.under18.rperp']
data_var[vargroup %in% c('non_contact_age1stexp.under12','noncontact_csa_age1stexp.under12', 'noncontact_only.under12'), vargroup:='noncontact_only.under12']
data_var[vargroup %in% c('noncontact_csa_age1stexp.under12.rperp', 'noncontact_only.under12.rperp'), vargroup:='noncontact_only.under12.rperp']

#copy
data <- copy(data_var)
data[, var:=vargroup]

#intercourse only group
data[var %in% c('intercourse_only', 'intercourse_only_age1stexp', 'intercourse_only_resp'), vargroup:='intercourse_only']
data[var %in% c('intercourse_only.rperp', 'intercourse_only_age1stexp.rperp', 'intercourse_only_resp.rperp'), vargroup:='intercourse_only.rperp']
data[var %in% c('intercourse_only.under18', 'intercourse_only_age1stexp.under18'), vargroup:='intercourse_only.under18']
data[var %in% c('intercourse_only.under18.rperp', 'intercourse_only_age1stexp.under18.rperp'), vargroup:='intercourse_only.under18.rperp']
data[var %in% c('intercourse_only.under12', 'intercourse_only_age1stexp.under12'), vargroup:='intercourse_only.under12']
data[var %in% c('intercourse_only.under12.rperp', 'intercourse_only_age1stexp.under12.rperp'), vargroup:='intercourse_only.under12.rperp']

#contact only group
data[var %in% c('contact_only', 'any_SV_age1stexp', 'any_sex_viol_age1stexp', 'SVage1stexp_firstsexCSA', 'any_SV_resp'), vargroup:='contact_only']
data[var %in% c('contact_only.rperp', 'any_SV_age1stexp.rperp', 'any_sex_viol_age1stexp.rperp', 'any_SV_resp.rperp'), vargroup:='contact_only.rperp']
data[var %in% c('contact_only.under12', 'any_SV_age1stexp.under12', 'any_sex_viol_age1stexp.under12', 'SVage1stexp_firstsexCSA.under12'), vargroup:='contact_only.under12']
data[var %in% c('contact_only.under12.rperp', 'any_SV_age1stexp.under12.rperp', 'any_sex_viol_age1stexp.under12.rperp'), vargroup:='contact_only.under12.rperp']
data[var %in% c('contact_only.under18', 'any_SV_age1stexp.under18', 'any_sex_viol_age1stexp.under18', 'SVage1stexp_firstsexCSA.under18', 'contactsex_lifetime_before18_anyperp_any_force_incl_attempts'), vargroup:='contact_only.under18']
data[var %in% c('contact_only.under18.rperp', 'any_SV_age1stexp.under18.rperp', 'any_sex_viol_age1stexp.under18.rperp'), vargroup:='contact_only.under18.rperp']

#first sex csa group
data[var %in% c('first_sex_csa', 'first_sex_csa_under15'), vargroup:='sexual_debut']
data[var %in% c('first_sex_csa.under12', 'first_sex_csa_under12'), vargroup:='sexual_debut.under12']
data[var %in% c('first_sex_csa.under18', 'first_sex_csa_under18'), vargroup:='sexual_debut.under18']

#sex acts only
data[var %in% c('sex_acts_only_age1stexp'), vargroup:='sex_acts_only']
data[var %in% c('sex_acts_only_age1stexp.rperp'), vargroup:='sex_acts_only.rperp']
data[var %in% c('sex_acts_only_age1stexp.under12'), vargroup:='sex_acts_only.under12']
data[var %in% c('sex_acts_only_age1stexp.under12.rperp'), vargroup:='sex_acts_only.under12.rperp']
data[var %in% c('sex_acts_only_age1stexp.under18'), vargroup:='sex_acts_only.under18']
data[var %in% c('sex_acts_only_age1stexp.under18.rperp'), vargroup:='sex_acts_only.under18.rperp']


# Extra formatting ------------------------------------------------------------------------

#age information
data[, age_start := age_start_orig]
data[, age_end := age_end_orig]

#get minimum values for zero adjustments
mean_adj_val <- quantile(data[mean!=0]$mean, probs=0.025)

#rename and drop extra columns
setnames(data, 'standard_error', 'se')
data <- data[, c('sex_id', 'age_start', 'age_end', 'ihme_loc_id', 'nid', 'year_id', 'survey_name', 'vargroup', 'mean', 'se')]

#drop subnational data points - small samples can skew final adjustments
data <- data[!ihme_loc_id %like% '_']

#adjust zeros - otherwise logit transform will fail
data[mean == 0, mean := mean_adj_val]


# Update vars to match case definition ----------------------------------------------------

#switch reference from .under15 to .under18
data[!vargroup %like% 'under' & !vargroup %like% 'rperp', vargroup := paste0(vargroup, '.under15')]
data[!vargroup %like% 'under' & vargroup %like% 'rperp', vargroup := str_replace_all(vargroup, 'rperp', 'under15.rperp')]
data[vargroup %like% 'under18', vargroup := str_replace_all(vargroup, '.under18', '')]



##### 2. CREATE WITHIN-STUDY PAIRS OF DATA #########################################################################################################################

#save an unmatched version of the data to apply adjustments to 
data_orig <- copy(data)

#get subsets of gs and alternate defs
ref_subset <- copy(data)
alts_subset <- data[!vargroup == 'contact_only']

#set names accordingly
setnames(ref_subset, c('mean', 'se', 'vargroup'), c('ref_mean', 'ref_se', 'refvar'))
setnames(alts_subset, c('mean', 'se', 'vargroup'), c('alt_mean', 'alt_se', 'altvar'))

#merge back onto each other
matched <- merge(ref_subset, alts_subset, by=c('sex_id', 'age_start', 'age_end', 'ihme_loc_id', 'nid', 'year_id', 'survey_name'), allow.cartesian = T)

#get rid of rows that match the same def
onepoint_data <- matched[altvar==refvar]
matched <- matched[!altvar==refvar]

#remove duplicate indirect comparisons (B:C == C:B)
data <- copy(matched)
alt_defs <- unique(alts_subset$altvar)
for (i in 1:length(alt_defs)){
  for (j in 1:length(alt_defs)){
    data[refvar==alt_defs[i] & altvar==alt_defs[j], comparison_pair:=paste0(alt_defs[i], ' to ', alt_defs[j])]
    data[refvar==alt_defs[j] & altvar==alt_defs[i], comparison_pair:=paste0(alt_defs[i], ' to ', alt_defs[j])]
  }
}

#drop duplicates and points with missing information
data[!is.na(comparison_pair), duplicate_pair:=duplicated(comparison_pair), by=c('nid', 'age_start', 'age_end')]
data <- data[is.na(duplicate_pair) | duplicate_pair==FALSE]
data[, comparison_pair:=NULL]
data[, duplicate_pair:=NULL]
matched <- copy(data[!is.na(alt_se) & !is.na(ref_se)])

#get logit calcs using the delta transform package
logit_alt_means <- as.data.table(delta_transform(mean=matched$alt_mean, sd=matched$alt_se, transformation='linear_to_logit'))
setnames(logit_alt_means, c('mean_logit', 'sd_logit'), c('logit_alt_mean', 'logit_alt_se'))
logit_ref_means <- as.data.table(delta_transform(mean=matched$ref_mean, sd=matched$ref_se, transformation='linear_to_logit'))
setnames(logit_ref_means, c('mean_logit', 'sd_logit'), c('logit_ref_mean', 'logit_ref_se'))

#bind back onto main data table
matched <- cbind(matched, logit_alt_means)
matched <- cbind(matched, logit_ref_means)

#calculate diff
matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = matched, 
  alt_mean = "logit_alt_mean", alt_sd = "logit_alt_se",
  ref_mean = "logit_ref_mean", ref_sd = "logit_alt_se" )

#add sex coviariate
matched[, sex_cov:=ifelse(sex_id==1, 1, 0)]



##### 3. SPECIFIC MODEL PARAMETERS + FIT CROSSWALK MODEL ###########################################################################################################

df <- CWData(
  df = matched,             # dataset for metaregression
  obs = "logit_diff",       # column name for the observation mean
  obs_se = "logit_diff_se", # column name for the observation standard error
  alt_dorms = "altvar",     # column name of the variable indicating the alternative definition
  ref_dorms = "refvar",     # column name of the variable indicating the reference definition
  dorm_separator = '.',
  covs = list(),
  study_id = 'nid',         # name of the column indicating group membership (matching groups)
  add_intercept = TRUE
)


fit_csa_network <- CWModel(
  cwdata = df,             # object returned by `CWData()`
  obs_type = "diff_logit", # diff_logit with bounds [0, 1]
  cov_models = list(       # specifying predictors in the model
    CovModel("intercept")),
  gold_dorm = "contact_only",  # the level of `ref_dorms` that indicates it's the gold standard
  order_prior = list(c('contact_only', 'any_csa'), c('intercourse_only', 'contact_only')),
  max_iter=100L, # default
  inlier_pct=0.9 # trimming == 10%
)



##### 4. SAVE THE FINAL CROSSWALK MODEL ############################################################################################################################

#model name
version <- 'VERSION-NAME'

#save model object
py_save_object(object = fit_csa_network, filename = paste0(xwalk_output_fp, version, '_modelobj', '.pkl'), pickle = "dill")




