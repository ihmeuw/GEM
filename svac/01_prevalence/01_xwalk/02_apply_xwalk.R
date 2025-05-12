####################################################################################################################################################################
# 
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Crosswalking Network Meta-analysis for SVAC alternate definitions
#
####################################################################################################################################################################

rm(list=ls())

female <- F
current_year <- T
xw_date <- "MODEL-DATE"
release_id_num <- 16


##### 0. SET UP ENVIRONMENT ########################################################################################################################################

#libraries + central functions
pacman::p_load(data.table, openxlsx, dplyr, ggplot2, stringr, locfit)
library(reticulate)
reticulate::use_python("FILEPATH")
cw <- import("crosswalk")
library(crosswalk002)
library(logitnorm, lib.loc = paste0("FILEPATH"))
invisible(sapply(list.files("FILEPATH", full.names = T), source))

#custom function - check for missing nids
check <- function(df) {
  #check to make sure nothing is missing
  og[!nid %in% df$nid]$nid %>% unique()
}

#model information
if (female==T){
  model_sex <- "fem_csa"
  bv <- "FEMALE-ID"

} else { 
  model_sex <- "male_csa"
  bv <- "MALE-ID"

}

#location and age metadata
loc_dt <- get_location_metadata(location_set_id = 22, release_id = release_id_num)
age_dt <- get_age_metadata(24, release_id = release_id_num)
age_dt[, age_group_years_end := age_group_years_end - 1]
age_dt[age_group_id == 235, age_group_years_end := 99]


##### 1. GET + FORMAT DATA #########################################################################################################################################

#load data
bv_data <- get_bundle_version(bv, fetch="all")

#save copy to check
og <- copy(bv_data)
og_nids <- unique(bv_data$nid)


# Rename and trim variable names ----------------------------------------------------------

#copy var to check
bv_data[, og_var := var]

#change age threshold tag
bv_data[var %like% "_under", var := str_replace(var, "_under", ".under")]
bv_data[var %like% "_before", var := str_replace(var, "_before", ".under")]

#drop age threshold tag if age not given
bv_data[var %like% ".under$", var := str_replace(var, ".under", "")]

#remove below 15 because that is GS
bv_data[var %like% ".under15", var := str_replace(var, ".under15", "")]

#remove force types + recall period
bv_data[var %like% "_any_force", var := str_replace(var, "_any_force", "")]
bv_data[var %like% "_incl_attempts", var := str_replace(var, "_incl_attempts", "")]
bv_data[var %like% "_attempts_only", var := str_replace(var, "_attempts_only", "")]
bv_data[var %like% "_lifetime", var := str_replace(var, "_lifetime", "")]
bv_data[var %like% "_coerce_force", var := str_replace(var, "_coerce_force", "")]

#reset any adult or any perp to non-restricted csa
bv_data[var %like% "_anyperp", var := str_replace(var, "_anyperp", "")]
bv_data[var %like% "_anyadult", var := str_replace(var, "_anyadult", "")]

#set rperp
bv_data[var %like% "_nonpart", var := str_replace(var, "_nonpart", ".rperp")]
bv_data[var %like% "_relative", var := str_replace(var, "_relative", ".rperp")]
bv_data[var %like% "_anypart", var := str_replace(var, "_anypart", ".rperp")]
bv_data[var %like% "_stranger", var := str_replace(var, "_stranger", ".rperp")]
bv_data[var %like% "_knownperp", var := str_replace(var, "_knownperp", ".rperp")]
bv_data[var %like% "_caregiver", var := str_replace(var, "_caregiver", ".rperp")]
bv_data[var %like% "_familyperp", var := str_replace(var, "_familyperp", ".rperp")]
bv_data[var %like% "_partnerperp", var := str_replace(var, "_partnerperp", ".rperp")]
bv_data[var %like% "_rperp", var := str_replace(var, "_rperp", ".rperp")]

#rename abuse types
bv_data[var %like% "intsex", var := str_replace(var, "intsex", "intercourse_only")]
bv_data[var %like% "contactsex", var := str_replace(var, "contactsex", "contact_only")]
bv_data[var %like% "anysex", var := str_replace(var, "anysex", "any_csa")]
bv_data[var %like% "any_sex", var := str_replace(var, "any_sex", "any_csa")]
bv_data[var %in% c("csa"), var := "any_csa"]


# Rename VACS vars that don"t match -------------------------------------------------------

#any csa
bv_data[vargroup %in% c("any_csa_age1stexp", "any_csa"), vargroup:="any_csa"]
bv_data[vargroup %in% c("any_csa_age1stexp.rperp", "any_csa.rperp"), vargroup:="any_csa.rperp"]
bv_data[vargroup %in% c("any_csa_age1stexp.under18", "any_csa.under18"), vargroup:="any_csa.under18"]
bv_data[vargroup %in% c("any_csa_age1stexp.under18.rperp", "any_csa.under18.rperp"), vargroup:="any_csa.under18.rperp"]
bv_data[vargroup %in% c("any_csa_age1stexp.under12", "any_csa.under12"), vargroup:="any_csa.under12"]
bv_data[vargroup %in% c("any_csa_age1stexp.under12.rperp", "any_csa.under12.rperp"), vargroup:="any_csa.under12.rperp"]

#non contact only
bv_data[vargroup %in% c("noncontact_csa_age1stexp", "noncontact_age1stexp", "non_contact_age1stexp", "noncontact_only"), vargroup:="noncontact_only"]
bv_data[vargroup %in% c("noncontact_csa_age1stexp.rperp", "noncontact_only.rperp"), vargroup:="noncontact_only.rperp"]
bv_data[vargroup %in% c("non_contact_age1stexp.under18", "noncontact_csa_age1stexp.under18", "noncontact_csa.under18", "noncontact_only.under18"), vargroup:="noncontact_only.under18"]
bv_data[vargroup %in% c("noncontact_csa_age1stexp.under18.rperp", "noncontact_only.under18.rperp"), vargroup:="noncontact_only.under18.rperp"]
bv_data[vargroup %in% c("non_contact_age1stexp.under12","noncontact_csa_age1stexp.under12", "noncontact_only.under12"), vargroup:="noncontact_only.under12"]
bv_data[vargroup %in% c("noncontact_csa_age1stexp.under12.rperp", "noncontact_only.under12.rperp"), vargroup:="noncontact_only.under12.rperp"]

#rename and adjust zero vals
setnames(bv_data, "val", "mean")
mean_adj_val <- quantile(bv_data[mean>0.001]$mean, probs=0.025)
bv_data[mean<=mean_adj_val, mean:=mean_adj_val]

#copy to edit
data <- copy(bv_data)


# Create vargroup -------------------------------------------------------------------------

#intercourse only group
data[var %in% c("intercourse_only", "intercourse_only_age1stexp", "intercourse_only_resp"), vargroup:="intercourse_only"]
data[var %in% c("intercourse_only.rperp", "intercourse_only_age1stexp.rperp", "intercourse_only_resp.rperp"), vargroup:="intercourse_only.rperp"]
data[var %in% c("intercourse_only.under18", "intercourse_only_age1stexp.under18"), vargroup:="intercourse_only.under18"]
data[var %in% c("intercourse_only.under18.rperp", "intercourse_only_age1stexp.under18.rperp"), vargroup:="intercourse_only.under18.rperp"]
data[var %in% c("intercourse_only.under12", "intercourse_only_age1stexp.under12"), vargroup:="intercourse_only.under12"]
data[var %in% c("intercourse_only.under12.rperp", "intercourse_only_age1stexp.under12.rperp"), vargroup:="intercourse_only.under12.rperp"]

#contact only group
data[var %in% c("contact_only", "any_SV_age1stexp", "any_sex_viol_age1stexp", "SVage1stexp_firstsexCSA", "any_SV_resp"), vargroup:="contact_only"]
data[var %in% c("contact_only.rperp", "any_SV_age1stexp.rperp", "any_sex_viol_age1stexp.rperp", "any_SV_resp.rperp"), vargroup:="contact_only.rperp"]
data[var %in% c("contact_only.under12", "any_SV_age1stexp.under12", "any_sex_viol_age1stexp.under12", "SVage1stexp_firstsexCSA.under12"), vargroup:="contact_only.under12"]
data[var %in% c("contact_only.under12.rperp", "any_SV_age1stexp.under12.rperp", "any_sex_viol_age1stexp.under12.rperp"), vargroup:="contact_only.under12.rperp"]
data[var %in% c("contact_only.under18", "any_SV_age1stexp.under18", "any_sex_viol_age1stexp.under18", "SVage1stexp_firstsexCSA.under18"), vargroup:="contact_only.under18"]
data[var %in% c("contact_only.under18.rperp", "any_SV_age1stexp.under18.rperp", "any_sex_viol_age1stexp.under18.rperp"), vargroup:="contact_only.under18.rperp"]

#non-contact and any_csa can stay the same
data[var %like% "any_csa" | var %like% "non", vargroup:=var]

#first sex csa group
data[var %in% c("first_sex_csa", "first_sex_csa.rperp"), vargroup:="sexual_debut"]
data[var %in% c("first_sex_csa.under12", "first_sex_csa.under12.rperp"), vargroup:="sexual_debut.under12"]
data[var %in% c("first_sex_csa.under18", "first_sex_csa.under18.rperp"), vargroup:="sexual_debut.under18"]

#sex acts only
data[var %in% c("sex_acts_only_age1stexp"), vargroup:="sex_acts_only"]
data[var %in% c("sex_acts_only_age1stexp.rperp"), vargroup:="sex_acts_only.rperp"]
data[var %in% c("sex_acts_only_age1stexp.under12"), vargroup:="sex_acts_only.under12"]
data[var %in% c("sex_acts_only_age1stexp.under12.rperp"), vargroup:="sex_acts_only.under12.rperp"]
data[var %in% c("sex_acts_only_age1stexp.under18"), vargroup:="sex_acts_only.under18"]
data[var %in% c("sex_acts_only_age1stexp.under18.rperp"), vargroup:="sex_acts_only.under18.rperp"]

#trim
data[vargroup %like% "rperp", new_vargroup:=str_sub(vargroup,1,-7)]
data[!vargroup %like% "rperp", new_vargroup:=vargroup]
data[, vargroup:=new_vargroup]
data[, new_vargroup:=NULL]
data[vargroup %like% "under", new_vargroup:=str_sub(vargroup,1,-9)]
data[!vargroup %like% "under", new_vargroup:=vargroup]
data[, vargroup:=new_vargroup]
data[, new_vargroup:=NULL]


# Update vars to match case definition ----------------------------------------------------

#switch reference from .under15 to .under18
data[!var %like% "under" & !var %like% "rperp", var := paste0(var, ".under15")]
data[!var %like% "under" & var %like% "rperp", var := str_replace_all(var, "rperp", "under15.rperp")]
data[var %like% "under18", var := str_replace_all(var, ".under18", "")]

#add age tagback on
data[var %like% "under12", vargroup := paste0(vargroup, ".under12")]
data[var %like% "under15", vargroup := paste0(vargroup, ".under15")]



##### 2. ADJUST ALTERNATIVE DEFINITIONS ############################################################################################################################

#read in xwalk model
csa_network <- py_load_object(filename = paste0("FILEPATH", xw_date, "/vacs_data_sex_neutral_modelobj", ".pkl"), pickle = "dill")

#split into data used for modeling that needs alternate definition adjustments (ie, not contact-only-under18)
data_to_adjust <- data[(group_review==1 | is.na(group_review)) & !vargroup=="contact_only"]
data_unadj <- data[(group_review==1 | is.na(group_review)) & vargroup=="contact_only"]

#create data id for merge:
data_to_adjust[, data_id:=1:.N]

#crosswalk
preds <- adjust_orig_vals(
  fit_object = csa_network, # object returned by `CWModel()`
  df = data_to_adjust,
  orig_dorms = "vargroup",
  orig_vals_mean = "mean",
  orig_vals_se = "standard_error", 
)

#add the adjusted values back to the original dataset
data_to_adjust[, c("meanvar_adjusted", "sdvar_adjusted", "adjustment_logit", "adjustment_se_logit", "data_id")] <- preds

#add note to adjusted points
data_to_adjust[, note_modeler := paste0(note_modeler, " | applied xwalk for alternate definition")]

#set crosswalk parent seq and seq
data_to_adjust[!vargroup == "contact_only", crosswalk_parent_seq := seq]

#set adjusted means to mean
setnames(data_to_adjust, c("mean", "standard_error", "meanvar_adjusted", "sdvar_adjusted"), c("mean_unadj", "standard_error_unadj", "mean", "standard_error"))

#clear out unadjusted se information
data_to_adjust[, c("upper", "lower", "variance") := ""]

#re-calculate variance
data_to_adjust[, variance := as.numeric(standard_error)^2]

#bind adjusted and unadjusted data
data_to_save <- rbind(data_to_adjust, data_unadj, fill=T)



##### 3. SAVE ADJUSTED DATA ########################################################################################################################################

#create dir
dir.create(paste0("FILEPATH"), recursive = T)

#save
write.xlsx(data_to_save, paste0("FILEPATH", xw_date, "/", model_sex, "_bv", bv, "_2022xwalkadj_sexneutraladj_norperp_noagelimit_double_adjust.xlsx"), sheetName = "extraction")


