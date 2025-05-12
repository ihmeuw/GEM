####################################################################################################################################################################
# 
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Run ST-GPR model on SVAC data
#
####################################################################################################################################################################

rm(list=ls())

#toggle sex
female <- F


### Set male/female options ----------------------------------------------------------------------------------------------------------------------------------------

if (female==T) {
  model_id <- "CONFIG-ID"
  model_name <- "fem_csa"
  
} else {
  model_id <- "CONFIG-ID"
  model_name <- "male_csa"

}


### Run ST-GPR model -----------------------------------------------------------------------------------------------------------------------------------------------

#functions
reticulate::use_python("FILEPATH")
source("FILEPATH/public.R")

#register model
run_id <- register_stgpr_model(paste0("FILEPATH", model_name, "_stgpr_config_for_sdgs.csv"), model_index_id = model_id)

#launch model
stgpr_sendoff(run_id, "proj_team")




