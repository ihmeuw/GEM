#####################################################################################################################################################################
#
# Cleaned Code for: Sexual violence against children before age 18
# Purpose: Create final draws by reformatting the forecasted and raked (sub)national draws
#
#####################################################################################################################################################################
rm(list=ls())

#working directory
root <- "FILEPATH"


### Set up environment ----------------------------------------------------------------------------------------------------------------------------------------------
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/utility.r")
source("FILEPATH/get_population.R")

list.of.packages <- c("data.table","stringr","ggplot2","gridExtra","parallel", "boot", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
lapply(list.of.packages, require, character.only = TRUE)

#load demographics
locs <- get_location_metadata(22, release_id =  16)
ages <- get_age_metadata(24, release_id = 16)
age21 <- data.table(age_group_id=21, age_group_name="80 plus")
ages <- rbind(ages, age21, fill=T)


### Arguments -------------------------------------------------------------------------------------------------------------------------------------------------------
female <- F
version <- "MODEL-VERSION"

if (female==T){
  risk <- "fem_csa"
  run_id <- "FEMALE-RUN-ID"
  id <- 2
} else {
  risk <- "male_csa"
  run_id <- "MALE-RUN-ID"
  id <- 1
}


### Copy files to final folder --------------------------------------------------------------------------------------------------------------------------------------

#create final folder
dir.create(paste0(root, "FILEPATH" , risk, "/", run_id, "/", version, "/"), recursive = T)

#read in, format, and save unraked draws (level 3 locs)
lvl3 <- paste0(locs[level==3]$location_id, ".csv")
for (f in lvl3) {
  print(f)
  file <- fread(paste0(root, "FILEPATH", risk, "/", run_id, "/", version, "/", f))
  if ("V1" %in% colnames(file)) {
    file$V1 <- NULL
    }
  file <- dcast(file, year_id+location_id+age_group_id~draw, value.var="mean")
  file[, sex_id:=id]
  write.csv(file, paste0(root, "FILEPATH", risk, "/", run_id, "/", version, "/", f), row.names = F)
}

#read in, format and save raked draws (above level 3)
raked <- list.files(paste0(root, "FILEPATH", risk, "/", run_id, "/", version, "/"))
i <- length(raked)
for (r in raked) {
  print(paste0(i, ": ", r))
  file <- fread(paste0(root, "FILEPATH", risk, "/", run_id, "/", version, "/", r))
  if ("V1" %in% colnames(file)) {
    file$V1 <- NULL
    }
  file[, draw:=paste0("draw_", draw)]
  file <- dcast(file, year_id+location_id+age_group_id~draw, value.var="raked_value")
  file[, sex_id:=id]
  write.csv(file, paste0(root, "FILEPATH", risk, "/", run_id, "/", version, "/", r), row.names = F)
  i <- i - 1
}
