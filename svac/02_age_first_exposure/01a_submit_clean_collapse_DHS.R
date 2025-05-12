########################################################################################################################
# 
# Author: USERNAME
# Purpose: Submit DHS data processing code by filepath
#
########################################################################################################################
rm(list=ls())
# set-up the environment -----------------------------------------------------------------------------------------------

library(data.table)
library(stringr)
user <- Sys.getenv(x='USER')

#some arguments:
shell  <-  "FILEPATH"
script <-  'FILEPATH'

proj <- "proj_team"

output_info <- paste0('FILEPATH', user, '/output') 
error_info <- paste0('FILEPATH', user, '/errors')

time_alloc <- "5:00:00"
mem_alloc <- 25
threads <- 5
queue <- 'QUEUE'

date <- Sys.Date()

#submit job for each file in each filepath in list ------------------------------------------------------------------------

female <- T #female or male modules

basic <- fread('FILEPATH')
cb <- basic[survey_name %like% 'DHS' & survey_module %in% c("WN", "MN")]
cb$file_path <- str_replace(cb$file_path, 'pattern1', 'pattern1a')
cb$file_path <- str_replace(cb$file_path, 'pattern2', 'pattern2a')

#check for filepath issues

cb[, test := file.exists(file_path)]
cb[nid==77384 & survey_module=='WN', file_path:="FILEPATH"]
cb[nid==26866 & survey_module=='WN', file_path:="FILEPATH"]

fps <- cb$file_path
fps <- fps[str_sub(fps,30,30)=='2'] #subset to modules from 2000 or later

for (f in fps) {
  survey_name <- cb[file_path==f]$survey_name
  nid <- cb[file_path==f]$nid
  survey_module <- cb[file_path==f]$survey_module
  ihme_loc_id <- cb[file_path==f]$ihme_loc_id
  year_start <- cb[file_path==f]$year_start
  year_end <- cb[file_path==f]$year_end
  args <- paste0(f, ' ', nid, ' ', survey_module, ' ', survey_name, ' ', ihme_loc_id, ' ', year_start, ' ', year_end, ' ', date)
  
  job_name <- paste0('DHS_',nid)
  
  #launch
  command   <- paste0("sbatch",
                      " -J ", job_name,
                      " --mem ", mem_alloc, "G",
                      " -c ", threads, 
                      " -t ", time_alloc,
                      " -A ", proj,
                      " -p ", queue, 
                      " -C archive",
                      " -e ", paste0(error_info, "/%x.e%j"),
                      " -o ", paste0(output_info, "/%x.o%j"),
                      " ", shell, 
                      " -s ", script,
                      " ", args)
  system(command) 
}

