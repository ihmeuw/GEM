#########################################################################################################################################################################
# 
# Purpose: Launch jobs for vaccine hesitancy, any vaccination, and full vaccination
#
#########################################################################################################################################################################

##### LAUNCH HESITANCY BY AGE-SEX SCRIPT ################################################################################################################################
rm(list=ls())

#Specifies shell to use and script.
shell  <- "/FILEPATH.sh"
script <- "/FILEPATH/01_merge_daily_hesitancy_by_age_sex.R"
#Specifying project to run on and where the output and error files to land
proj   <- "proj_gem"
job_name <- "cov_vax_hes"
output_info <- "/FILEPATH/"
error_info <- "/FILEPATH/"
mem_alloc <- 145
l_drive <- TRUE
threads <- 15
time_alloc <- "01:00:00"
queue <- "all.q"

##Launches job
command   <- paste0("qsub -l m_mem_free=", mem_alloc,
                    "G -l archive=", l_drive, 
                    " -l fthread=", threads, 
                    " -l h_rt=",time_alloc,
                    " -q ", queue, 
                    " -e ", error_info,  
                    " -o ", output_info, 
                    " -P ", proj, 
                    " -N ", job_name,
                    " ", shell, 
                    " -s ", script, 
                    " ")
system(command)


##### LAUNCH HESITANCY BY SEX SCRIPT ###################################################################################################################################
rm(list=ls())

#Specifies shell to use and script.
shell  <- "/FILEPATH.sh"
script <- "/FILEPATH/01_merge_daily_hesitancy_by_sex.R"
#Specifying project to run on and where the output and error files to land
proj   <- "proj_gem"
job_name <- "cov_vax_hes_sex"
output_info <- "/FILEPATH/"
error_info <- "/FILEPATH/"
mem_alloc <- 145
l_drive <- TRUE 
threads <- 15
time_alloc <- "01:00:00"
queue <- "all.q"

##Launches job
command   <- paste0("qsub -l m_mem_free=", mem_alloc,
                    "G -l archive=", l_drive, 
                    " -l fthread=", threads, 
                    " -l h_rt=",time_alloc,
                    " -q ", queue, 
                    " -e ", error_info,  
                    " -o ", output_info, 
                    " -P ", proj, 
                    " -N ", job_name,
                    " ", shell, 
                    " -s ", script, 
                    " ")
system(command)


##### LAUNCH ANY VACCINE SCRIPTS #######################################################################################################################################
rm(list=ls())

#Specifies shell to use and script.
shell  <- "/FILEPATH.sh"
script <- "/FILEPATH/01_merge_daily_uptake_by_sex.R"
#Specifying project to run on and where the output and error files to land
proj   <- "proj_gem"
job_name <- "cov_any_vax"
output_info <- "/FILEPATH/"
error_info <- "/FILEPATH/"
mem_alloc <- 145
l_drive <- TRUE
threads <- 15
time_alloc <- "01:00:00"
queue <- "all.q"
 
##Launches job
command   <- paste0("qsub -l m_mem_free=", mem_alloc,
                    "G -l archive=", l_drive, 
                    " -l fthread=", threads, 
                    " -l h_rt=",time_alloc,
                    " -q ", queue, 
                    " -e ", error_info,  
                    " -o ", output_info, 
                    " -P ", proj, 
                    " -N ", job_name,
                    " ", shell, 
                    " -s ", script, 
                    " ")
system(command)


##### LAUNCH FULLY VACCINATED SCRIPTS ##################################################################################################################################
rm(list=ls())

#Specifies shell to use and script.
shell  <- "/FILEPATH.sh"
script <- "/FILEPATH/01_merge_daily_full_vax_by_sex.R"
#Specifying project to run on and where the output and error files to land
proj   <- "proj_gem"
job_name <- "cov_full_vax"
output_info <- "/FILEPATH/"
error_info <- "/FILEPATH/"
mem_alloc <- 145
l_drive <- TRUE 
threads <- 15
time_alloc <- "01:00:00"
queue <- "all.q"


##Launches job
command   <- paste0("qsub -l m_mem_free=", mem_alloc,
                    "G -l archive=", l_drive, 
                    " -l fthread=", threads, 
                    " -l h_rt=",time_alloc,
                    " -q ", queue, 
                    " -e ", error_info,  
                    " -o ", output_info, 
                    " -P ", proj, 
                    " -N ", job_name,
                    " ", shell, 
                    " -s ", script, 
                    " ")
system(command)
