####################################################################################################################################################################
# 
# Author: USERNAME
# Purpose: Clean and collapse DHS microdata extractions 
#
####################################################################################################################################################################

rm(list=ls())

library(data.table)
library(readstata13)
library(readr)
library(magrittr)
library(stringr)
library(binom)
library(Hmisc)
library(DBI)
library(RMySQL)
library(survey)
library(haven)
library(knitr)
library(dplyr)
library(ini)
library(bit64)

jpath <- "FILEPATH"
user  <- Sys.getenv(x="USER")
source('FILEPATH/db_tools.r')
source('FILEPATH/collapse.r')

# basic set up before beginning extraction -----------------------------------------------------------------------------------------------------------------------------------------------

if (interactive()){ #if testing
  basic <- fread('FILEPATH')
  cb <- basic[survey_name %like% 'DHS' & survey_module=='WN']
  cb$file_path <- str_replace(cb$file_path, 'pattern1', 'pattern2')
  cb[nid==77384 & survey_module=='WN', file_path:="FILEPATH"]
  f <-  "FILEPATH"
  survey_name <- cb[file_path==f]$survey_name
  nid <- cb[file_path==f]$nid
  survey_module <- cb[file_path==f]$survey_module
  ihme_loc_id <- cb[file_path==f]$ihme_loc_id
  year_start <- cb[file_path==f]$year_start
  year_end <- cb[file_path==f]$year_end
  version <- ifelse(survey_module=='WN', 'female', 'male')
  date <- 'test'
  
} else { # Else script will run given arguments from submit script 
  f <- commandArgs(trailingOnly = T)[1]
  nid <- commandArgs(trailingOnly = T)[2]
  survey_module <- commandArgs(trailingOnly = T)[3]
  survey_name <- commandArgs(trailingOnly = T)[4]
  ihme_loc_id <- commandArgs(trailingOnly = T)[5]
  year_start <- commandArgs(trailingOnly = T)[6]
  year_end <- commandArgs(trailingOnly = T)[7]
  date <- commandArgs(trailingOnly = T)[8]
}

# set sex-conditional information
version <- ifelse(survey_module=='WN', 'female', 'male')
id <- ifelse(version=='female', 2, 1)
vstem <- ifelse(version=='female', 'v', 'mv')
dstem <- ifelse(version=='female', 'd', 'md')

#set dirs
out_dir  <- 'FILEPATH'

#get GBD subnatmap
subnat_map <- fread('FILEPATH')[, c('ihme_loc_id', 'indicator', 'value', 'location_name_short_ihme_loc_id')]
subnat_map[, c("location_name_short", "admin_1_id") := tstrsplit(location_name_short_ihme_loc_id, "|", fixed=TRUE)]
setnames(subnat_map, 'ihme_loc_id', 'ihme_loc_id_nat')
subnat_map <- subnat_map[ihme_loc_id_nat==ihme_loc_id]

#set up some relevant vars
recall_periods <- c('lifetime', 'pastyr')
basic_vars <- c(paste0(vstem, '022'), paste0(vstem, '001'), paste0(vstem, '005'), paste0(vstem, '002'), paste0(vstem, '003'), paste0(vstem, '012'), paste0(vstem, '025'), paste0(vstem, '024'), paste0(vstem, '133'))
basic_var_names <- c('strata', 'psu', 'pweight', 'hh_id', 'line_id', 'age_year', 'urban', 'admin_1', 'education')
marital_control <- paste0(dstem, '101', letters[1:6])
phys <- paste0(dstem, '105', letters[c(1:7,10)])
sex <- paste0(dstem, '105', letters[c(8,9,11)])
psych <- paste0(dstem, '103', letters[1:6])
prevpart <- paste0(dstem, '130', letters[1:3])
pregvars <- paste0(vstem, '2', c('01', '13', '28'))
inj <- paste0(dstem, '110', letters[1:4])
dv_vars <- c(paste0(vstem, '044'), paste0(vstem, '502'), paste0(vstem,'503'), paste0(dstem,'005'), paste0(dstem, '115y'), paste0(dstem,'117a'), paste0(dstem,'118y'),
             paste0(dstem,'12', 4:7), marital_control, phys, sex, psych, prevpart)
if (version=='female'){
  dv_vars <- c(dv_vars, pregvars)
}
wife_beating <- paste0(vstem, '744', letters[1:5])
sex_attitudes <- paste0(vstem, '850', letters[1:2])

#read in data and set up meta vars
dt <- as.data.table(read.dta13(f, convert.factors=F))
names(dt) <- tolower(names(dt)) #standardize to lowercase
dt[, `:=` (nid=nid, survey_module=survey_module, ihme_loc_id=ihme_loc_id, year_start=year_start, year_end=year_end, survey_name=survey_name, file_path=f, sex_id=id)]

#merge on admin1 strings and map subnats, if modeled subnationally
if (nrow(subnat_map)>0){
  admin_1_id <- as.data.table(read.dta13(f))
  names(admin_1_id) <- tolower(names(admin_1_id))
  setnames(admin_1_id, paste0(vstem, '024'), 'admin1_str')
  admin_1_vars <- c(paste0(vstem, '00', c('1', '5', '2', '3')), 'admin1_str')
  dt <- merge(dt, admin_1_id[, (admin_1_vars), with=F], by=c(paste0(vstem, '00', c('1', '5', '2', '3'))))
  dt <- merge(dt, subnat_map, by.x='admin1_str', by.y='value', all.x=TRUE, allow.cartesian = TRUE)
} else {
  dt[, `:=` (admin_1_id=NA, location_name_short=NA)]
}

#standardize variable naming
setnames(dt, basic_vars, basic_var_names)

#save original copy
orig <- copy(dt)

#now look for violence-specific information
if (paste0(vstem, '044') %in% names(dt) & paste0(dstem, '005') %in% names(dt)){ 
  test_var <- paste0(dstem, '105a')
  selec <- paste0(vstem, '044')
  if (nrow(dt[!is.na(get(test_var))])>0){ # filter out modules with dummy variables for the standard DV naming
    dt <- dt[get(selec)==1] #subset to those successfully interviewed for DV module
    
    #create target population vars
    dt[, epart:=ifelse(eval(parse(text=paste0(vstem, '502'))) %in% c(1,2), 1, 0)]
    dt[, currpart:=ifelse(eval(parse(text=paste0(vstem, '502')))==1, 1, 0)]
    dt[, multpart:=ifelse(eval(parse(text=paste0(vstem, '503')))==2, 1, 0)]
    if (version == 'female'){
      dt[, epreg:=ifelse(v201>0 | v213==1 | v228==1,1,0)]
    }
    
    #replace pweight with dv-specific pweight
    weight_var <- paste0(dstem, '005')
    dt[, pweight:=get(weight_var)]
    dt[, paste0(dstem, '005'):=NULL]
    
    #binary event variable
    sv_vars <- c('d105h', 'd105i', 'd105k', 'd130b', 'd124', 'd125')
    dt[, ever_sv:=0]
    # dt[, ever_sv:=0]
    for (s in sv_vars){
      if (s %in% names(dt)){
        dt[get(s) %in% 1:4, paste0(s):=1]
      } else {dt[, paste0(s):=NA]}
    }
    
    dt[d105h==1 | d105i==1 | d105k==1 | d130b==1 | d124==1 | d125==1, ever_sv:=1]
    dt[ever_sv!=1 & (is.na(d105h) | is.na(d105i) | is.na(d105k) | is.na(d130b) | is.na(d124) | is.na(d125)), ever_sv:=NA]
    
    #fix if ever_sv==0 but age at first exp filled in
    dt[ever_sv==0 & !is.na(d126), ever_sv:=1]
    
    if ('d126' %in% names(dt) & length(unique(dt$d126))!=1){

    
      dt[ever_sv==1 & is.na(d126), d126:=99]
      
      
      dt[d126<0 | d126>age_year, d126:=99] # reassign when implausible values in d126
      
      #retain measure of missingness in d126
      age_first_exp_missingness <- dim(dt[d126==99])[1]/dim(dt[!is.na(d126)])[1]
      dt[, age_first_exp_missingness:=age_first_exp_missingness]
      
      dt <- dt[v044==1]
      dt[, csa:=0]
      dt[d126 <= 18, csa:=1]
      
      setnames(dt, 'd126', 'age_first_exp')
      
    } else { print('No age first exp information available in DV module')}
    
    #drop unnecessary columns
    vars <- c('ever_sv', 'age_first_exp', basic_var_names, 'admin_1_id', 'location_name_short',
              "nid", "survey_name", "ihme_loc_id", "year_start", "year_end", "survey_module", "file_path", "sex_id", "age_first_exp_missingness")
    dt <- dt[, (vars), with=F]
    
    #save cleaned microdata file
    dir.create(paste0(out_dir, '/dhs/01_cleaned/', ihme_loc_id), recursive=T)
    write.csv(dt, paste0(out_dir, '/dhs/01_cleaned/', ihme_loc_id, '/', survey_name, '_', nid, '_', survey_module, '_', ihme_loc_id, '_', year_start, '_', year_end, '.csv'))
    
  } else {
    print('DT does not have any real observations for DV vars, check for non-standard naming')
  }
} else {
  print('DT does not contain DV information, or does not follow standard DV variable naming rules')
}
