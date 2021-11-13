#################################################################################################################################################################################
# Purpose: Read + format WBHFPS data for GEM's economic indicators
# Date: September 24, 2021
#################################################################################################################################################################################

rm(list=ls())

#create filepaths
fp <- "/FILEPATH/"

output_dir <- paste0('/FILEPATH/', Sys.Date(), '/')
dir.create(output_dir, recursive = T, showWarnings = T)


##### 0. SET UP #################################################################################################################################################################

#libraries
pacman::p_load(data.table, dplyr, readstata13, readr, stringr, plyr)

#function to loop through and merge files
read_draw <- function(c.file){
  dat <- as.data.table(read.dta13(c.file, nonint.factors = T))
  return(dat)
}

#functions to find 'not'
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

#function to calculate employment loss
get_emp_lost <- function(df){
  df[emp_1 == 1 & emp_2 == 0, emp_lost := 1]
  df[emp_2 == 1, emp_lost := 0]
  return(df)
}


##### 1. GET + FORMAT DATA ######################################################################################################################################################

#---- ARM -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
arm_hfps <- c("/FILEPATH/")

#get data
arm <- read_draw(paste0(fp, arm_hfps))

#drop extra columns
arm <- arm[, c('uniq_id', 'datestamp', 'q1', 'q2', 'q7', 'q8', 'q12r1', colnames(arm)[colnames(arm) %like% 'q12r6'], 'q12r7_1'), with = F]

#rename columns
setnames(arm, 'q1', 'age')
setnames(arm, 'q2', 'sex')
setnames(arm, 'q7', 'urban_rural')
setnames(arm, 'q8', 'education')

#edit ages
arm[age < 25, age_simple := 'Less than 25']
arm[age >= 25 & age < 45, age_simple := '25 to 45']
arm[age >= 45, age_simple := 'More than 45']

#create education bins
arm[education == 'Secondary vocational (technical school, college)' | 
      education == 'Incomplete higher education' |
      education == 'Higher education (bachelor, masters)' | 
      education == 'Academic (postgraduate)', education_bin := 1]
arm[education == 'Secondary' | 
      education == 'Basic' | 
      education == 'Primary vocational' |
      education == 'Primary' | 
      education == 'Does not have primary education and not literate' | 
      education == 'Does not have primary education but literate', education_bin := 0]

#fix urbanicity
arm[urban_rural %like% 'Urban' | urban_rural %like% 'Yerevan', urban_rural := 'Urban']
arm[urban_rural %like% 'Rural', urban_rural := 'Rural']

#fix date
arm[, date := as.Date(substr(datestamp, 1, 10), '%Y-%m-%d')]

#fix other (please specify) responses by assigning them to original question
arm[is.na(q12r6_1) & !is.na(q12r6_othcode1), q12r6_1 := q12r6_othcode1]
arm[is.na(q12r6_2) & !is.na(q12r6_othcode2), q12r6_2 := q12r6_othcode2]

#consolidate q12r6_ + drop duplicate columns
arm[!is.na(q12r6_1), q12r6 := q12r6_1]
arm[!is.na(q12r6_2), q12r6 := q12r6_2]
arm[!is.na(q12r6_3), q12r6 := q12r6_3]
arm[!is.na(q12r6_4), q12r6 := q12r6_4]
arm[!is.na(q12r6_5), q12r6 := q12r6_5]
arm[!is.na(q12r6_6), q12r6 := q12r6_6]
arm[!is.na(q12r6_7), q12r6 := q12r6_7]
arm[!is.na(q12r6_99), q12r6 := q12r6_99]
arm[!is.na(q12r6_othcode1), q12r6 := q12r6_othcode1]
arm[!is.na(q12r6_othcode2), q12r6 := q12r6_othcode2]
arm[, (colnames(arm)[colnames(arm) %like% 'q12r6_']) := NULL]

#format employment responses
arm[q12r1 == 'Yes', emp_1 := 1] 
arm[q12r1 == 'No', emp_1 := 0]
arm[q12r1 %like% 'Refuse', emp_1 := NA]

arm[q12r7_1 == 'Yes', emp_2 := 1] 
arm[q12r7_1 == 'No', emp_2 := 0]
arm[q12r7_1 %like% 'Refuse', emp_2 := NA]

#calculate emp lost
get_emp_lost(arm)

#tag location + nid
arm[, ihme_loc_id := 'ARM']
arm[, nid := 480438]

#save
write.csv(arm, paste0(output_dir, 'armenia_hfps.csv'), row.names = F)


#---- GEO HFPS ------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
geo_hfps_files <- "/FILEPATH/"

#read and subset
geo_hfps <- read_draw(paste0(fp, geo_hfps_files))
geo_hfps <- geo_hfps[, c('sex', 'age', 'stratum', 'q11', 'q21', 'q24', 'int_date')]

#change names
setnames(geo_hfps, 'stratum', 'urban_rural')
setnames(geo_hfps, 'q11', 'education')
setnames(geo_hfps, 'int_date', 'date')

#edit age
geo_hfps[age < 25, age_simple := 'Less than 25']
geo_hfps[age >= 25 & age < 45, age_simple := '25 to 45']
geo_hfps[age >= 45, age_simple := 'More than 45']

#edit urbanicity
geo_hfps[urban_rural == 'Capital', urban_rural := 'Urban']

#edit education
geo_hfps[education %in% c('Master, 5-year diploma, Doctor or equivalent',
                          'Currently studying for BA, Bachelor or equivalent',
                          'Secondary technical/Vocational education',
                          'Upper secondary education'), education_bin := 1]
geo_hfps[education %in% c('Lower secondary education',
                          'Do not have primary education',
                          'Primary education'), education_bin := 0]

#edit employment questions
geo_hfps[q21 == 'Yes', emp_1 := 1] 
geo_hfps[q21 == 'No', emp_1 := 0]

geo_hfps[q24 == 'Yes', emp_2 := 1] 
geo_hfps[q24 == 'No', emp_2 := 0]

#emp los
get_emp_lost(geo_hfps)

#tag location + nid
geo_hfps[, ihme_loc_id := 'GEO']
geo_hfps[, nid := 467741]

#save
write.csv(geo_hfps, paste0(output_dir, 'geo_hfps.csv'), row.names = F)


#---- GEO MONITOR ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
geo_monitor_files <- "/FILEPATH/"

#read + subset
geo_monitor <- read_draw(paste0(fp, geo_monitor_files))
geo_monitor <- geo_monitor[, c('mid', 'wave', 'sex', 'age', 'agegroup', 'stratum', 'd2', 'e1', 'e1a', 'e1b', 'weight', 'int_date')]

#rename to standardize data
setnames(geo_monitor, 'd2', 'education') %>%
  setnames('stratum', 'urban_rural') %>%
  setnames('int_date', 'date')

#edit ages
geo_monitor[age < 25, age_simple := 'Less than 25']
geo_monitor[age >= 25 & age < 45, age_simple := '25 to 45']
geo_monitor[age >= 45, age_simple := 'More than 45']

#edit education
geo_monitor[education == 'Complete higher education' | 
              education %like% 'Vocational education' |
              education == 'Incomplete higher education', education_bin := 1]

geo_monitor[education == 'Complete or incomplete secondary education' |
              education %like% 'Complete or incomplete secondary education' |
              education == 'Complete or incomplete basic education', education_bin := 0]

#format urbanicity
geo_monitor[urban_rural %like% 'Urban' | urban_rural %like% 'Capital', urban_rural := 'Urban']
geo_monitor[urban_rural %like% 'Rural', urban_rural := 'Rural']

#format employment questions --- 
geo_monitor[e1 == 'Yes', c19_emp_loss_perm := 1]
geo_monitor[e1 == 'No', c19_emp_loss_perm := 0] 

geo_monitor[e1a == 'Yes', e1a_resp := 1]
geo_monitor[e1a == 'No', e1a_resp := 0] 

geo_monitor[e1b == 'Yes', emp_2 := 1]
geo_monitor[e1b == 'No', emp_2 := 0] 

#tag location + nid
geo_monitor[, ihme_loc_id := 'GEO']
geo_monitor[wave == 1, nid := 467760]
geo_monitor[wave == 2, nid := 467967]
geo_monitor[wave == 3, nid := 467973]
geo_monitor[wave == 4, nid := 467979]
geo_monitor[wave == 5, nid := 467984]
geo_monitor[wave == 6, nid := 467988]

#save
write.csv(geo_monitor, paste0(output_dir, 'georgia_monitor.csv'), row.names = F)


#---- IND -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
ind_hfps <- c("/FILEPATH/")

#get + subset data
ind <- read_draw(paste0(fp, ind_hfps))
keep <- c('demo_gender',  'age', 'demo_age', 'demo_resp_age', 'demo_edu',
          colnames(ind)[colnames(ind) %like% 'demo_resp_gen'], 
          colnames(ind)[colnames(ind) %like% 'weight_hh'],  
          colnames(ind)[colnames(ind) %like% 'lab_march_occu'],  
          colnames(ind)[colnames(ind) %like% 'lab_curr_occu'])
ind <- ind %>%
  select(keep[keep %in% colnames(ind)]) %>%
  as.data.table()

#edit wave 1
ind$lab_march_occu_oth <- NULL 
ind[, nid := 463553]
ind[, wave := 1]
setnames(ind, 'demo_age', 'age') %>% 
  setnames('demo_gender', 'sex') %>% 
  setnames('demo_edu', 'education')

#create age bins
ind[age %like% '18-19' | age %like% '20-24', age_simple := 'Less than 25']
ind[age %like% '25-29' | age %like% '30-34' | age %like% '35-39' | age %like% '40-44', age_simple := '25 to 45']
ind[age >= 45, age_simple := 'More than 45']


#re-assign education
ind[education %like% "More than high school", education_bin := 1]
ind[education %like% "Class 5-10" |
      education %like% "High school graduate" |
      education %like% "No schooling" |
      education %like% "Class 5 or less", education_bin := 0]

#re-assign employment answers
ind[lab_march_occu == 'Did not work for income', emp_1 := 0]
ind[lab_march_occu != 'Did not work for income', emp_1 := 1] 

ind[lab_curr_occu == 'Did not work for income', emp_2 := 0]
ind[lab_curr_occu != 'Did not work for income', emp_2 := 1] 

#emp los
get_emp_lost(ind)

#tag
ind[, urban_rural := 'Rural']
ind[, ihme_loc_id := 'IND']
ind[, date := as.Date('2020-05-08')] 
ind[, nid := 463553]

#save
write.csv(ind, paste0(output_dir, 'india_hfps.csv'), row.names = F)

#remove extra info
rm(keep)


#---- IRQ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
irq_hfps <- c("/FILEPATH/")

#get + subset data
irq <- read_draw(paste0(fp, irq_hfps))
irq <- irq[, c('hh_weight', 'pop_weight', 'adult_weight', 'respsex', 'age_recoded', 'RESPEducation_recoded', 'environment_type', 'EMP1', 'EMP7', 'EMP8')]  

#rename
setnames(irq, 'respsex', 'sex')
setnames(irq, 'age_recoded', 'age')
setnames(irq, 'RESPEducation_recoded', 'education')
setnames(irq, 'environment_type', 'urban_rural')

#create age bins
irq[age %like% '18-19' | age %like% '20-24', age_simple := 'Less than 25']
irq[age %like% '25-29' | age %like% '30-34' | age %like% '35-39' | age %like% '40-44', age_simple := '25 to 45']
irq[age >= 45, age_simple := 'More than 45']

#re-assign education
irq[education == "Bachelor's Degree" |
      education == "Master's Degree or Doctoral Degree" |
      education == 'Preparatory/Secondary Certificate - Academic/vocational' |
      education == 'Technical diploma' |
      education == 'Professional Degree or Higher Diploma Degree', education_bin := 1]
irq[education == 'Did not attend any school or complete any level' |
      education == 'Intermediate Certificate (7-9) or Basic Certificate (1-9)' |
      education == 'Primary/Elementary Certificate (1-6)', education_bin := 0]

#edit employment responses
irq[!is.na(EMP1) & EMP1 %like% 'Did not have any job', emp_1 := 0] 
irq[!is.na(EMP1) & EMP1 %not like% 'Did not have any job', emp_1 := 1]

irq[!is.na(EMP7) & EMP7 %like% 'Did not have any job', emp_2 := 0] 
irq[!is.na(EMP7) & EMP7 %not like% 'Did not have any job', emp_2 := 1]

#emp loss
get_emp_lost(irq)

irq[EMP8 %in% c('The same', 'Increased'), c19_inc_loss_bn := 0]
irq[EMP8 %in% c('Decreased', 'No current income due to temporary layoff', 'No income due to business closure/no farm activities'), c19_inc_loss_bn := 1]
irq[emp_2 == 0, c19_inc_loss_bn := NA] 

#tag location + nid + date
irq[, ihme_loc_id := 'IRQ']
irq[, nid := 480841]
irq[, date := as.Date('2020-08-15')]

#save
write.csv(irq, paste0(output_dir, 'iraq_hfps.csv'), row.names = F)


#---- KHM -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file paths
khm_hfps <- c("/FILEPATH1/",
              "/FILEPATH2/",
              "/FILEPATH3/",
              "/FILEPATH4/")

#read + bind + subset data
khm <- paste0(fp, khm_hfps) %>% 
    lapply(read_draw) %>% 
    join_all(by='interview__key')
khm <- khm[, c('s2bq3', 'sector', 'hhweight', 's2bq2', 's6q1', 's6q2', 's1q2', 's2bq10')]

#rename
setnames(khm, 's2bq2', 'age') %>%
  setnames('s2bq3','sex') %>%
  setnames('sector','urban_rural') %>%
  setnames('s1q2','date') %>%
  setnames('s2bq10', 'education')

#create age bins
khm[age < 25, age_simple := 'Less than 25']
khm[age >= 25 & age < 45, age_simple := '25 to 45']
khm[age >= 45, age_simple := 'More than 45']

#re-assign education
khm[education == 'Incomplete bachelor' | 
      education %like% 'Incomplete bachelor' | 
      education == 'Complete Bachelor or higher', education_bin := 1]
khm[education == 'Incomplete lower secondary' | 
      education == 'Incomplete primary' | 
      education == 'Complete primary' | 
      education == 'Incomplete upper secondary' | 
      education == 'Complete lower secondary' | 
      education == 'Complete upper secondary' | 
      education == 'No education', education_bin := 0]

#edit employment responses
khm[s6q1 == 'YES', emp_2 := 1]
khm[s6q1 == 'NO', emp_2 := 0]
khm[, s6q1 := NULL]

khm[s6q2 == 'YES', emp_1 := 1] 
khm[s6q2 == 'NO', emp_1 := 0]
khm[, s6q2 := NULL]

#emp los
get_emp_lost(khm)
#get_emp_lost_all(khm)
khm[emp_2 == 1, emp_lost_all := 0] 

#tag location
khm[, ihme_loc_id := 'KHM']
khm[, date := as.Date('2020-06-20')]
khm[, nid := 480517]

#save
write.csv(khm, paste0(output_dir, 'khm_hfps.csv'), row.names = F)


#---- ZMB -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
zmb_hfps <- c("/FILEPATH/")

#get + subset data
zmb <- read_draw(paste0(fp, zmb_hfps))
zmb <- zmb[, c('s1q2_age', 's1q3_male', 's1q8_educ', 's1q8_OTH', 'weight', 'hhi_urban', 's5_E1', 's5_E2')]

#change names
setnames(zmb, 's1q2_age', 'age')
setnames(zmb, 's1q3_male', 'sex')
setnames(zmb, 's1q8_educ', 'education')
setnames(zmb, 'hhi_urban', 'urban_rural')

#create age bins
zmb[age < 25, age_simple := 'Less than 25']
zmb[age >= 25 & age < 45, age_simple := '25 to 45']
zmb[age >= 45, age_simple := 'More than 45']

#create education bins
zmb[education %like% 'OTHER', education := s1q8_OTH]
zmb$s1q8_OTH <- NULL

zmb[education %in% c('BACHELOR INCOMPLETE', 'BACHELOR COMPLETED OR HIGHER'), education_bin := 1]
zmb[education %in% c('SECONDARY COMPLETE', 'PRIMARY COMPLETE', 'PRIMARY INCOMPLETE', 'INTERMEDIATE COMPLETE', 'SECONDARY INCOMPLETE', 'INTERMEDIATE INCOMPLETE', 'NEVER ATTENDED SCHOOL'), education_bin := 0]

zmb[education %in% c('Pursuing diploma', 'Diploma', 'Diploma general insurance', 'Degree', 'Advanced diploma', 'Diploma complete', 'College', 'college nursing', 'Certificate', 'Mechqnics', 'Crafts certificate', 'University', 'Diploma completed', 'diploma', 'Certificate in Accounts', 'Advanced certificate', 'certificate level Mn E,Psychosocial couselling, purchasing and supply', 'Tertiary'), education_bin := 1]
zmb[education %in% c('Incomplete diploma', 'Incomplete craft  certificate', 'Short course done', 'Incomplete  diploma', 'Studying'), education_bin := 0]

#edit employment responses
zmb[s5_E1 == 'YES', emp_2 := 1]
zmb[s5_E1 == 'NO', emp_2 := 0]

zmb[s5_E2 == 'YES', emp_1 := 1] 
zmb[s5_E2 == 'NO', emp_1 := 0]

#emp los
get_emp_lost(zmb)
#get_emp_lost_all(zmb)

#tag location + nid + date
zmb[, ihme_loc_id := 'ZMB']
zmb[, nid := 480834]
zmb[, date :=  '2020-06-15']

#save
write.csv(zmb, paste0(output_dir, 'zambia_hfps.csv'), row.names = F)


#---- MMR -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

mmr_hfps <- c("/FILEPATH1/",
              "/FILEPATH2/",
              "/FILEPATH3/",
              "/FILEPATH4/",
              "/FILEPATH5/",
              "/FILEPATH6/")

mmr <- c()
for (f in 1:6) {
  files <- mmr_hfps[mmr_hfps %like% paste0('R', f, '_')]

  temp <- read_draw(paste0(fp, files))

  temp <- temp[, c('s1q1','s1q2','s1q13','s1q13str','s4q1','s4q5','ppweight','hhweight', 's2q1', 's4q11')]

  #tag with wave and location id
  temp$wave <- f
  temp$ihme_loc_id <- 'MMR'
  
  #set names
  setnames(temp,'s1q1', 'age') %>%
    setnames('s1q2', 'sex') %>%
    setnames('s1q13', 'education') %>%
    setnames('s1q13str', 'edu_other') %>%
    setnames('s4q1', 'before_march') %>%
    setnames('s4q5', 'past_week') %>%
    setnames('s2q1', 'location')
  
  #bind temp
  mmr <- rbind(mmr, temp, fill=T)
}

#create age bins
mmr[age < 25, age_simple := 'Less than 25']
mmr[age >= 25 & age < 45, age_simple := '25 to 45']
mmr[age >= 45, age_simple := 'More than 45']

#re-assign education
mmr[education == 'Bachelor Graduate' |
      education == 'Teachers Certificate (After Std 10)' |
      education == 'Tvet Diploma (Gti, Gtc Etc.)' |
      education == 'Masters Degree' |
      education == 'Undergraduate Diploma' |
      education == 'Postgraduate Diploma' |
      education == 'Phd' |
      education == 'TVET DIPLOMA (GTI, GTC ETC.)' |
      education == 'UNDERGRADUATE DIPLOMA' |
      education == 'BACHELOR GRADUATE' |
      education == 'POSTGRADUATE DIPLOMA' |
      education == 'MASTERS DEGREE' |
      education == 'PHD', education_bin := 1]

mmr[education == 'Grade 2' |
      education == 'Grade 10' |
      education == 'Grade 11' |
      education == 'Grade 1' |
      education == 'Ths (After Std 8)' |
      education == 'Monastic/ Religious' |
      education == 'None' |
      education == 'Grade 7' |
      education == 'Grade 5' |
      education == 'Grade 9' |
      education == 'Grade 8' |
      education == 'Grade 6' |
      education == 'Grade 4' |
      education == 'MONASTIC/ RELIGIOUS' |
      education == 'Kg Completed(2015-16 Old System)', education_bin := 0]

#format employment responses
mmr[before_march == 'Yes', emp_1 := 1]
mmr[before_march == 'No', emp_1 := 0]

mmr[past_week %like% 'Yes', emp_2 := 1]
mmr[past_week == 'No', emp_2 := 0]

#emp los
get_emp_lost(mmr)

#income loss 
mmr[s4q11 %in% c('Increased', 'Stayed the same'), c19_inc_loss_bn := 0]
mmr[s4q11 %in% c('Decreased', 'No payment'), c19_inc_loss_bn := 1]
mmr[emp_2 == 0, c19_inc_loss_bn := NA] 

#tag urban_rural as missing
mmr[, urban_rural := NA]
mmr[, nid := 488950]

#save
write.csv(mmr, paste0(output_dir, 'myanmar_hfps.csv'), row.names = F)


#---- STP -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#file path
stp_hfps <- c("/FILEPATH/") 

#get + subset data
stp <- read_draw(paste0(fp, stp_hfps))
stp <- stp[, c('s2q5', 's3q8', 'Meio', 'hhweight', 's7q1', 's7q2')]

setnames(stp, 's2q5', 'sex') %>%
  setnames('s3q8', 'education') %>%
  setnames('Meio', 'urban_rural')

#edit sex
stp[sex == '1. Homem', sex := 'Male']
stp[sex == '2. Mulher', sex := 'Female']

#edit education
stp[education %like% '8. SUPERIOR INCOMPLETO' |
      education %like%  '9. SUPERIOR COMPLETO OU MAIS', education_bin := 1]
stp[education %like% '1. NUNCA FREQUENTOU A ESCOLA' |
      education %like% '2. PRIMÁRIO INCOMPLETO' |
      education %like% '3. PRIMÁRIO COMPLETO' |
      education %like% '4. BÁSICO INCOMPLETO' |
      education %like% '5. BÁSICO COMPLETO' |
      education %like% '6. SECUNDÁRIO INCOMPLETO' |
      education %like% '7. SECUNDÁRIO COMPLETO', education_bin := 0]

#edit employment responses
stp[s7q2 %like% '1.', emp_1 := 1]
stp[s7q2 %like% '2.', emp_1 := 0]
  
stp[s7q1 %like% '1.', emp_2 := 1]
stp[s7q1 %like% '2.', emp_2 := 0]

#emp los
get_emp_lost(stp)

#edit urban-rural
stp[urban_rural %like% 'Urbano', urban_rural := 'Urban']
stp[urban_rural %like% 'Rural', urban_rural := 'Rural']

#save
write.csv(stp, paste0(output_dir, 'sao_tome_principe_hfps.csv'), row.names = F)
