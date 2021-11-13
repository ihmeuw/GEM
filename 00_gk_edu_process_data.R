#############################################################################################################
#
# Author: USERNAME
# Purpose: Clean and process schooling disparity data from goalkeepers premise survey 2021
#
#############################################################################################################

rm(list = ls())

pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4)
aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text =element_text(size=13,face='bold'),plot.title =element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')

in.root <- "FILEPATH"
out.root <- "FILEPATH"

#--metadata
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--load data, set dirs
dt <- fread(paste0(in.root,"FINAL_education_clean.csv"))
out.dir <- 'FILEPATH'
plot.dir <- 'FILEPATH'

#get parental education map
edu_map <- fread(paste0(out.root,"ref/edu_map.csv"))
dt <- merge(dt,edu_map,by='education',all.x=T)

#--reshape to learner-level data
c.ids <- c("location_id","country","observation_id","gender","age","geography","financial_situation" ,"gbd_education","employment_status","ethnicity", "religion")
dt.l <- melt.data.table(dt[,c(c.ids,names(dt)[names(dt)%like%"edu_"]),with=F],id.vars=c.ids)
for (i in 1:4){dt.l[grepl(paste0("ch",i),variable),chid:=i]}
for (i in 1:4){dt.l[,variable:=gsub(paste0("edu_ch",i,"_"),"",variable)]}
dt.l[variable=="age",variable:="child_age"]
dt.l[variable=="gender",variable:="child_gender"]
dt <- dcast.data.table(as.formula(paste(paste(c(c.ids,"chid"), collapse = " + "), "~ variable")),data=dt.l)
dt <- dt[!is.na(child_age)]

#--clean desired variables

#merge on locs
dt <- merge(dt,locs,by='location_id',all.x=T)

#set child gender
dt[child_gender=="0",child_gender:="Male"]
dt[child_gender=="1",child_gender:="Female"]
dt[child_gender=="2",child_gender:="Non-Binary"]
dt[,child_female:=ifelse(child_gender=="Female",1,0)]

#set level of school
dt[level=="1",level:="Preschool or kindergarten"]
dt[level=="2",level:="Grades/years 1-5 (Primary or elementary school)"]
dt[level=="3",level:="Grades/years 6-8 (Higher elementary or Secondary school)"]
dt[level=="4",level:="Grades/years 9-12 (Secondary School)"]
dt[level=="5",level:="University (Post-Secondary)"]

#school situation
dt[situation=="1",situation:="In person class"]
dt[situation=="2",situation:="Learning online or remotely at home"]
dt[situation=="3",situation:="Mix of in-person and online/remote"]
dt[situation=="0",situation:="Not currently in school"]

#why out of school
dt[no_school_why=="1",no_school_why:="School is closed due to COVID-19"]
dt[no_school_why=="2",no_school_why:="School is closed due to break/vacation"]
dt[no_school_why=="3",no_school_why:="Already graduated"]
dt[no_school_why=="4",no_school_why:="School is too far away"]
dt[no_school_why=="5",no_school_why:="Cannot afford to go to school"]
dt[no_school_why=="6",no_school_why:="School does not have clean, safe buildings and bathrooms"]
dt[no_school_why=="7",no_school_why:="Had to leave school to work"]
dt[no_school_why=="8",no_school_why:="Had to leave school to care for a family member"]
dt[no_school_why=="99",no_school_why:="Had to leave school for other reason"]

#dropout
dt[situation%in%c("In person class","Learning online or remotely at home","Mix of in-person and online/remote"),dropout:=0]
dt[situation=="Not currently in school"&no_school_why!="Already graduated",dropout:=1]
#dt[reopen=="Person will return to school",dropout==1]

#tech access among distance learners
dt[internet_access%in%c("1","2","0"),good_internet:=0]
dt[internet_access%in%c("3","4"),good_internet:=1]

#long for graphing/aggregations
dt.l <- melt.data.table(dt,id.vars=c(c.ids,"chid","child_age","child_gender","child_female","low_parental_edu","cg_pe","level",names(locs)[-1]))
dt.l[,obs:=1]

#format dt for regressions and standard covariates
all <- copy(dt.l)
all[, value:=as.numeric(value)]

#low parental education == higher edu bin
all[gbd_education%in%c("edu_prop_12_14","edu_prop_15_18"),higher_edu:=1]
all[gbd_education%in%c("edu_prop_0","edu_prop_1_5","edu_prop_6_11"),higher_edu:=0]

#female
all[, female:=ifelse(gender=='Female',1,ifelse(gender=='Male',0,NA))]

#urbanicity
all[geography %in% c('Suburban/Peri-urban','City center or metropolitan area'), rural:=0]
all[geography %in% c('Rural'), rural:=1]

#age
all <- all %>%
  mutate(age_simple = case_when(
    age %in% c('16-', '16-24', '16-25', '18-24', '16 to 25 years old') ~ "Less than 25", 
    age %in% c('25-34', '26-35', '35-44', '36-45', '26 to 35 years old', '36 to 45 years old') ~ "25 to 45", 
    age %in% c('45-54', '46+', '55-64', '65-74', '65+', '75+', 'Over 45 years old') ~ "More than 45"
  ))
all <- all %>%
  mutate(age25less = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~1,
    age_simple == 'More than 45' ~0
  ))
all <- all %>%
  mutate(age25to45 = case_when(
    age_simple == '25 to 45' ~ 1,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~0
  ))
all <- all %>%
  mutate(age45more = case_when(
    age_simple == '25 to 45' ~ 0,
    age_simple == 'Less than 25' ~0,
    age_simple == 'More than 45' ~1
  ))

#create binary child_female
all[, child_female:=ifelse(child_gender=='Female', 1, ifelse(child_gender=='Male',0,NA))]

#write file
write.csv(all, paste0(out.dir, 'edu_data_prepped_for_regs.csv'))
