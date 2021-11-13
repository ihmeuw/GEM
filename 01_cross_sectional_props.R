#############################################################################################################
#
# Author: USERNAME
# Purpose: Create country-sex proportions of selected indicators from cross-sectional data
#
#
#############################################################################################################

rm(list=ls())

library(data.table)
library(boot)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')
library(crosswalk002, lib.loc = "FILEPATH")

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]
out.dir <- 'FILEPATH'
out.dir2 <- 'FILEPATH'

# 1. SRHC disruption due to covid from UN women RGA -------------------------------------------------------------------------------------------------------------------------------

#read in, subset to relevant variables, merge on standard location information
rga <- fread('FILEPATH')[, V1:=NULL]
rga <- rga[, c('gender', 'age', 'weight', 'urbanicity', 'education', 'partner_status', 'srhc_covid_among_need', 'ihme_loc_id')]
rga <- merge(rga, locs[, c('ihme_loc_id', 'location_name')], by='ihme_loc_id')
rga[, c('ihme_loc_id', 'partner_status'):=NULL]

#set survey dates and format date variable
rga <- rga %>% mutate(date = case_when(
  location_name == "Mozambique" ~ "2020-11-01",
  location_name == "Central African Republic" ~ "2020-11-01",
  location_name == "Guinea" ~ "2020-10-01",
  location_name == "Mali" ~ "2020-11-01",
  location_name == "Senegal" ~ "2020-08-01",
  location_name == "Côte d'Ivoire" ~ "2020-09-01",
  location_name == 'Uganda' ~ "2020-11-30"))
rga[, date:=as.Date(date)]
rga[, yearmonth:=format(date, "%Y-%m")]
rga[, date:=yearmonth]

#melt to long and set up standard variables (e.g. source/gender/date)
rga <- melt.data.table(rga, measure.vars='srhc_covid_among_need')
rga[, source:='UN Women RGA']
rga <- rga[location_name!='Uganda'] #this indicator not available from Uganda module; remove all NAs
setnames(rga, c('gender'), c('sex'))
rga[, date:=as.character(date)]
rga[, observation_id:=1:.N] #create observation id by unique row of data for summing nums/denoms below

#copy data and set levels of proportion aggregation (location/gender)
all <- copy(rga)
c.lvls2 <- c('sex', 'variable', 'value', 'location_name', 'source', 'date') #
c.lvls <- c('sex', 'variable', 'location_name', 'source', 'date') # 

#Get weighted numerators and sample size
props <- all[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),proportion:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),proportion:=0]
props[!is.na(value), standard_error:=sqrt((proportion*(1-proportion))/sample)]
props[!is.na(value),proportion_lower:=proportion- (1.96 * sqrt((proportion*(1-proportion))/sample))]
props[!is.na(value),proportion_upper:=proportion+ (1.96 * sqrt((proportion*(1-proportion))/sample))]

#subset to props of TRUE and format for central saving
rga.props <- props[value==1 & sample>30]
setnames(rga.props, c('variable', 'source'), c('indicator', 'data_source'))
rga.props <- rga.props[, c('date', 'indicator', 'sample', 'proportion', 'proportion_lower', 'proportion_upper', 'sex', 'location_name', 'data_source')]
rga.props[, age:='18+']
rga.props <- merge(rga.props, locs[, c('location_id', 'location_name')], by='location_name')
setcolorder(rga.props, c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'proportion_lower', 'proportion_upper', 'data_source', 'age'))
write.csv(rga.props, paste0(out.dir, 'srhc_covid_props.csv'))
write.csv(rga.props, paste0(out.dir2, 'srhc_covid_props.csv'))

#2. Safety at home from a) FB GEAH and b) UN Women RGA --------------------------------------------------------------------------------------------------------------------------------

#a) FB GEAH: read in data and initial format ------------------------------------------------
fb.geah <- fread('FILEPATH')[, V1:=NULL]
fb.geah <- fb.geah[, c('location_name', 'gender', 'weight', 'safety')]
fb.geah[, unsafe_at_home:=ifelse(safety %in% 1:2, 1, ifelse(safety %in% 3:5, 0, NA))]
fb.geah <- melt.data.table(fb.geah, measure.vars=c('unsafe_at_home'), value.name='value', variable.name='variable')
setnames(fb.geah, 'gender', 'sex')

#set stratifiers
c.lvls2 <- c('location_name', 'sex', 'variable', 'value')
c.lvls <- c('location_name', 'sex', 'variable')

#Get numerators, sample size, remove missing sex
fb.geah[, observation_id:=1:.N]
fb.geah <- fb.geah[!is.na(sex)]
props <- fb.geah[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),proportion:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),proportion:=0]
props[!is.na(value),proportion_lower:=proportion-(1.96 * sqrt((proportion*(1-proportion))/sample))]
props[!is.na(value),proportion_upper:=proportion+(1.96 * sqrt((proportion*(1-proportion))/sample))]

#subset to TRUE values and create variables for central saving
geah <- copy(props[value==1 & sample>30])
geah[, `:=` (date='2020-07', data_source='Facebook Gender Equality at Home', age='18+')]

#clean up locations to standard
geah[location_name=='Bolivia', location_name:='Bolivia (Plurinational State of)']
geah[location_name=='Vietnam', location_name:='Viet Nam']
geah[location_name=='Taiwan', location_name:='Taiwan (Province of China)']
geah[location_name=='Swaziland', location_name:='Eswatini']
geah[location_name=='Czech Republic', location_name:='Czechia']
geah[location_name=='Laos', location_name:="Lao People's Democratic Republic"]
geah[location_name=='Macedonia', location_name:='North Macedonia']
geah[location_name=='Moldova', location_name:='Republic of Moldova']
geah[location_name=='Russia', location_name:='Russian Federation']
geah <- merge(geah, locs[, c('location_name', 'location_id', 'super_region_name')], by='location_name', all.x=T)
geah <- geah[!(location_name=='Georgia' & super_region_name=='High-income')] #creates repeat observations for the US state of Georgia; remove
geah <- geah[!is.na(location_id)] #some locations not mappable (e.g. "Rest of {super region}"), remove here

#finish formatting for central saving
geah <- geah[, c('location_id', 'location_name', 'sex', 'variable', 'sample', 'proportion' , 'proportion_lower', 'proportion_upper', 'date', 'data_source', 'age')]
setnames(geah, 'variable', 'indicator')
setcolorder(geah, c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'proportion_lower', 'proportion_upper', 'data_source', 'age'))

#b) UN WOMEN RGA: read and format data  ---------------------------------------------------------
rga <- fread('FILEPATH')
rga <- rga[, c('sex', 'location_name', 'super_region_name', 'regional_file', 'weight', 'unsafe_at_home', 'ihme_loc_id')]

#set data based upon survey, format
rga <- rga %>% mutate(date = case_when(
  ihme_loc_id == "MOZ" ~ "2020-11-01",
  ihme_loc_id == "CAF" ~ "2020-11-01",
  ihme_loc_id == "GIN" ~ "2020-10-01",
  ihme_loc_id == "MLI" ~ "2020-11-01",
  ihme_loc_id == "SEN" ~ "2020-08-01",
  ihme_loc_id == "CIV" ~ "2020-09-01",
  ihme_loc_id == 'UGA' ~ "2020-11-30", 
  ihme_loc_id == 'COL' ~ "2020-10-09",
  ihme_loc_id == 'MEX' ~ '2020-11-15',
  regional_file == 'arab_states' ~ '2020-05-27',
  ihme_loc_id == 'ETH' ~ '2020-11-15',
  ihme_loc_id == 'KEN' ~ '2020-09-15',
  ihme_loc_id == 'MWI' ~ '2020-11-30',
  ihme_loc_id == 'ZAF' ~ '2020-11-15',
  ihme_loc_id == 'SWZ' ~ '2021-02-08'
  ))

rga[, date:=as.Date(date)]
rga[, yearmonth:=format(date, "%Y-%m")]
rga[, date:=yearmonth]

#melt long, set up standard variables and finish clean-up
rga <- melt.data.table(rga, measure.vars='unsafe_at_home')
rga[, source:='UN Women RGA']
rga[, date:=as.character(date)]
rga[, observation_id:=1:.N]

#copy and set level of aggregation (location/gender)
all <- copy(rga)
c.lvls2 <- c('location_name', 'sex', 'variable', 'value', 'source', 'date') #
c.lvls <- c('location_name', 'sex', 'variable', 'source', 'date') # 

#Get numerators and sample sizes
props <- all[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings, no skips)
props[!is.na(value),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(value),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(value),proportion:=num/denom_nm]
props[!is.na(value)&(num==0|denom_nm==0),proportion:=0]
props[!is.na(value),proportion_lower:=proportion- (1.96 * sqrt((proportion*(1-proportion))/sample))]
props[!is.na(value),proportion_upper:=proportion+ (1.96 * sqrt((proportion*(1-proportion))/sample))]

#subset to TRUE answers and format for central saving
rga.props <- props[value==1 & sample>30]
setnames(rga.props, c('variable', 'source'), c('indicator', 'data_source'))
rga.props <- rga.props[, c('date', 'indicator', 'sample', 'proportion', 'proportion_lower', 'proportion_upper', 'sex', 'location_name', 'data_source')]
rga.props[, age:='18+']
rga.props <- merge(rga.props, locs[, c('location_id', 'location_name')], by='location_name')
setcolorder(rga.props, c('date', 'location_id', 'location_name', 'sex', 'indicator', 'proportion', 'sample', 'proportion_lower', 'proportion_upper', 'data_source', 'age'))
rga.props[, indicator:='unsafe_at_home']

#bind two sources (a & b) together and save
safety <- rbind(rga.props, geah, fill=T)
write.csv(safety, paste0(out.dir, 'unsafe_at_home_props.csv'))
write.csv(safety, paste0(out.dir2, 'unsafe_at_home_props.csv'))

#3. Perception of GBV worsening in the community from a)goalkeepers and b) un women rga -------------------------------------------------------------------------------

# a) Goalkeepers 2021: read in and format -----------------------------------------------------------------------
gk_micro <- fread(paste0('FILEPATH'))[, V1:=NULL]
gk <- gk_micro[, c('country', 'observation_id'):=NULL]
gk[age=='Not Available', age:=NA]
gk <- gk[gender=='Female']
gk <- merge(gk, locs[, c('location_id', 'location_name')], by='location_id')
setnames(gk, 'geography', 'urbanicity')
gk <- gk[, c('location_name', 'age', 'urbanicity','education', 'community_gbv_change')]
gk[, source:='Goalkeepers']

#copy and set levels of aggregation for props (location only; data only available from females)
dt.long <- copy(gk)
c.lvls2 <- c('community_gbv_change', 'location_name') 
c.lvls <- c('location_name')  

#aggs
dt.long[, obs:=1] #unweighted data
gbv.props <- dt.long[,.(num=sum(obs,na.rm=T)),by=c.lvls2]
gbv.props[!is.na(community_gbv_change),sample:=sum(num,na.rm=T),by=c.lvls]
gbv.props[!is.na(community_gbv_change),proportion:=num/sample]
gbv.props[!is.na(community_gbv_change),proportion_lower:=proportion-(1.96 * sqrt((proportion*(1-proportion))/sample))]
gbv.props[!is.na(community_gbv_change),proportion_upper:=proportion+(1.96 * sqrt((proportion*(1-proportion))/sample))]
gbv.props <- gbv.props[community_gbv_change==1 & sample>30] #subset to TRUE values

#format for central saving
setnames(gbv.props, c('community_gbv_change'), c('indicator'))
gbv.props <- gbv.props[, c('indicator', 'sample', 'proportion', 'proportion_lower', 'proportion_upper', 'location_name')]
gbv.props[, `:=` (age='16+', data_source='Goalkeepers', date='2021-05')]
gbv.props <- merge(gbv.props, locs[, c('location_id', 'location_name')], by='location_name')
gbv.props[, indicator:=as.character(indicator)]
gbv.props[, indicator:='community_gbv_change']
gbv.props[, sex:='Female']
setcolorder(gbv.props, c('date', 'location_id', 'location_name','sex', 'indicator', 'proportion', 'sample', 'proportion_lower', 'proportion_upper', 'data_source', 'age'))

#b) UN WOMEN RGA: read in and format --------------------------------------------------------------------
rga <- fread('FILEPATH')
rga <- rga[, c('sex', 'location_name', 'super_region_name', 'regional_file', 'weight', 'community_gbv_change', 'ihme_loc_id')]

rga <- rga %>% mutate(date = case_when(
  ihme_loc_id == "MOZ" ~ "2020-11-01",
  ihme_loc_id == "CAF" ~ "2020-11-01",
  ihme_loc_id == "GIN" ~ "2020-10-01",
  ihme_loc_id == "MLI" ~ "2020-11-01",
  ihme_loc_id == "SEN" ~ "2020-08-01",
  ihme_loc_id == "CIV" ~ "2020-09-01",
  ihme_loc_id == 'UGA' ~ "2020-11-30", 
  ihme_loc_id == 'COL' ~ "2020-10-09",
  ihme_loc_id == 'MEX' ~ '2020-11-15',
  regional_file == 'arab_states' ~ '2020-05-27',
  ihme_loc_id == 'ETH' ~ '2020-11-15',
  ihme_loc_id == 'KEN' ~ '2020-09-15',
  ihme_loc_id == 'MWI' ~ '2020-11-30',
  ihme_loc_id == 'ZAF' ~ '2020-11-15',
  ihme_loc_id == 'SWZ' ~ '2021-02-08',
  ihme_loc_id == 'ALB' ~ '2020-04-25',
  ihme_loc_id == 'BIH' ~ '2020-05-15',
  ihme_loc_id == 'GEO' ~ '2020-05-08',
  ihme_loc_id == 'MDA' ~ '2020-04-25',
  ihme_loc_id == 'MKD' ~ '2020-05-20',
  ihme_loc_id == 'TUR' ~ '2020-05-25',
  ihme_loc_id == 'AZE' ~ '2020-05-03',
  ihme_loc_id == 'SRB' ~ '2020-07-30',
  ihme_loc_id == 'ARM' ~ '2020-06-01', #no date available for armenia
  ihme_loc_id == 'BLR' ~ '2020-06-01', #no date available for belarus
))

rga[, date:=as.Date(date)]
rga[, yearmonth:=format(date, "%Y-%m")]
rga[, date:=yearmonth]
rga[, source:='UN Women RGA']
rga[, date:=as.character(date)]
rga[, observation_id:=1:.N]

#copy and set levels of aggregation for props (location/gender)
all <- copy(rga)
c.lvls2 <- c('location_name', 'community_gbv_change', 'source', 'date', 'sex') #
c.lvls <- c('location_name', 'source', 'date', 'sex') # 

#Get numerators and sample sizes
props <- all[,.(num=sum(weight,na.rm=T),obs=uniqueN(observation_id,na.rm=T)),by=c.lvls2]

#Proportion of Non-Missing Total (no missings)
props[!is.na(community_gbv_change),denom_nm:=sum(num,na.rm=T),by=c.lvls]
props[!is.na(community_gbv_change),sample:=sum(obs,na.rm=T),by=c.lvls]
props[!is.na(community_gbv_change),proportion:=num/denom_nm]
props[!is.na(community_gbv_change)&(num==0|denom_nm==0),proportion:=0]
props[!is.na(community_gbv_change),proportion_lower:=proportion- (1.96 * sqrt((proportion*(1-proportion))/sample))]
props[!is.na(community_gbv_change),proportion_upper:=proportion+ (1.96 * sqrt((proportion*(1-proportion))/sample))]
props[!is.na(community_gbv_change)&(proportion_lower<0),proportion_lower:=0]
props[!is.na(community_gbv_change)&(proportion_upper>1),proportion_upper:=1]

#subset to TRUE answers and format for central saving
rga.props <- props[community_gbv_change==1 & sample>30]
setnames(rga.props, c('community_gbv_change', 'source'), c('indicator', 'data_source'))
rga.props <- rga.props[, c('date', 'indicator', 'sex', 'sample', 'proportion', 'proportion_lower', 'proportion_upper', 'location_name', 'data_source')]
rga.props[, age:='18+']
rga.props <- merge(rga.props, locs[ihme_loc_id!='USA_533', c('location_id', 'location_name')], by='location_name')
setcolorder(rga.props, c('date', 'location_id', 'location_name','sex', 'indicator', 'proportion', 'sample', 'proportion_lower', 'proportion_upper', 'data_source', 'age'))
rga.props[, indicator:=as.character(indicator)]
rga.props[, indicator:='community_gbv_change']

#bind two sources (a & b) together and save
gbv.community <- rbind(rga.props, gbv.props)
write.csv(gbv.community[data_source=='UN Women RGA'], paste0(out.dir, '/community_gbv_change_props.csv'))
write.csv(gbv.community[sex=='Female' & data_source=='Goalkeepers'], paste0(out.dir, '/female_only/community_gbv_change_props.csv'))
write.csv(gbv.community[data_source=='UN Women RGA'], paste0(out.dir2, '/community_gbv_change_props.csv'))
write.csv(gbv.community[sex=='Female' & data_source=='Goalkeepers'], paste0(out.dir2, '/female_only/community_gbv_change_props.csv'))
