#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for unsafe at home indicator from FB Gender Equality at Home, by world region
#
#############################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')

vecSplit <- function(string, pattern, n) {
  sapply(strsplit(string, pattern), `[`, n)
}

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'

#if starting here:
all <- fread(paste0(in.dir, 'safety_microdata_harmonized_for_regs.csv'))[source=='FB GEAH']
all[, unsafe_at_home:=as.numeric(unsafe_at_home)]
all[location_name=='Bolivia', location_name:='Bolivia (Plurinational State of)']
all[location_name=='Vietnam', location_name:='Viet Nam']
all[location_name=='Taiwan', location_name:='Taiwan (Province of China)']
all[location_name=='Swaziland', location_name:='Eswatini']
all[location_name=='Czech Republic', location_name:='Czechia']
all[location_name=='Laos', location_name:="Lao People's Democratic Republic"]
all[location_name=='Macedonia', location_name:='North Macedonia']
all[location_name=='Moldova', location_name:='Republic of Moldova']
all[location_name=='Russia', location_name:='Russian Federation']
all <- merge(all, locs[ihme_loc_id!='USA_533', .(location_name, super_region_name, location_id)], all.x=T)
setnames(all, 'weight', 'survey_weight')

#fill in 'rest of ...' location names from FB GEAH
all[location_name=="Rest of East Asia & Pacific", super_region_name:="Southeast Asia, East Asia, and Oceania"]
all[location_name=="Rest of Sub-Saharan Africa", super_region_name:="Sub-Saharan Africa"]
all[location_name=="Rest of Europe and Central Asia" , super_region_name:="Central Europe, Eastern Europe, and Central Asia"]
all[location_name=="Rest of latin America and Caribbean", super_region_name:="Latin America and Caribbean"]
all[location_name=="Rest of Middle East and North Africa", super_region_name:="North Africa and Middle East"]
all[location_name=="Rest of South Asia", super_region_name:= "South Asia"]

covs <- c('female+rural+higher_edu')

for (sr in unique(all$super_region_name)){
  cof.list <- list()
  mod.list <-list()
  
  for (c in covs){
    print(c)
    #run glm
    mod <- glmer(as.formula(paste0('unsafe_at_home~', c, '+(1|location_name)')),
                 data=all[super_region_name==sr],
                 weights=NULL,
                 family=binomial(link='logit'))
    
    summary_mod <- summary(mod)
    cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                      outcome='unsafe_at_home',N=nobs(mod),covs=c,source='Facebook Gender Equality at Home', super_region_name=sr,
                      n_countries=length(unique(all[super_region_name==sr]$location_name)))
    cof[,OR:=exp(Estimate)]
    cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
    cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
    cof.list[[length(cof.list)+1]] <- cof
    mod.list[[paste0("unsafe_at_home_", c)]] <- mod
  }
  
  cofs <- rbindlist(cof.list, fill=T)
  
  #files
  write.csv(cofs, paste0(out.dir, 'unsafe_at_home/unweighted_model_', sr, '.csv'))
  saveRDS(mod.list, file=paste0(out.dir, 'unsafe_at_home/unweighted_model_', sr, '.rds'))
  
  #Table of regression results
  for (out in unique(cofs$outcome)) {
    list_names <- grep(out, names(mod.list), value = T)
    mini_list <- list()
    for (ln in list_names) {
      mini_list[[ln]] <- mod.list[[ln]]
    }
    stargazer(mini_list, type = "html", font.size = "small",
              title = paste0("Mixed effect logistic regression with random intercepts on country, estimates are not exponentiated outcome: ", out),
              omit = c("source*"), omit.labels = c("Source Vars"),
              out = paste0(out.dir, 'unsafe_at_home/table_unweighted_', sr, '.htm'))
  }
  
  print(Sys.time())
}

