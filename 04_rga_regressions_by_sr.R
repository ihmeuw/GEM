#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for UN Women RGA non-healthcare indicators, by world region
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

#resubmission
in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'
central.dir <- 'FILEPATH'

all <- fread(paste0(in.dir, 'cleaned_data/rga_all_data_cleaned.csv'))
setnames(all, c('sex'), c('gender'))

outcomes <- c('unsafe_at_home', 'community_gbv_change')
covs <- 'female+age35to64+age65plus+higher_edu+rural'

#loop through regressions by outcome and super-region
for (v in outcomes){
  print(v)
  
  mod.list <- list()
  cof.list <- list()
  
  #subset to locs that have this indicator avail
  rga.locs <- unique(all[!is.na(get(v))]$location_name)
  all.sub <- all[location_name %in% rga.locs]
  
  #loop through SRs
  for (sr in unique(all.sub$super_region_name)){
    print(sr)
    ip <- all.sub[super_region_name==sr]
    
    if ((v=='unsafe_at_home' & sr=='Latin America and Caribbean') | (v=='community_gbv_change' & sr=="Central Europe, Eastern Europe, and Central Asia")){ #no urbanicity info in these countries
      covs <- 'female+age35to64+age65plus+higher_edu'
      ip <- ip[, .SD, .SDcols=c(v, 'female', 'higher_edu',
                                "location_name", "super_region_name", 
                                'age35to64', 'age65plus')]
    } else {
      covs <- 'female+age35to64+age65plus+higher_edu+rural'
      ip <- ip[, .SD, .SDcols=c(v, 'female', 'higher_edu', 'rural',
                                "location_name", "super_region_name", 
                                'age35to64', 'age65plus')]
    }
    
    #subset data and fit model
    ip <- ip[complete.cases(ip) & !is.infinite(ip[[v]]), ]
      
      mod <- glmer(as.formula(paste0(v, '~', covs, '+ (1|location_name)')),
                   weights = NULL,
                   data=ip,
                   family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome=v,N=nobs(mod),covs=covs, source_dat = 'UN Women RGA',
                        n_countries=length(unique(ip$location_name)),
                        super_region_name=sr)
      
      cof[,OR:=exp(Estimate)]
      cof[,OR_95lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_95upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0(v, "_", sr)]] <- mod
  }
  
  #save
  cofs <- rbindlist(cof.list, fill=T)
  dir.create(paste0(out.dir, v, '/'), showWarnings=F)
  write.csv(cofs, paste0(out.dir, v, '/unweighted_model_coefficients_by_super_region.csv'), row.names=F)
  saveRDS(mod.list, file=paste0(out.dir, v, '/unweighted_model_objects_by_super_region.rds'))
  
  #table of results by super-region
  stargazer(mod.list, type = "html", font.size = "small",
            title = paste0("Mixed effect logistic regression with random intercepts on country, estimates are not exponentiated outcome: ", v),
            omit = c("source*"), omit.labels = c("Source Vars"),
            column.labels = str_sub(names(mod.list), nchar(v)+2, -1),
            out = paste0(out.dir, v, '/', 'UN Women RGA_', v, '_regression_table_by_super_region.htm'))
  
}
  
  
 