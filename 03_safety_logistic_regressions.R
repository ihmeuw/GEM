#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for unsafe at home and community gbv increase from non-RGA sources
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


for (topic in c('safety', 'community_gbv_change')){
  
  if (topic=='safety'){
    
    #dirs
    plot.dir <- 'FILEPATH'
    out.dir <- 'FILEPATH'
    appendix.dir <- 'FILEPATH'
    main.dir <- 'FILEPATH'
    in.dir <- 'FILEPATH'
    
    #if starting here:
    all <- fread(paste0(in.dir, 'safety_microdata_harmonized_for_regs.csv'))[source=='FB GEAH']
    all[, unsafe_at_home:=as.numeric(unsafe_at_home)]
    all <- merge(all, locs[, .(location_name, location_id)], by='location_name', all.x=T, allow.cartesian=T)
    setnames(all, 'weight', 'survey_weight')
    
    all <- all[, .SD, .SDcols=c('unsafe_at_home', 'female', 'higher_edu', 'rural', 
                              "location_name", 'location_id')]
    all <- all[complete.cases(all) & !is.infinite(all[['unsafe_at_home']]), ]

    #covs
    covs <- c('female',
              'female+rural',
              'female+rural+higher_edu')
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      print(c)
      #run glm
      mod <- glmer(as.formula(paste0('unsafe_at_home~', c, '+(1|location_name)')),
                   data=all,
                   family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome='unsafe_at_home',N=nobs(mod),
                        N_male=dim(all[female==0 & !is.na(rural) & !is.na(higher_edu)])[1],
                        N_female=dim(all[female==1 & !is.na(rural) & !is.na(higher_edu)])[1],
                        covs=c,source='Facebook Gender Equality at Home',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("unsafe_at_home_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #files
    write.csv(cofs, paste0(out.dir, 'unsafe_at_home_Facebook Gender Equality at Home_coefs_unweighted.csv'))
    saveRDS(mod.list, file=paste0(out.dir, 'unsafe_at_home_Facebook Gender Equality at Home_unweighted.rds'))
    
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
                out = paste0(plot.dir, 'unsafe_at_home_Facebook Gender Equality at Home_unweighted.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most-adjusted model centrally 
    geah.cofs <- cofs[covs=='female+rural+higher_edu']
    write.csv(geah.cofs, paste0(main.dir, 'unsafe_at_home_Facebook Gender Equality at Home_coefs_unweighted.csv'))

  } else if (topic=='community_gbv_change'){
    
    #dirs
    plot.dir <- 'FILEPATH'
    out.dir <- 'FILEPATH'
    appendix.dir <- 'FILEPATH'
    main.dir <- 'FILEPATH'
    
    #if starting here:
    all <- fread(paste0('FILEPATH'))
    all[, community_gbv_change:=as.numeric(community_gbv_change)]
    all <- merge(all, locs[, .(location_name, location_id)], by='location_name', all.x=T, allow.cartesian=T)
    all[, female:=1] #female only sample
    
    all <- all[, .SD, .SDcols=c('community_gbv_change', 'female', 'higher_edu', 'rural', 'age25to45', 'age45more',
                                "location_name", 'location_id')]
    all <- all[complete.cases(all) & !is.infinite(all[[topic]]), ]

    #set covariates
    covs <- c('age25to45',
              'age25to45 + age45more',
              'age25to45 + age45more + rural',
              'age25to45 + age45more + rural + higher_edu')
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      
      print(c)
      #run glm
      mod <- glmer(as.formula(paste0('community_gbv_change~', c, '+(1|location_name)')),
                   data=all,
                   family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome='community_gbv_change',N=nobs(mod),covs=c,source='Goalkeepers',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("community_gbv_change_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, 'unweighted_model_community_gbv_change_standard_bins.csv'))
    saveRDS(mod.list, file=paste0(out.dir, 'unweighted_community_gbv_change_standard_bins.rds'))
    
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
                out = paste0(plot.dir, out, '_Goalkeepers_table_unweighted_standard_bins.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most adjusted model in central location
    reg.data <- cofs[covs=='age25to45 + age45more + rural + higher_edu']
    write.csv(reg.data, paste0(main.dir, '/community_gbv_change_Goalkeepers_coefs_unweighted.csv'))
    
  } 
}

