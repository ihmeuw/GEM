#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions from UN Women RGA, non-healthcare indicators
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
plot.dir <- 'FILEPATH'
in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'
appendix.dir <- 'FILEPATH'
dir.create(appendix.dir)
main.dir <- 'FILEPATH'

weighted <- 'unweighted' 

#main data 
main <- fread(paste0(in.dir, 'cleaned_data/rga_all_data_cleaned.csv'))
setnames(main, c('sex', 'weight'), c('gender', 'survey_weight'))
main[, location_id:=NULL]
main <- merge(main, locs[, .(location_id, ihme_loc_id)], by='ihme_loc_id', main.x=T)

for (topic in c('community_gbv_change', 'unsafe_at_home', 'srhc_covid_among_need')){

  print(topic)
  if (topic=='community_gbv_change'){
    
    rga.gbv.locs <- unique(main[!is.na(community_gbv_change)]$location_name)
    all <- main[location_name %in% rga.gbv.locs]
    all <- all[, .SD, .SDcols=c(topic, 'female', 'higher_edu', 'rural', 'age35to64', 'age65plus',
                              "location_name", "super_region_name", 'location_id')]
    all <- all[complete.cases(all) & !is.infinite(all[[topic]]), ]
    
    
    n_obs <- all[, .(total_obs=.N), by=.(location_id, female)]
    n_obs[, sex_id:=ifelse(female==0,1, ifelse(female==1,2,NA))]
    weights <- merge(adult_pop, n_obs, by=c('location_id', 'sex_id'), main.y=T)
    weights[, weight:=population/total_obs]
    weights <- weights[sex_id %in% c(1,2)]
    ratio <- sum(weights$total_obs)/sum(weights$weight)
    weights[, weight:=weight*ratio]
    all <- merge(all, weights[, .(location_id, female, weight)], by=c('location_id', 'female'), main.x=T, mainow.cartesian=T)

    covs <- c('female',
              'female+age35to64+age65plus',
              'female+age35to64+age65plus+higher_edu',
              'female+age35to64+age65plus+higher_edu+rural')

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
                        outcome='community_gbv_change',N=nobs(mod),
                        N_male=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age35to64) & !is.na(age65plus) & !is.na(higher_edu) & female==0])[1],
                        N_female=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age35to64) & !is.na(age65plus) & !is.na(higher_edu) & female==1])[1],
                        covs=c,source='UN Women RGA',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("community_gbv_change_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, '/', topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, '/', topic, '_UN Women RGA_', weighted, '.rds'))
    
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
                out = paste0(plot.dir, out, '_UN Women RGA_', weighted, '.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most adjusted model
    reg.data <- cofs[covs=='female+age35to64+age65plus+higher_edu+rural']
    write.csv(reg.data, paste0(main.dir, topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    write.csv(reg.data, paste0(appendix.dir, topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    
  } else if (topic=='unsafe_at_home'){
    
    rga.safe.locs <- unique(main[!is.na(unsafe_at_home)]$location_name)
    all <- main[location_name %in% rga.safe.locs]
    all <- all[, .SD, .SDcols=c(topic, 'female', 'higher_edu', 'rural', 'age35to64', 'age65plus',
                                "location_name", "super_region_name", 'location_id')]
    all <- all[complete.cases(all) & !is.infinite(all[[topic]]), ]

    covs <- c('female',
              'female+age35to64+age65plus',
              'female+age35to64+age65plus+higher_edu',
              'female+age35to64+age65plus+higher_edu+rural')
    
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
                        N_male=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age35to64) & !is.na(age65plus) & !is.na(higher_edu) & female==0])[1],
                        N_female=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age35to64) & !is.na(age65plus) & !is.na(higher_edu) & female==1])[1],
                        covs=c,source='UN Women RGA',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("unsafe_at_home_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, '/', topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, '/', topic, '_UN Women RGA_', weighted, '.rds'))
    
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
                out = paste0(plot.dir, out, '_UN Women RGA_', weighted, '.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most adjusted model
    reg.data <- cofs[covs=='female+age35to64+age65plus+higher_edu+rural']
    write.csv(reg.data, paste0(appendix.dir, topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    
  } else if (topic=='srhc_covid_among_need'){
    
    #(1) Main regression, using standard covariate bins ----------------------------------------------------------------------------------------------------------------------------------------
    
    all <- main[age45more==0]  # remove above 45 for srhc
    rga.srhc.locs <- unique(all[!is.na(srhc_covid_among_need)]$location_name)
    all <- all[location_name %in% rga.srhc.locs]
    all <- all[, .SD, .SDcols=c(topic, 'female', 'higher_edu', 'rural', 'age25to45', 'age', 'urban_bin', 'edu_dummy',
                                "location_name", "super_region_name", 'location_id')]
    all <- all[complete.cases(all) & !is.infinite(all[[topic]]), ]

    #set more-detailed, tri-category urbanicity
    all[urban_bin %in% c('Big city', 'Capital city/Big city', 'Capital city'), urban_bin:='Capital city']
    all[urban_bin %in% c('Small city/Small town', 'Other city', 'Small town'), urban_bin:='Non-capital city']
    all[urban_bin %in% c('Village/Rural', 'Village/rural', 'Rural'), urban_bin:='Rural']
    
    #set rural as the referent
    all[, urban_bin:=factor(urban_bin, levels=c('Rural', 'Non-capital city', 'Capital city'))]
    
    covs <- c('female', 
              'female + age25to45',
              'female + age25to45 + rural',
              'female + age25to45 + rural + higher_edu')
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      print(c)
      #run glm
      mod <- glmer(as.formula(paste0(topic, '~', c, '+(1|location_name)')),
                   data=all,
                   family=binomial(link='logit'))
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome=topic,N=nobs(mod),
                        N_male=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(higher_edu) & female==0])[1],
                        N_female=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(higher_edu) & female==1])[1],
                        covs=c,source='UN Women RGA',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0(topic, "_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, '/', topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, '/', topic, '_UN Women RGA_', weighted, '.rds'))
    
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
                out = paste0(plot.dir, out, '_UN Women RGA_', weighted, '.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most adjusted model
    reg.data <- cofs[covs=='female + age25to45 + rural + higher_edu']
    write.csv(reg.data, paste0(main.dir, topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    write.csv(reg.data, paste0(appendix.dir, topic, '_UN Women RGA_coefs_', weighted, '.csv'))
    
    #(2) Appendix regression, using more detailed bins ----------------------------------------------------------------------------------------------------------------------------------------
    
    #set referent group for age/education
    all[, age:=factor(age, levels=c('25-34', '18-24', '35-44'))]
    all[, edu_dummy:=factor(edu_dummy, levels=c('Secondary', 'None', 'Primary', 'Tertiary or higher'))]

    #covariates with more granular age/education/urbanicity bins
    covs <- c('female',
              'female+age',
              'female+age+urban_bin',
              'female+age+urban_bin+edu_dummy')
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      print(c)
      #run glm
      mod <- glmer(as.formula(paste0('srhc_covid_among_need~', c, '+(1|location_name)')),
                     data=all,
                     family=binomial(link='logit'))
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome='srhc_covid_among_need',N=nobs(mod),
                        N_male=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(higher_edu) & female==0])[1],
                        N_female=dim(all[!is.na(get(topic)) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(higher_edu) & female==1])[1],
                        covs=c,source='UN Women RGA',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("srhc_covid_among_need_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, '/', topic, '_detailed_reg_UN Women RGA_coefs_', weighted, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, '/', topic, '_detailed_reg_UN Women RGA_', weighted, '.rds'))
    
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
                out = paste0(plot.dir, out, '_detailed_reg_UN Women RGA_', weighted, '.htm'))
    }
    
    #save standardized coefs centrally
    rga.cofs <- cofs[covs=='female+age+urban_bin+edu_dummy']
    write.csv(rga.cofs, paste0(appendix.dir, topic, '_detailed_reg_UN Women RGA_coefs_', weighted, '.csv'))
    
    # (3) Female only subset ----------------------------------------------------------------------------------
    
    #covariates with more granular age/education bins
    covs <- c('age',
              'age+urban_bin',
              'age+urban_bin+edu_dummy')
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      print(c)
      #run glm
      mod <- glmer(as.formula(paste0('srhc_covid_among_need~', c, '+(1|location_name)')),
                     data=all[female==1],
                     family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome='srhc_covid_among_need',N=nobs(mod),covs=c,source='UN Women RGA',
                        n_countries=length(unique(all$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0("srhc_covid_among_need_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, '/', topic, '_females_only_UN Women RGA_coefs_', weighted, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, '/', topic, '_females_only_UN Women RGA_', weighted, '.rds'))
    
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
                out = paste0(plot.dir, out, '_females_only_UN Women RGA_', weighted, '.htm'))
    }
    
    #save standardized coefs centrally
    rga.cofs <- cofs[covs=='age+urban_bin+edu_dummy']
    write.csv(rga.cofs, paste0(appendix.dir, topic, '_females_only_UN Women RGA_coefs_', weighted, '.csv'))
    
  } 
  
}
