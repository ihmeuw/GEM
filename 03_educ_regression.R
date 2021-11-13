#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for education indicators
#
#############################################################################################################

rm(list = ls())

pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4)
aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text =element_text(size=13,face='bold'),plot.title =element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')

in.root <- "FILEPATH"
out.root <- "FILEPATH"

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')

#--metadata
source("FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

# ------------------------------------------------------------------------------------------------------------------------------

# set dirs
in.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'
plot.dir <- 'FILEPATH'
main.dir <- 'FILEPATH'

#--load data
all <- fread(paste0(in.dir, 'edu_data_prepped_for_regs.csv'))

#set covs
covs <- c('child_female',
          'child_female + female',
          'child_female + female + age25to45',
          'child_female + female + age25to45 + age45more',
          'child_female + female + age25to45 + age45more + rural',
          'child_female + female + age25to45 + age45more + rural + higher_edu')

most.covs <- 'child_female + female + age25to45 + age45more + rural + higher_edu'
        
#regressions -- importance of gender by parental education; 2 indicators
for (c.o in c('dropout', 'good_internet')){
  
  cof.list <- list()
  mod.list <-list()
  
  for (c in covs){
    
    print(c)
    ip <- all[variable==c.o]
    ip <- ip[, .SD, .SDcols=c('value', 'female', 'higher_edu', 'rural', 'child_female', 'age25to45', 'age45more',
                              "location_name", "super_region_name", 'location_id')]
    ip <- ip[complete.cases(ip) & !is.infinite(ip[['value']]), ]

    #run glm
    mod <- glmer(as.formula(paste0('value~', c, '+ (1|location_name)')),
                 data=ip,
                 family=binomial(link='logit'))
    
    summary_mod <- summary(mod)
    cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                      outcome=c.o,N=nobs(mod),
                      N_male=dim(all[variable==c.o & !is.na(value) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(age45more) & !is.na(child_female) & !is.na(higher_edu) & female==0])[1],
                      N_female=dim(all[variable==c.o & !is.na(value) & !is.na(female) & !is.na(rural) & !is.na(age25to45) & !is.na(age45more) & !is.na(child_female) &!is.na(higher_edu) & female==1])[1],
                      covs=c,source='Goalkeepers',
                      n_countries=length(unique(all[variable==c.o]$location_name)))
    cof[,OR:=exp(Estimate)]
    cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
    cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
    cof.list[[length(cof.list)+1]] <- cof
    mod.list[[paste0(c.o, c)]] <- mod
  }
  
  cofs <- rbindlist(cof.list, fill=T)
  
  #file
  write.csv(cofs, paste0(out.dir, c.o, '_unweighted_model.csv'))
  saveRDS(mod.list, file=paste0(out.dir, c.o, '_unweighted_model.rds'))
  
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
              out = paste0(plot.dir, out, '_table_unweighted.htm'))
  }

  print(Sys.time())
  
  reg.data <- cofs[covs==most.covs]
  write.csv(reg.data, paste0(main.dir, c.o, '_coefs_unweighted.csv'))

}

