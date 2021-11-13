#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for education indicators, by world region
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
library(stargazer, lib.loc='/homes/corycns/')

#--metadata
source("FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

# ------------------------------------------------------------------------------------------------------------------------------

#resubmission
out.dir <- 'FILEPATH'
plot.dir <- 'FILEPATH'
all <- fread('FILEPATH')
all <- merge(all, locs[, .(location_name, super_region_name)], all.x=T)

#set covs
covs <- c('child_female + female + age25to45 + age45more + rural + higher_edu')
 
#regressions
for (c.o in c('dropout', 'good_internet')){
  print(c.o)
  
  for (sr in unique(all[variable==c.o & super_region_name!="Central Europe, Eastern Europe, and Central Asia"]$super_region_name)){
    print(sr)
    dir.create(paste0(out.dir, c.o), recursive = T)
    
    cof.list <- list()
    mod.list <-list()
    
    for (c in covs){
      
      print(c)
      
      #run glm
      mod <- glmer(as.formula(paste0('value~', c, '+ (1|location_name)')),
                   data=all[variable==c.o & super_region_name==sr],
                   # weights=weight,
                   family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome=c.o,N=nobs(mod),covs=c,source='Goalkeepers',super_region_name=sr, 
                        n_countries=length(unique(all[variable==c.o & super_region_name==sr]$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0(c.o, c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    dir.create(paste0(plot.dir, c.o), showWarnings = F, recursive=T)
    write.csv(cofs, paste0(out.dir, c.o, '/unweighted_model_', sr, '.csv'))
    saveRDS(mod.list, file=paste0(out.dir, c.o, '/unweighted_model_', sr, '.rds'))
    
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
                out = paste0(plot.dir, c.o, '/unweighted_', sr, '.htm'))
    }
    
    print(Sys.time())
    
    }
  
}

