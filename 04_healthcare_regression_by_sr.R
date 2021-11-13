#############################################################################################################
#
# Author: USERNAME
# Purpose: Run non-FB healthcare source regressions by world region
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
out.dir <- 'FILEPATH'
central.dir <- 'FILEPATH'
dir.create(plot.dir, recursive=T)
dir.create(out.dir, recursive=T)

#resubmission
all <- fread('FILEPATH')
all <- all[variable %in% c('healthcare_covid', 'medication_covid', 'health_products_covid')]
setnames(all, 'yearmonth', 'date')
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
all <- all[!is.na(location_id)] #removes 'rest of...' locations from FB GEAH

#fill in 'rest of ...' location names from FB GEAH
all[location_name=="Rest of East Asia & Pacific", super_region_name:="Southeast Asia, East Asia, and Oceania"]
all[location_name=="Rest of Sub-Saharan Africa", super_region_name:="Sub-Saharan Africa"]
all[location_name=="Rest of Europe and Central Asia" , super_region_name:="Central Europe, Eastern Europe, and Central Asia"]
all[location_name=="Rest of latin America and Caribbean", super_region_name:="Latin America and Caribbean"]
all[location_name=="Rest of Middle East and North Africa", super_region_name:="North Africa and Middle East"]
all[location_name=="Rest of South Asia", super_region_name:= "South Asia"]

#remove FinMRK; not enough countries for regional analysis
all <- all[!source %in% c('FinMark Trust')]

for (s in unique(all$source)){
  print(s)
  
  #set covariate values based upon what is available per source; 
  #set most.covs as the list of covariates from most-adjusted model
  if (s == 'Facebook Gender Equality at Home'){
    covs <- c('female+rural+higher_edu')
    most.covs <- 'female+rural+higher_edu'
  } else if (s %in% c('Goalkeepers')){
    covs <- c('female + age25to45 + age45more + rural + higher_edu + date')
    most.covs <- 'female + age25to45 + age45more + rural + higher_edu + date'
  } else {
    covs <- c('female+age35to64+age65plus+rural+higher_edu')
    most.covs <- 'female+age35to64+age65plus+rural+higher_edu'
  }
  
  #loop through each indicator available per source
  for (o in unique(all[source==s]$variable)){
    print(o)
    dir.create(paste0(out.dir, s, '/', o), recursive = T)
    dir.create(paste0(plot.dir, s, '/', o), recursive = T)
    
    #health_products indicator only available from goalkeepers 2021; remove date covariate set for goalkeepers above
    if (s=='Goalkeepers' & o=='health_products_covid'){
      covs <- c('female + age25to45 + age45more + rural + higher_edu')
      most.covs <- 'female + age25to45 + age45more + rural + higher_edu'
    }
    
    if (s=='UN Women RGA' & o=='health_products_covid'){ #rural variable not available for countries with this indicator from UN Women RGA; remove here
      covs <- c('female+age35to64+age65plus+higher_edu')
      most.covs <- 'female+age35to64+age65plus+higher_edu'
      all <- all[location_name!='Turkey']
    } 
    
    #loop through each super-region
    for (sr in unique(all[source==s & variable==o]$super_region_name)){
      print(sr)
      
      cof.list <- list()
      mod.list <-list()
      
      for (c in covs){
        print(c)
        #run glm
        mod <- glmer(as.formula(paste0('value~', c, '+(1|location_name)')),
                     data=all[source==s & variable==o & super_region_name==sr],
                     family=binomial(link='logit'))
        summary_mod <- summary(mod)
        cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                          outcome=o,N=nobs(mod),covs=c,source=s,super_region=sr,
                          n_countries=length(unique(all[source==s & variable==o & super_region_name==sr]$location_name)))
        cof[,OR:=exp(Estimate)]
        cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
        cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
        cof.list[[length(cof.list)+1]] <- cof
        mod.list[[paste0(o, "_", c)]] <- mod
      }
      
      cofs <- rbindlist(cof.list, fill=T)
      
      #file
      write.csv(cofs, paste0(out.dir, s, '/', o, '/unweighted_model_', sr, '.csv'))
      saveRDS(mod.list, file=paste0(out.dir, s, '/', o, '/unweighted_model_', sr, '.rds'))
      
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
                  out = paste0(plot.dir, s, '/', o,'/unweighted_', sr, '.htm'))
      }
    }
  }
}
