#############################################################################################################
#
# Author: USERNAME
# Purpose: Logistic regressions for healthcare indicators from non-FB symptoms sources
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

# resubmission
plot.dir <- 'FILEPATH'
out.dir <- 'FILEPATH'
central.dir <- 'FILEPATH'

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
all <- all[!is.na(location_id)] 

for (s in unique(all$source)){
  print(s)
  
  #set covariate values based upon what is available per source; 
  #set most.covs as the list of covariates from most-adjusted model
  if (s == 'Facebook Gender Equality at Home'){
    covs <- c('female',
              'female+rural',
              'female+rural+higher_edu')
    most.covs <- 'female+rural+higher_edu'
  } else if (s %in% c('Goalkeepers', 'FinMark Trust')){
    covs <- c('female', 
              'female + age25to45',
              'female + age25to45 + age45more',
              'female + age25to45 + age45more + rural',
              'female + age25to45 + age45more + rural + higher_edu',
              'female + age25to45 + age45more + rural + higher_edu + date')
    most.covs <- 'female + age25to45 + age45more + rural + higher_edu + date'
  } else {
    covs <- c('female',
              'female+age35to64+age65plus',
              'female+age35to64+age65plus+rural',
              'female+age35to64+age65plus+rural+higher_edu')
    most.covs <- 'female+age35to64+age65plus+rural+higher_edu'
  }
  
  #loop through each indicator available per source
  for (o in unique(all[source==s]$variable)){
    print(o)
    
    #health_products indicator only available from goalkeepers 2021; remove date covariate set for goalkeepers above
    if (s=='Goalkeepers' & o=='health_products_covid'){
      covs <- c('female', 
                'female + age25to45',
                'female + age25to45 + age45more',
                'female + age25to45 + age45more + rural',
                'female + age25to45 + age45more + rural + higher_edu')
      most.covs <- 'female + age25to45 + age45more + rural + higher_edu'
    }
    
    if (s=='UN Women RGA' & o=='health_products_covid'){ #rural variable not available for countries with this indicator from UN Women RGA; remove here
      covs <- c('female',
                'female+age35to64+age65plus',
                'female+age35to64+age65plus+higher_edu')
      most.covs <- 'female+age35to64+age65plus+higher_edu'
    } 
    
    cof.list <- list()
    mod.list <-list()
  
    for (c in covs){
      print(c)
      
      #get list of covs to subset data to
      vars <- str_split(c, '\\+')
      cov.vars <- c()
      for (i in 1:length(vars[[1]])){
        cov.vars <- c(cov.vars, vars[[1]][i])
      }
      cov.vars <- str_trim(cov.vars)
      
      ip <- all[source==s & variable==o]
      ip <- ip[, .SD, .SDcols=c('source', 'variable', 'value', cov.vars, "location_name", "location_id")]
      ip <- ip[complete.cases(ip) & !is.infinite(value), ]

      #run glm
      mod <- glmer(as.formula(paste0('value~', c, '+(1|location_name)')),
                   data=ip,
                   family=binomial(link='logit'))
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome=o,N=nobs(mod),
                        N_male=dim(ip[female==0])[1],
                        N_female=dim(ip[female==1])[1],
                        covs=c,source=s,
                        n_countries=length(unique(ip$location_name)))
      cof[,OR:=exp(Estimate)]
      cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0(o, "_", c)]] <- mod
    }
    
    cofs <- rbindlist(cof.list, fill=T)
    
    #file
    write.csv(cofs, paste0(out.dir, o, '_', s, '_coefs_unweighted.csv'))
    saveRDS(mod.list, file=paste0(out.dir, o, '_', s, '.rds'))
    
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
                out = paste0(plot.dir, o, '_', s, '_unweighted.htm'))
    }
    
    print(Sys.time())
    
    #save coefficients from most-adjusted model
    reg.data <- cofs[covs==most.covs]
    write.csv(reg.data, paste0(central.dir, o, '_', s, '_coefs_unweighted.csv'))
    
  }
  
}
