#################################################################################################
# 
# Logistic regressions for FB symptoms data, by world region
#
# USERNAME
#
#################################################################################################

# (1) Setup -------------------------------------------------------------------------------------
rm(list = ls())

start_run <- Sys.time()

library(data.table)
library(tidyverse)
library(ggplot2)
library(lme4)
library(stargazer)
library(gtools)
source('FILEPATH/econ_functions_datatable.R')

# Important cluster functions
invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, release_id = 9)
run_date <- gsub('-', '_', Sys.Date())

# directories
out.dir <- 'FILEPATH'
plot.dir <- 'FILEPATH'

# (1) Read in pre-prepped data -----------------------------------------------------------------------------------------

fb_microdata <- readRDS('FILEPATH')

#for all indicators, estimate results since April
fbsinceApr <- filter(fb_microdata, as.Date(date) > as.Date('2021-03-31'))
fbsinceApr$weight_og <- NULL
fbsinceApr$weight <- NULL

# (2) Run regression by super-region for econ/health/vax inds ----------------------------------------------------------------------------------------------------------------

outcomes <- c('emp_lost_combined', 'notworking_careothers', 'healthcare_covid',
              'preventative_health_covid', 'health_products_covid', 'medication_covid')
covs = c('female + age35to64 + age65plus + higher_educ + rural')
vax_outcomes <- c('fullvax', 'hesitancy', 'V1')
all_outcomes <- c(outcomes, vax_outcomes)

#loop through regressions by outcome and super-region
for (v in all_outcomes){
  print(v)
  
  mod.list <- list()
  cof.list <- list()
  for (sr in unique(fbsinceApr$super_region_name)){
    print(sr)
    
      #subset data and fit model
      ip <- fbsinceApr[super_region_name==sr]
      ip <- ip[, .SD, .SDcols=c(v, 'female', 'higher_educ', 'rural',
                                "location_name", "super_region_name", "weight",
                                'age35to64', 'age65plus')]
      ip <- ip[complete.cases(ip) & !is.infinite(ip[[v]]), ]
      
      mod <- glmer(as.formula(paste0(v, '~', covs, '+ (1|location_name)')),
                   weights = NULL,
                   data=ip,
                   family=binomial(link='logit'))
      
      summary_mod <- summary(mod)
      cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,
                        outcome=v,N=nobs(mod),covs=covs, source_dat = 'FB Symptoms',
                        n_countries=length(unique(ip$location_name)),
                        super_region_name=sr)
      cof[,OR:=exp(Estimate)]
      cof[,OR_95lwr:=exp(Estimate-(1.96*`Std. Error`))]
      cof[,OR_95upr:=exp(Estimate+(1.96*`Std. Error`))]
      cof.list[[length(cof.list)+1]] <- cof
      mod.list[[paste0(v, "_", sr)]] <- mod
    }
  # }
  
  #save
  cofs <- rbindlist(cof.list, fill=T)
  dir.create(paste0(out.dir, v, '/'), showWarnings=F)
  dir.create(paste0(plot.dir, v, '/'), recursive=T, showWarnings = F)
  write.csv(cofs, paste0(out.dir, v, '_fbsinceApr_unweighted_model_coefficients_by_super_region.csv'), row.names=F)
  saveRDS(mod.list, file=paste0(out.dir, v, '_fbsinceApr_unweighted_model_objects_by_super_region.rds'))
  
  #table of results by super-region
  stargazer(mod.list, type = "html", font.size = "small",
            title = paste0("Mixed effect logistic regression with random intercepts on country, estimates are not exponentiated outcome: ", v),
            omit = c("source*"), omit.labels = c("Source Vars"),
            column.labels = str_sub(names(mod.list), nchar(v)+2, -1),
            out = paste0(plot.dir, v, '/regression_table_by_super_region.htm'))
}

