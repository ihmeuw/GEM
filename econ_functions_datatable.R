#################################################################################################
# 
# Functions used in econ variable cleaning, summary plots and regressions for paper on the 
# the impacts of covid on gender, for datatable
#
# USERNAME
# 
#################################################################################################

library(tidyverse)
library(stargazer, lib.loc = 'FILEPATH') 

##### Helper functions ####
"%ni%" <- Negate("%in%")
misMat <- function(data, cols = NULL, all = T, notmissing = F) {
  if (is.null(cols)) {cols <- colnames(data)}
  if (any(cols %ni% colnames(data))) {
    nope <- cols[cols %ni% colnames(data)]
    stop(paste0(paste(nope, collapse = ", "), " not in dataset"))
  }
  m <- mapply(function(x) length(which(is.na(data[[x]]))), cols)
  if (notmissing == T) {
    m <- mapply(function(x) length(which(!is.na(data[[x]]))), cols)
  }
  if (all == F) {m <- m[m>0]}
  return(m)
}

vecSplit <- function(string, pattern, n) {
  sapply(strsplit(string, pattern), `[`, n)
}

#### Cacluating proportions function ####

#calculate prop (non-missing)
prop <- function(dat, var_list, wt, group_vec = c('gender', 'age_bin', 'qweek', 'location_name')) {
  outlist <- list()
  for (var in var_list){
    dt <- as.data.table(dat)
    dt[, paste0(var):=as.numeric(get(var))]
    dt <- dt[!get(var) %in% c(NA, NaN)]
    dt[, weighted_var:=get(var)*get(wt)]
    #get sample size
    denom <- dt[, .(denom=sum(get(wt), na.rm = T)), by=group_vec]
    num_obs <- dt[, .(num_obs=.N), by=group_vec]
    #get num who said yes 
    n <- dt[, .(n=sum(weighted_var, na.rm = T)), by=c(group_vec, paste0(var))][get(var)==1]
    #calculate prop and se
    props <- merge(n, denom, by=group_vec)[, paste0(var):=NULL]
    props <- merge(props, num_obs, by=group_vec)
    props <- props[num_obs>30]
    props[, prop:=n/denom]
    props[, se:=sqrt((prop*(1-prop))/num_obs)]
    props[, prop_upper:=prop+(1.96*se)]
    props[, prop_lower:=prop-(1.96*se)]
    props[, type:=paste0(var)]
    props[, prop_ans:='yes']
    outlist[[var]] <- props
    print(paste0("finished ", var))
  }
  return(outlist)
}

##### Collapsing function, for big data regressions ####

collapseFun <- function(data, covs, outcomes, logit = F, prop = F) {
  out1 <- outcomes[1]
  if (prop == F) {
    covs1 <- c(covs, out1)
  }
  d <- data %>% 
    filter(!is.na(get(out1))) %>% 
    group_by_at(vars(all_of(c("location_name", covs1, "source")))) %>%
    summarise(outip = mean(get(out1)), weight = sum(weight), n = n())
  if (prop == F) {
    d$outip <- NULL
  } else if (prop == T) {
    colnames(d) <- gsub("outip", out, colnames(d))
  }
  if (length(outcomes) > 1) {
    for (out in outcomes[2:length(outcomes)]) {
      if (prop == F) {
        covs1 <- c(covs, out)
      }
      dip <- data %>%
        filter(!is.na(get(out))) %>% 
        group_by_at(vars(all_of(c("location_name", covs1, "source")))) %>%
        summarise(outip = mean(get(out)), weight = sum(weight), n = n())
      if (prop == F) {
        dip$outip <- NULL
      } else if (prop == T) {
        colnames(dip) <- gsub("outip", out, colnames(dip))
      }
      d <- as.data.frame(bind_rows(d, dip))
    }
  }
  
  if (logit == T) {
    d[, outcomes] <- apply(d[, outcomes], 2, logit)
  }
  return(d)
}

#### Regression functions ####

#Runs regressions
doRegs <- function(dat, covs, outcomes, source, weighted = F) {
  cof.list <- list()
  mod.list <-list()
  for (v in outcomes){
    print(paste0("OUTCOME: ", v))
    for (c in covs){
      skip <- 0
      print(paste0("MODEL: ", c))
      s <- c()
      # For all
      if (source == "All") {
        if (grepl("\\*", c)) {
          cip <- gsub("\\*", " \\+ ", c)
        } else {
          cip <- c
        }
        for (n in 1:length(str_split(cip, "\\+")[[1]])){
          cip2 <- vecSplit(cip, " \\+ ", n)
          # cip <- vecSplit(c, " \\+ ", n)
          s <- c(s, cip2)
        }
        #s <- unique(s)
        ip <- dat[, c(v, s, "location_name")]
        ip <- ip[complete.cases(ip) & !is.infinite(ip[[v]]), ]
        if (length(unique(ip[["source"]])) < 2) {
          c <- gsub(" \\+ source", "", c)
          s <- s[s != "source"]
        }
        # For by source
      } else if (source != "All") {
        if (source %ni% c(unique(dat$source), "All")) {
          stop(print("Select a source or All"))
        }
        if (grepl("\\*", c)) {
          cip <- gsub("\\*", " \\+ ", c)
        } else {
          cip <- c
        }
        for (n in 1:length(str_split(cip, "\\+")[[1]])){
          cip2 <- vecSplit(cip, " \\+ ", n)
          s <- c(s, cip2)
          c <- gsub(" \\+ source", "", c)
        }
        s <- s[s != "source" & !is.na(s)]
        ip <- dat[dat$source == source]
        ip <- ip[, .SD, .SDcols=c(v, s, "location_name", "weight")]
        ip <- ip[complete.cases(ip) & !is.infinite(ip[[v]]), ]
      }
      # Common for both
      if (nrow(ip) == 0) {
        skip <- 1
      }
      if (length(unique(ip[[v]])) < 2) {
        skip <- 1
      }
      for (var in s) {
        if (length(unique(ip[[var]])) < 2){
          skip <- 1
        }
      }
      if (length(unique(ip[['location_name']])) < 2) {
        skip <- 1
      }
      if (skip == 1) {
        print("Skipped")
      } else if (skip == 0) {
        #run glmer
        if (length(unique(ip[[v]])) == 2) {
          if (weighted == T) {
            mod <- glmer(as.formula(paste0(v, '~', c, '+ (1|location_name)')),
                         weights = weight,
                         data=ip,
                         family=binomial(link='logit'))
          } else {
            mod <- glmer(as.formula(paste0(v, '~', c, '+ (1|location_name)')),
                         weights = NULL,
                         data=ip,
                         family=binomial(link='logit'))
          }
        } else if (length(unique(ip[[v]]) > 2)) {
          if (weighted == T) {
            mod <- glmer(as.formula(paste0(v, '~', c, '+ (1|location_name)')),
                         weights = weight,
                         data=ip)
          } else {
            mod <- glmer(as.formula(paste0(v, '~', c, '+ (1|location_name)')),
                         weights = NULL,
                         data=ip)
          }
        }
        summary_mod <- summary(mod)
        cof <- data.table(variable=row.names(summary_mod$coefficients),summary_mod$coefficients,outcome=v,N=nobs(mod),
                          N_male=dim(ip[female==0])[1],
                          N_female=dim(ip[female==1])[1],
                          covs=c, source_dat = source,
                          n_countries=length(unique(ip$location_name)))
        cof[,OR:=exp(Estimate)]
        cof[,OR_95lwr:=exp(Estimate-(1.96*`Std. Error`))]
        cof[,OR_95upr:=exp(Estimate+(1.96*`Std. Error`))]
        cof[,OR_90lwr:=exp(Estimate-(1.64*`Std. Error`))]
        cof[,OR_90upr:=exp(Estimate+(1.64*`Std. Error`))]
        cof.list[[length(cof.list)+1]] <- cof
        
        mod.list[[paste0(v, "_", c, "_", source)]] <- mod
      }
    }
  }
  cofs <- rbindlist(cof.list, fill=T)
  return(list("mod.list" = mod.list, "cofs" = cofs))
}
