#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                   Custom subnational GBD logit raking function                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' Modified from MBG calculate_raking_factors and apply_raking_factors functions
#' @param rake_targets List of raking targets with location and year
#' @param gbd_loc_id location id that matches name in rake_targets
#' @param cell_pred draws from subnational location
#' @param nyears number of years in data
#' @param year_list list of years in data
#' @param population vector of population previously pulled using get_population function
#' @param year_list integer vector of years
#' @param save_vec default `NULL`. If null, uses names from subnational_rake_output. Otherwise takes a character vector with the names of objects in subnational_rake_output to save.
#' @param save_draws_as character string default `RDS`. additional supported arguments `rds` and `Rdata`. The file type to save raked draws. If using `RDS` or `rds`, the file extension will be .RDS or .rds accordingly.
#'
#' @return raked "cell_pred"

gbd_raking_level4 <- function(rake_targets, gbd_loc_id, cell_pred, nyears, year_list, age_group_ids){
  
  lyv = c('name','year', 'ages', 'value')
  rake_targets = copy(rake_targets[, lyv, with = F])
  setnames(rake_targets, lyv, c('loc', 'year', 'age', 'target'))

  sr_locs <- c(gbd_loc_id)
  cyas = setDT(expand.grid(loc = sr_locs, year = year_list, age = age_group_ids))
  start_row = nrow(cyas)
  cyas = merge(cyas, rake_targets, by= c('loc','year', 'age'), all.x= T)

  raker=data.table(loc_means = rowMeans(cell_pred),
                   loc = rep.int(gbd_loc_id, nyears), 
                   year = as.vector(cell_pred$year_id), 
                   age = as.vector(cell_pred$age_group_id), 
                   weight = as.vector(cell_pred$population))
  
  cell_pred_no_labels <- copy(cell_pred)
  cell_pred_no_labels <- cell_pred_no_labels[, c('location_id', 'year_id', 'age_group_id', 'sex_id', 'population'):=NULL]

  raker[, cell_pred_id := .I]
  
  #specify by_vars
  byvars = c('loc', 'year', 'age')
  
  #remove NA weight values from raker
  pre = dim(raker)[1]
  post = dim(raker)[1]
  
  if(pre != post){
    warning(paste0(pre - post, ' (', scales::percent((pre - post)/pre), ') pixels (over the whole cube) were removed because of NA weight_brick or pixel values'))
  }
  
  #for each country year, find the logit raking factor
  #redefine cys
  cyas = unique(raker[,.(loc, year, age)])
  rf = lapply(seq(nrow(cyas)), function(x) {
    
    #year and location
    theloc = cyas[x,loc]
    theyear = cyas[x,year]
    theage = cyas[x,age]
    
    message(theloc, " in ", theyear, " age group: ", theage)
    
    if (nrow(rake_targets[loc == theloc & year == theyear & age==theage]) == 0) {
      if(if_no_gbd == "return_na"){
        return(NA)
      } else {
        return(0)
      }
    } else if (rake_targets[loc == theloc & year == theyear & age==theage, .(target)] == 0 & zero_heuristic == T) {
      # catch true zeroes (i.e. pre-introduction of an intervention) and return -9999. This will speed things up & will replace later with 0
      return(-9999)
    } else {
      ret <- try(LogitFindK(gbdval     = rake_targets[loc == theloc & year == theyear & age==theage,.(target)],
                            pixelval   = cell_pred_no_labels[raker[loc == theloc & year == theyear & age==theage, cell_pred_id],], #pass the cell pred rows (without ids attached, or else it will break!) that corrospond to this country year age
                            weightval  = raker[loc == theloc & year == theyear & age==theage, weight],
                            MaxJump    = MaxJump,
                            MaxIter    = MaxIter,
                            FunTol     = FunTol,
                            approx_0_1 = approx_0_1))
      return(ret)
    }
  })
  
  cyas[,raking_factor := unlist(rf)]
  cyas = merge(cyas, rake_targets, all.x = T, by =c('loc', 'year', 'age'))

  #calculate the starting point post hoc for standardized reporting
  rak = raker[, list(px_mean = sum(loc_means * weight, na.rm = T), sumweight = sum(weight, na.rm = T)), by = byvars]
  rak[, start_point := px_mean/sumweight]
  rak = merge(rak, cyas, all.x = T, by = c('loc','year', 'age'))
  
  #raking factors at the cyas level 
  rak = rak[, .(loc, year, age, start_point, target, raking_factor)]
  rake_dt <- rak
  
  message('apply raking factors')

  cpdim = dim(cell_pred)
  thelocs = unique(cell_pred$location_id)
  thelocs = thelocs[!is.na(thelocs)]
  dt = setDT(expand.grid(loc = thelocs, year = year_list, age = age_group_ids))
  
  #merge on the raking factors and rake
  rake_dt = merge(dt, rake_dt, all.x = T, by = c('year', 'age'))
  rake_dt[, loc:=loc.x]
  rake_dt[, c('loc.x', 'loc.y'):=NULL]
  raked_cell_pred <- merge(cell_pred, rake_dt, all=T, by.x=c('location_id', 'year_id', 'age_group_id'), by.y=c('loc', 'year', 'age')) %>% draws_to_long
  raked_cell_pred[, raked_value:=invlogit(logit(value)+raking_factor)]

  return(raked_cell_pred)
  
}

