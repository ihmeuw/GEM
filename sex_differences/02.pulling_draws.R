## HEADER #################################################################
# Date: 9/8/2023
# Purpose: Pulling draws for custom metrics
#          

## SET-UP #################################################################
rm(list=ls())

# Load packages, and install if missing ========================================================================
packages <- c("data.table","tidyverse","ggplot2", "haven", "parallel", "gridExtra", "stringr")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

# Read in shared functions =================================================
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
# Set parameters ===========================================================
gbd.version <- 7    
gbd.step <- "iterative"
release <- 9
compare_version <- 7987
daly_version <- 76
yld_version <- 1471
yll_version <- 363

ages <- c(1099, 1024, 2549, 5069, 7099)
years <- seq(1990, 2021, 1)
measures <- c(2, 3, 4)
save_data <- T

# Set filepaths to save files ==============================================
data_fp <- "FILEPATH"
out_fp <- "FILEPATH"

## SCRIPT ##################################################################
top_causes <- fread(paste0(data_fp, "long_top20_burden_estimates_version_", compare_version, ".csv"))
locations <- get_location_metadata(location_set_id = 35, release_id = release) %>% .[level == 1 | level == 0]
age_weights <- get_age_weights(release_id = release)

full_child_ids <- c()
for(a in ages){
  if(a == 1099){
    child_ids <- c(7:20, 30:32, 235)
    age_name <- "10+ years old (standardized)"
  } else if(a == 1024){
    child_ids <- c(7:9)
    age_name <- "10-24 years old"
  } else if(a == 2549){
    child_ids <- c(10:14)
    age_name <- "25-49 years old"
  } else if(a == 5069){
    child_ids <- c(25) 
    age_name <- "50-69 years old"
  } else if(a == 7099){
    child_ids <- c(26) 
    age_name <- "70+ years old"
  } else {
    stop("You haven't specified this age group!")
  }
  
  full_child_ids <- c(full_child_ids, child_ids) %>% unique()
}

pops <- get_population(release_id = release, age_group_id = full_child_ids, sex_id = c(1, 2),
                       year_id = years, location_id = locations$location_id)

draws_pulled <- data.table()
for(i in measures){
  if(i == 2){
    measure_name <- "DALYs"
    machinery <- "dalynator"
    version <- daly_version
  } else if(i == 3){
    measure_name <- "YLDs"
    machinery <- "como"
    version <- yld_version
  } else if(i == 4){
    measure_name <- "YLLs"
    machinery <- "codcorrect"
    version <- yll_version
    next
  } else {
    stop("You haven't specified this measure!")
  }
  
  subset_pulled <- get_draws(rep("cause_id", 20), c(top_causes$cause_id), 
                            location_id = locations$location_id, 
                            year_id = years, 
                            sex_id = c(1,2), 
                            measure_id = i, 
                            metric_id = 3,
                            age_group_id = full_child_ids,
                            source = machinery, 
                            release_id = release, 
                            version_id = version)
  subset_pulled$measure_name <- measure_name
  
  draws_pulled <- rbindlist(list(subset_pulled, draws_pulled), use.names = T)
}

counts_pulled <- data.table()
for(i in measures){
  if(i == 2){
    measure_name <- "DALYs"
    machinery <- "dalynator"
    version <- daly_version
  } else if(i == 3){
    measure_name <- "YLDs"
    machinery <- "como"
    version <- yld_version
    next
  } else if(i == 4){
    measure_name <- "YLLs"
    machinery <- "codcorrect"
    version <- yll_version
  } else {
    stop("You haven't specified this measure!")
  }
  
  subset_pulled <- get_draws(rep("cause_id", 20), c(top_causes$cause_id), 
                             location_id = locations$location_id, 
                             year_id = years, 
                             sex_id = c(1,2), 
                             measure_id = i, 
                             metric_id = 1,
                             age_group_id = full_child_ids,
                             source = machinery, 
                             release_id = release, 
                             version_id = version)
  subset_pulled$measure_name <- measure_name
  
  counts_pulled <- rbindlist(list(subset_pulled, counts_pulled), use.names = T, fill = T)
}

draw_cols <- paste0("draw_", seq(0, 499, 1))
count_cols <- paste0("count_", seq(0, 499, 1))
yll_cols <- paste0("draw_", seq(0, 999, 1))
yll_count_cols <- paste0("count_", seq(0, 999, 1))

counts_pulled <- merge(counts_pulled, pops[, .(age_group_id, location_id, year_id, sex_id, population)], by = c("age_group_id", "location_id", "year_id", "sex_id"), all.x = T)
draws_pulled <- merge(draws_pulled, pops[, .(age_group_id, location_id, year_id, sex_id, population)], by = c("age_group_id", "location_id", "year_id", "sex_id"), all.x = T)

# We want age-standardized for 10+ age group
if(1099 %in% ages){
  as_draws <- copy(draws_pulled) %>% .[age_group_id %in% c(7:20, 30:32, 235) & measure_id != 4]
  as_draws <- merge(as_draws, age_weights, by = "age_group_id", all.x = T)
  
  as_draws[, (draw_cols) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  
  if(4 %in% measures){
    as_yll_draws <- copy(counts_pulled) %>% .[age_group_id %in% c(7:20, 30:32, 235) & measure_id == 4]
    as_yll_draws <- merge(as_yll_draws, age_weights, by = "age_group_id", all.x = T)
    
    as_yll_draws[, (yll_cols) := lapply(.SD, function(x) x/population), .SDcols = yll_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
    as_yll_draws[, (yll_cols) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = yll_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  }
  
  if(3 %in% measures){
    count_yld_draws <- copy(draws_pulled) %>% .[age_group_id %in% c(7:20, 30:32, 235) & measure_id == 3]

    count_yld_draws[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
    count_yld_draws[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
    count_yld_draws <- count_yld_draws %>% select(-all_of(draw_cols))
  }
  
  as_draws <- rbindlist(list(as_draws, as_yll_draws), fill = T)
  
  count_1099_draws <- copy(counts_pulled) %>% .[age_group_id %in% c(7:20, 30:32, 235) & measure_id != 3]
  count_1099_draws[, (yll_count_cols) := lapply(.SD, function(x) sum(x, na.rm = T)), .SDcols = yll_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  count_1099_draws <- count_1099_draws %>% select(-all_of(yll_cols))
  count_1099_draws <- rbindlist(list(count_1099_draws, count_yld_draws), use.names = T, fill = T)
  count_1099_draws[, `:=` (version_id = NULL, metric_id = NULL)]
  
  as_draws <- merge(count_1099_draws, as_draws, all = T)
  
  as_draws <- as_draws[age_group_id == 20]
  as_draws[, `:=` (age_group_id = 1099, age_group_weight_value = NULL, age_group_name = "10+ years old (standardized)", population = NULL, version_id = NULL, metric_id = NULL)]
}


# We want all ages for other age groups
if(1024 %in% ages){
  child_ids <- c(7:9)
  age_name <- "10-24 years old"
  
  draws_1024 <- copy(counts_pulled) %>% .[age_group_id %in% child_ids & measure_id != 3]
  setDT(draws_1024)
  draws_1024[, total_pop := sum(population), by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  draws_1024[, (yll_count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = yll_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  
  draws_1024[, (yll_cols) := lapply(.SD, function(x) x/total_pop), .SDcols = yll_count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  
  if(3 %in% measures){
    count_yld_draws <- copy(draws_pulled) %>% .[age_group_id %in% child_ids & measure_id == 3]
    count_yld_draws[, total_pop := sum(population), by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
    
    count_yld_draws[, (draw_cols) := lapply(.SD, function(x) x*population), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
    count_yld_draws[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
    count_yld_draws[, (draw_cols) := lapply(.SD, function(x) x/total_pop), .SDcols = count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  }

  draws_1024 <- rbindlist(list(draws_1024, count_yld_draws), use.names = T, fill = T)
  
  draws_1024 <- draws_1024[age_group_id == max(child_ids)]
  draws_1024[, `:=` (age_group_id = 1024, age_group_name = age_name, total_pop = NULL, population = NULL, version_id = NULL, metric_id = NULL)]
}

# We want all ages for other age groups
if(2549 %in% ages){
  child_ids <- c(10:14)
  age_name <- "25-49 years old"
  
  draws_2549 <- copy(counts_pulled) %>% .[age_group_id %in% child_ids & measure_id != 3]
  setDT(draws_2549)
  
  draws_2549[, total_pop := sum(population), by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  draws_2549[, (yll_count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = yll_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  
  draws_2549[, (yll_cols) := lapply(.SD, function(x) x/total_pop), .SDcols = yll_count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  
  if(3 %in% measures){
    count_yld_draws <- copy(draws_pulled) %>% .[age_group_id %in% child_ids & measure_id == 3]
    count_yld_draws[, total_pop := sum(population), by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
    
    count_yld_draws[, (draw_cols) := lapply(.SD, function(x) x*population), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
    count_yld_draws[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
    count_yld_draws[, (draw_cols) := lapply(.SD, function(x) x/total_pop), .SDcols = count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id")]
  }
  
  draws_2549 <- rbindlist(list(draws_2549, count_yld_draws), use.names = T, fill = T)
  
  draws_2549 <- draws_2549[age_group_id == max(child_ids)]
  draws_2549[, `:=` (age_group_id = 2549, age_group_name = age_name, total_pop = NULL, population = NULL, version_id = NULL, metric_id = NULL)]
}

# Subset the older age groups
if(5069 %in% ages){
  child_ids <- c(25)
  age_name <- "50-69 years old"
  draws_5069 <- copy(draws_pulled) %>% .[age_group_id %in% child_ids]
  draws_counts_5069 <- copy(counts_pulled) %>% .[age_group_id %in% child_ids]
  setnames(draws_counts_5069, yll_cols, yll_count_cols)
  draws_counts_5069[, `:=` (version_id = NULL, metric_id = NULL)]
  draws_5069 <- merge(draws_5069, draws_counts_5069, all = T, by = c("age_group_id", "location_id", "year_id", "sex_id", "cause_id", "measure_id", "measure_name", "population"))
  
  if(3 %in% measures){
    draws_5069[measure_id == 3, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
  }
  if(4 %in% measures){
    draws_5069[measure_id == 4, (yll_cols) := lapply(.SD, function(x) x/population), .SDcols = yll_count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
  }
  draws_5069[, `:=` (age_group_name = age_name, population = NULL, version_id = NULL, metric_id = NULL)]
}

if(7099 %in% ages){
  child_ids <- c(26)
  age_name <- "70+ years old"
  draws_7099 <- copy(draws_pulled) %>% .[age_group_id %in% child_ids]
  draws_counts_7099 <- copy(counts_pulled) %>% .[age_group_id %in% child_ids]
  setnames(draws_counts_7099, yll_cols, yll_count_cols)
  draws_counts_7099[, `:=` (version_id = NULL, metric_id = NULL)]
  draws_7099 <- merge(draws_7099, draws_counts_7099, all = T, by = c("age_group_id", "location_id", "year_id", "sex_id", "cause_id", "measure_id", "measure_name", "population"))
  
  if(3 %in% measures){
    draws_7099[measure_id == 3, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = draw_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
  }
  if(4 %in% measures){
    draws_7099[measure_id == 4, (yll_cols) := lapply(.SD, function(x) x/population), .SDcols = yll_count_cols, by = c("location_id", "sex_id", "measure_id", "metric_id", "year_id", "version_id", "cause_id", "age_group_id")]
  }
  draws_7099[, `:=` (age_group_name = age_name, population = NULL, version_id = NULL, metric_id = NULL)]
}

total_dataset <- rbindlist(list(as_draws, draws_1024, draws_2549, draws_5069, draws_7099), fill = T)
temp <- melt(total_dataset, id.vars = c("sex_id", "age_group_id", "year_id", "location_id", "measure_name", "measure_id", "age_group_name", "cause_id"), measure.vars = c(yll_count_cols, yll_cols))
temp <- temp[!is.na(value)]
temp <- temp[!(variable %in% paste0("count_", seq(500, 999, 1)) & measure_name == "DALYs")]
copy_version <- copy(total_dataset)
total_dataset <- dcast.data.table(temp, sex_id + age_group_id + year_id + location_id + measure_name + measure_id + age_group_name + cause_id ~ variable, value.var = "value")
total_rates <- total_dataset %>% select(-all_of(yll_count_cols))
total_counts <- total_dataset %>% select(-all_of(yll_cols))

if(save_data){
  fwrite(total_rates, paste0(data_fp, "all_draws_rates_version_", compare_version, ".csv"))
  fwrite(total_rates[cause_id %in% top_causes$cause_id], paste0(data_fp, "top20_draws_rates_version_", compare_version, ".csv"))
  
  fwrite(total_counts, paste0(data_fp, "all_draws_counts_version_", compare_version, ".csv"))
  fwrite(total_counts[cause_id %in% top_causes$cause_id], paste0(data_fp, "top20_draws_counts_version_", compare_version, ".csv"))
}

########################################################################################
# Now to produce extra measures
gap_dt <- total_dataset[sex_id != 3 & cause_id %in% top_causes$cause_id]
gap_dt[, sex := ifelse(sex_id == 1, "Male", "Female")]
gap_dt$sex_id <- NULL
gap_dt <- dcast.data.table(gap_dt, cause_id + age_group_name + year_id + location_id + measure_id + measure_name ~ sex, value.var = c(yll_count_cols, yll_cols))

male_counts <- paste0("count_", seq(0, 999, 1), "_Male")
male_draws <- paste0("draw_", seq(0, 999, 1), "_Male")

female_counts <- paste0("count_", seq(0, 999, 1), "_Female")
female_draws <- paste0("draw_", seq(0, 999, 1), "_Female")

abs_gap_counts <- paste0("count_gap_", seq(0, 999, 1))
abs_gap_draws <- paste0("draws_gap_", seq(0, 999, 1))
rel_gap_counts <- paste0("count_relgap_", seq(0, 999, 1))
rel_gap_draws <- paste0("draws_relgap_", seq(0, 999, 1))

# absolute gap
temp <- (gap_dt[, ..female_counts]-gap_dt[, ..male_counts])
gap_dt[, (abs_gap_counts) := temp]

temp <- (gap_dt[, ..female_draws]-gap_dt[, ..male_draws])
gap_dt[, (abs_gap_draws) := temp]

# Relative gap
temp <- (gap_dt[, ..abs_gap_counts]/gap_dt[, ..male_counts])*100
gap_dt[, (rel_gap_counts) := temp]

temp <- (gap_dt[, ..abs_gap_draws]/gap_dt[, ..male_draws])*100
gap_dt[, (rel_gap_draws) := temp]

######################################################################'
######################################################################
# Dropping one relative gap draw for 70+ year olds, cause_id 639, year_id 1990, location_id 31, measure_name YLDs
gap_dt[age_group_name == "70+ years old" & cause_id == 639 & year_id == 1990 & location_id == 31 & measure_name == "YLDs", `:=` (count_relgap_463 = NA, draws_relgap_463 = NA)]

# lets get summary values
gap_dt[, abs_count := rowMeans(.SD, na.rm = T), .SD = abs_gap_counts]
gap_dt[, lower_abs_count := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = abs_gap_counts]
gap_dt[, upper_abs_count := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = abs_gap_counts]

gap_dt[, rel_count := rowMeans(.SD, na.rm = T), .SD = rel_gap_counts]
gap_dt[, lower_rel_count := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = rel_gap_counts]
gap_dt[, upper_rel_count := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = rel_gap_counts]

gap_dt[, abs_rate := rowMeans(.SD, na.rm = T), .SD = abs_gap_draws]
gap_dt[, lower_abs_rate := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = abs_gap_draws]
gap_dt[, upper_abs_rate := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = abs_gap_draws]

gap_dt[, rel_rate := rowMeans(.SD, na.rm = T), .SD = rel_gap_draws]
gap_dt[, lower_rel_rate := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = rel_gap_draws]
gap_dt[, upper_rel_rate := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = rel_gap_draws]

gap_dt[, abs_issue := ifelse(abs_rate > upper_abs_rate | abs_rate < lower_abs_rate, 1, 0)]
gap_dt[, rel_issue := ifelse(rel_rate > upper_rel_rate | rel_rate < lower_rel_rate, 1, 0)]


temp <- melt(gap_dt[rel_issue == 1], id.vars = c("cause_id", "age_group_name", "year_id", "location_id", "measure_id", "measure_name"))

ggplot(temp[variable %in% male_draws]) + 
  geom_histogram(aes(x = value)) + 
  geom_vline(xintercept = quantile(temp[variable %in% male_draws, value], c(.025, .975), na.rm = T), color = "red") + 
  geom_vline(xintercept = mean(temp[variable %in% male_draws, value], na.rm = T), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = quantile(temp[variable %in% male_draws, value], c(0.5), na.rm = T, color = "green"))
  
ggplot(temp[variable %in% abs_gap_draws]) + 
  geom_histogram(aes(x = value)) + 
  geom_vline(xintercept = quantile(temp[variable %in% abs_gap_draws, value], c(.025, .975), na.rm = T), color = "red") + 
  geom_vline(xintercept = mean(temp[variable %in% abs_gap_draws, value], na.rm = T), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = quantile(temp[variable %in% abs_gap_draws, value], c(0.5), na.rm = T, color = "green"))

ggplot(temp[variable %in% rel_gap_draws]) + 
  geom_histogram(aes(x = value)) + 
  geom_vline(xintercept = quantile(temp[variable %in% rel_gap_draws, value], c(.025, .975), na.rm = T), color = "red") + 
  geom_vline(xintercept = mean(temp[variable %in% rel_gap_draws, value], na.rm = T), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = quantile(temp[variable %in% rel_gap_draws, value], c(0.5), na.rm = T, color = "green"))

ggplot(temp[variable %in% rel_gap_draws & value < 3e6]) + 
  geom_histogram(aes(x = value)) + 
  geom_vline(xintercept = quantile(temp[variable %in% rel_gap_draws, value], c(.025, .975), na.rm = T), color = "red") + 
  geom_vline(xintercept = mean(temp[variable %in% rel_gap_draws, value], na.rm = T), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = quantile(temp[variable %in% rel_gap_draws, value], c(0.5), na.rm = T, color = "green"))

gap_dt <- unique(gap_dt[, .(location_id, cause_id, age_group_name, year_id, measure_id, measure_name, 
                            abs_count, lower_abs_count, upper_abs_count, 
                            rel_count, lower_rel_count, upper_rel_count, 
                            abs_rate, lower_abs_rate, upper_abs_rate, 
                            rel_rate, lower_rel_rate, upper_rel_rate)])

if(save_data){

  fwrite(gap_dt, paste0(data_fp, "top20_absolute_relative_gaps_version_", compare_version, ".csv"))
}

