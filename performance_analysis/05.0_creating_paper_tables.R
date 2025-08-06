## HEADER #################################################################
# Date: 4/24/2025
# Purpose: Creating tables for performance paper
#          
# source("filepath/05.0_creating_paper_tables.R", echo=T)
#
## SET-UP #################################################################
rm(list = ls())
library(data.table)
library(tidyverse)
library(ggplot2)
date <- format(Sys.Date(), "%d-%m-%y")

source("filepath/get_location_metadata.R")
source("filepath/get_outputs.R")

## SETTING PARAMETERS
version <- "2spline"
cov_name <- "sdi"
measure_name <- "dalys"
cause_names <- c("ovarian_cancer", "cervical_cancer", "uterine_cancer", "breast_cancer", "maternal_disorders", 
                 "all_causes", "cardiovascular_diseases")
release <- 16
multiplier <- 100000

## FILEPATHS
if(cov_name != "sdi"){
  filename <- paste0(version, "_", cov_name, "_", measure_name, "_inefficiencies.csv")
} else {
  filename <- paste0(version, "_", measure_name, "_inefficiencies.csv")
}

output_dir <- "filepath"
full_input_dir <- "filepath/compiled_full_values_sdi_top_and_sexspecific.csv"
pdf_fp <- paste0(output_dir, "paper_figures/", gsub(".csv", "", filename), "/")
table_fp <- paste0(output_dir, "paper_tables/", gsub(".csv", "", filename), "/")

## AUTOMATIC HELPERS ####################################################
# Check filepaths
if(!dir.exists(pdf_fp)){
  dir.create(pdf_fp, recursive = T)
}
if(!dir.exists(table_fp)){
  dir.create(table_fp, recursive = T)
}

## SCRIPT #################################################################
# Load data
results <- lapply(cause_names, function(x) fread(paste0(output_dir, x, "_", filename))) %>% rbindlist()
full_data <- fread(full_input_dir)

## SDI table for appendix
sdi_table <- results[year_id %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), .(location_name, year_id, mean_predictor)]
sdi_table[, mean_predictor := sprintf('%.3f', mean_predictor)]
sdi_table[, year_id := paste0("SDI in ", year_id)]
sdi_table <- unique(sdi_table)
sdi_table <- dcast(sdi_table, location_name ~ year_id, value.var = "mean_predictor")
setorder(sdi_table, `SDI in 2023`)
fwrite(sdi_table, paste0(table_fp, "sdi_table.csv"))

## DALY table for appendix
daly_table <- results[year_id %in% c(1990, 2000, 2010, 2023), .(location_name, year_id, cause_name, val, upper, lower)]

daly_table[, `:=` (val = sprintf('%.1f', val*multiplier), upper = sprintf('%.1f', upper*multiplier), lower = sprintf('%.1f', lower*multiplier))]
daly_table[, val_clean := paste0(val, "\n(", lower, "–", upper, ")")]
daly_table <- unique(daly_table)
daly_table <- dcast(daly_table, location_name ~ cause_name, value.var = c("val_clean"))
fwrite(daly_table, paste0(table_fp, "daly_table.csv"))

locations <- get_location_metadata(location_set_id = 1, release_id = release) %>% .[level <= 3, .(location_id, region_name, super_region_name)]
daly_table <- full_data[year_id %in% c(1990, 2000, 2010, 2023), .(location_name, location_id, year_id, cause_name, val, upper, lower, mean_predictor)]
daly_table <- merge(daly_table, locations, by = "location_id", all.x = TRUE)
daly_table[, `:=` (val = sprintf('%.1f', val*multiplier), upper = sprintf('%.1f', upper*multiplier), lower = sprintf('%.1f', lower*multiplier),  mean_predictor = sprintf('%.3f', mean_predictor))]
daly_table[, val_clean := paste0(val, "\n(", lower, "–", upper, ")")]
daly_table <- unique(daly_table)
sdi_option <- daly_table[, .(year_id, location_id, location_name, region_name, super_region_name, mean_predictor)]
setDT(sdi_option)
sdi_option$cause_name <- "Socio-demographic index"
setnames(sdi_option, "mean_predictor", "val_clean")
daly_table$mean_predictor <- NULL
daly_table <- rbind(daly_table, sdi_option, use.names = TRUE, fill = TRUE)
daly_table[, `:=` (val = NULL, upper = NULL, lower = NULL, location_id = NULL)]
daly_table <- unique(daly_table)
daly_table <- dcast(daly_table, year_id + location_name + region_name + super_region_name ~ cause_name, value.var = "val_clean")
daly_table <- daly_table[, .(year_id, super_region_name, region_name, location_name, `Breast cancer`, `Cervical cancer`, `Ovarian cancer`, `Uterine cancer`, `Maternal disorders`, `Socio-demographic index`)]
setorder(daly_table, year_id, super_region_name, region_name, location_name)
fwrite(daly_table, paste0(table_fp, date, "_daly_table.csv"))

# % change
temp <- get_outputs(topic = "cause", 
                    release_id = 16, 
                    cause_id = c(366, 429, 432, 435, 465), 
                    location_id = "all", 
                    year_start_id = 1990, 
                    year_end_id = 2023, 
                    sex_id = 2, 
                    age_group_id = 27, 
                    compare_version_id = 8305,
                    measure_id = 2, 
                    metric_id = 3)
temp <- merge(temp[, .(location_id, location_name, cause_name, val, upper, lower)], locations, by = "location_id")
temp[, `:=` (val = sprintf('%.1f', val*100), upper = sprintf('%.1f', upper*100), lower = sprintf('%.1f', lower*100))]
temp[, val_clean := paste0(val, "%\n(", lower, "%–", upper, "%)")]
temp <- unique(temp)
temp <- dcast(temp, location_name + region_name + super_region_name ~ cause_name, value.var = "val_clean")
temp <- temp[, .(super_region_name, region_name, location_name, `Breast cancer`, `Cervical cancer`, `Ovarian cancer`, `Uterine cancer`, `Maternal disorders`)]
setorder(temp, super_region_name, region_name, location_name)
fwrite(temp, paste0(table_fp, date, "_percent_change_table.csv"))

# Super-region table
locations <- get_location_metadata(location_set_id = 1, release_id = release) %>% .[level <= 1 & !(location_name %like% "SDI"), .(location_id, location_name)]
sr_data <- copy(full_data) %>% .[(location_id %in% unique(locations$location_id)) & year_id == 2023 & cause_name != "Gynecological diseases"]
sr_data <- sr_data[, .(location_name, location_id, cause_name, val, upper, lower)]
sr_data[, `:=` (val = sprintf('%.1f', val*multiplier), upper = sprintf('%.1f', upper*multiplier), lower = sprintf('%.1f', lower*multiplier))]
sr_data[, val := paste0(val, "\n(", lower, "–", upper, ")")]
sr_data <- dcast(sr_data, location_name ~ cause_name, value.var = "val")

sr_data[, location_name := factor(location_name, levels = c("Global", unique(sr_data$location_name[!(sr_data$location_name %in% c("Global"))])))]
setorder(sr_data, location_name)

fwrite(sr_data, paste0(table_fp, "super_region_table.csv"))

# Table 2
results_small <- copy(results[year_id %in% c(1990, 2023)])
results_small[, ineff_pkg := ineff_pkg - 1]
results_small[, abs_ineff := abs_ineff*multiplier]
results_small[abs_ineff < 0, abs_ineff := 0]

compiled_results <- copy(results_small)
compiled_results[, `:=` (mean_ineff_global = mean(ineff_pkg), sd_ineff_global = sd(ineff_pkg), 
                         mean_abs_global = mean(abs_ineff), sd_abs_global = sd(abs_ineff)), by = c("cause_name", "year_id")]
compiled_results[, `:=` (mean_ineff = mean(ineff_pkg), sd_ineff = sd(ineff_pkg), 
                         mean_abs = mean(abs_ineff), sd_abs = sd(abs_ineff)), by = c("cause_name", "super_region_name", "year_id")]

compiled_results <- unique(compiled_results[, .(cause_name, year_id, super_region_name, mean_ineff, sd_ineff, mean_ineff_global, sd_ineff_global, mean_abs, sd_abs, mean_abs_global, sd_abs_global)])
global <- copy(compiled_results[, .(cause_name, year_id, mean_ineff_global, sd_ineff_global, mean_abs_global, sd_abs_global)])
global[, super_region_name := "Global"]
setnames(global, c("mean_ineff_global", "sd_ineff_global", "mean_abs_global", "sd_abs_global"), c("mean_ineff", "sd_ineff", "mean_abs", "sd_abs"))
global <- unique(global)

compiled_results[, `:=` (mean_ineff_global = NULL, sd_ineff_global = NULL, mean_abs_global = NULL, sd_abs_global = NULL)]
compiled_results <- rbindlist(list(compiled_results, global), use.names = T)

compiled_results[, `:=` (mean_ineff = sprintf('%.1f', mean_ineff*100), sd_ineff = sprintf('%.1f', sd_ineff*100), 
                         mean_abs = sprintf('%.1f', mean_abs), sd_abs = sprintf('%.1f', sd_abs))]
compiled_results[, mean_ineff := paste0(mean_ineff, " (", sd_ineff, ")")]
compiled_results[, mean_abs := paste0(mean_abs, " (", sd_abs, ")")]

compiled_results <- dcast(compiled_results, super_region_name + year_id ~ cause_name, value.var = c("mean_ineff", "mean_abs"))

compiled_results[, super_region_name := factor(super_region_name, levels = c("Global", unique(compiled_results$super_region_name[!(compiled_results$super_region_name %in% c("Global"))])))]
setorder(compiled_results, super_region_name)
fwrite(compiled_results, paste0(table_fp, date, "_super_region_ineffs.csv"))

# FOR SI
si_data <- copy(results) %>% .[year_id %in% c(1990, 2000, 2010, 2023), .(mean_predictor, year_id, location_name, cause_name, val_pred, val, ineff_pkg)]
si_data[, abs_ineff := val*multiplier - val_pred*multiplier]
si_data[abs_ineff < 0, abs_ineff := 0]

fwrite(si_data, paste0(table_fp, date, "_data_without_formatting.csv"))

si_data[, `:=` (val_pred = sprintf('%.1f', val_pred*multiplier), ineff_pkg = paste0(sprintf('%.1f', (ineff_pkg-1)*100), "%"), 
                abs_ineff = sprintf('%.1f', abs_ineff))]
si_data[, `:=` (val = NULL, val_pred = NULL)]
setorder(si_data, year_id, mean_predictor)
si_data <- dcast(si_data, year_id + mean_predictor + location_name ~ cause_name, value.var = c("abs_ineff", "ineff_pkg"))

fwrite(si_data, paste0(table_fp, date, "_big_si_table.csv"))
