## HEADER #################################################################
# Date: 6/26/2024
# Purpose: Pulling covariate estimates
#          
# source("filepath/01.2_pulling_predictor.R", echo=T)

## SET-UP #################################################################
rm(list = ls())
library(data.table)
library(tidyverse)

source("filepath/get_covariate_estimates.R")

## SCRIPT ##################################################################
release <- commandArgs(trailingOnly = T)[1]
covariate_id <- commandArgs(trailingOnly = T)[2]
covariate_name <- commandArgs(trailingOnly = T)[3]
output_fp <- commandArgs(trailingOnly = T)[4]

predictor <- get_covariate_estimates(covariate_id, release_id = release)
setDT(predictor)
predictor <- predictor[, .(year_id, location_id, mean_value, lower_value, upper_value)]
setnames(predictor, c("mean_value", "lower_value", "upper_value"), c(paste0("mean_", covariate_name), paste0("lower_", covariate_name), paste0("upper_", covariate_name)))

fwrite(predictor, paste0(output_fp, "summary_", covariate_name, ".csv"))
