## HEADER #################################################################
# Date: 12/2/2024
# Purpose: Creating visualizations for performance paper
#          
# source("filepath/04.0_creating_paper_visuals.R", echo=T)

## SET-UP #################################################################
rm(list = ls())
library(data.table)
library(tidyverse)
library(ggplot2)
date <- format(Sys.Date(), "%d-%m-%y")

source("filepath/get_location_metadata.R")
source("filepath/get_population.R")
source("filepath/get_cause_metadata.R")
source("filepath/gbd2023_map.R")

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

output_dir <- "filepath/"
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

# Plotting help
pretty_plot <- theme(strip.background = element_rect(fill = "white", color = "black"), 
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 20))
color_palette_region <- c("Sub-Saharan Africa" = "#1f77b4", 
                          "North Africa and Middle East" = "#ff7f0e", 
                          "Southeast Asia, East Asia, and Oceania" = "#2ca02c", 
                          "South Asia" = "#d62728", 
                          "Central Europe, Eastern Europe, and Central Asia" = "#9467bd", 
                          "Latin America and Caribbean" = "#8c564b", 
                          "High-income" = "#e377c2")
color_palette_outcomes <- c("Breast cancer" = "#1f77b4", 
                          "Cervical cancer" = "#ff7f0e", 
                          "Ovarian cancer" = "#2ca02c", 
                          "Uterine cancer" = "#d62728", 
                          "Maternal disorders" = "#9467bd")

## SCRIPT #################################################################
# Load data
results <- lapply(cause_names, function(x) fread(paste0(output_dir, x, "_", filename))) %>% rbindlist()

# Format absolute inefficiency and set base
results[, abs_ineff := abs_ineff*multiplier]
results[abs_ineff < 0, abs_ineff := 0]
results[, ineff_pkg := ineff_pkg - 1]

# Set SDI quintiles
results[, sdi_quintile := ifelse(mean_predictor < 0.5316, "Low SDI", 
                                       ifelse(mean_predictor < 0.6305, "Low-middle SDI", 
                                              ifelse(mean_predictor < .6767, "Middle SDI", 
                                                     ifelse(mean_predictor < 0.7188, "High-middle SDI", "High SDI"))))]
results[, sdi_quintile := factor(sdi_quintile, levels = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"))]

all_results <- copy(results)
results <- results[year_id == 2023]

### FIGURE 1 MAPS ##############################################################
maps <- function(data, cause, multiplier = 100000, legend_lab = "per 100K", measure = measure_name){
  plotting_data <- copy(data) %>% .[cause_name == cause]
  plotting_data[, mapvar := val*multiplier]
  
  if(cause == "Ovarian cancer"){
    limits_auto <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 300, 500)
  } else if(cause == "Uterine cancer"){
    limits_auto <- c(0, 20, 40, 60, 80, 100, 100, 120, 140, 160, 180, 200, 500)
  } else if(cause == "Cervical cancer") {
    limits_auto <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 3000)
  } else if(cause == "Breast cancer") {
    limits_auto <- c(0, 150, 300, 450, 600, 750, 900, 1050, 1200, 1350, 1500, 1650, 2000)
  } else if(cause == "Maternal disorders"){
    limits_auto <- c(0, 50, 100, 200, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 6000)
  } else if(cause == "SDI"){ 
    limits_auto <- seq(0, 1, 0.1)
  } else {
    minimum <- min(floor(plotting_data$mapvar/50)*50)
    maximum <- max(ceiling(plotting_data$mapvar/50)*50)
    limits_auto <- round_any(quantile(plotting_data$mapvar, probs = seq(0, 1, length.out = 8)), 10, f = floor)
    limits_auto[length(limits_auto)] <- maximum + 10
  }
  
  if(cause == "SDI"){
    labels_auto <- paste0(limits_auto[-length(limits_auto)], "-", limits_auto[-1]-0.01)
    labels_auto[length(labels_auto)] <- paste0(limits_auto[length(limits_auto) - 1], "+")
    reverse <- FALSE
  } else {
    labels_auto <- paste0(limits_auto[-length(limits_auto)], "-", limits_auto[-1]-1)
    labels_auto[length(labels_auto)] <- paste0(limits_auto[length(limits_auto) - 1], "+")
    reverse <- TRUE
  }
  
  measure_label <- ifelse(measure != "sdi", gsub("S", "s", toupper(measure)), toupper(measure))

  gbd_map(plotting_data,
          limits = limits_auto,
          labels = labels_auto,
          sub_nat = "none",
          inset = TRUE,
          col.reverse = reverse,
          legend.cex = 1.5,
          title = paste0(ifelse(measure_label != "SDI", paste0("", cause, " ", measure_label, " rates"), measure_label), ""),
          legend.title = paste0(measure_label, " ", legend_lab))
}

alt <- copy(unique(results[, .(location_id, location_name, ihme_loc_id, mean_predictor)]))
setnames(alt, "mean_predictor", "val")
alt$cause_name <- "SDI"

pdf(paste0(pdf_fp, date, "_observed_maps.pdf"), width = 10, height = 6)
for(i in cause_names){
  i <- gsub("_", " ", tools::toTitleCase(i))
  print(i)
  maps(results, cause = i)
}
maps(alt, cause = "SDI", multiplier = 1, legend_lab = "", measure = "sdi")
dev.off()

### FIGURE 2 Scatter of frontier ###############################################
results[outliers == FALSE, obs_rank := rank(-val), by = c("cause_name", "super_region_name")]
results[outliers == FALSE, obs_rank_rev := rank(val), by = c("cause_name", "super_region_name")]
results[, location_label := ifelse(location_name %like% "Iran", "Iran", 
                                       ifelse(location_name %like% "Virgin Islands", "US Virgin Islands", 
                                              ifelse(location_name == "Democratic Republic of the Congo", "DRC", 
                                                     ifelse(location_name == "Republic of Korea", "South Korea", 
                                                            ifelse(location_name %like% "Syrian", "Syria", 
                                                                   ifelse(location_name %like% "Bolivia", "Bolivia", 
                                                                          ifelse(location_name %like% "Venezuela", "Venezuela", 
                                                                                 ifelse(location_name %like% "Lao People's", "Laos", 
                                                                                        ifelse(location_name %like% "United States of", "USA", 
                                                                          ifelse(location_name == "Brunei Darussalam", "Brunei", location_name))))))))))]

# Flagging countries for labeling, including top/bottom performers and high-population countries, and cleaning up labels for readability
results[, location_label := ifelse(obs_rank %in% c(1,2) | obs_rank_rev %in% c(1) | 
                                     (location_name %in% c("United States of America", "India", "China", "Indonesia", "Pakistan", "Nigeria", "Brazil") & outliers == FALSE), 
                                   location_label, NA)]
results[cause_name == "Breast cancer" & location_name %in% c("Barbados", "Tunisia", "Singapore", "Serbia", "Maldives", "Albania", "Mozambique"), location_label := NA]
results[cause_name == "Cervical cancer" & location_name %in% c("Nigeria", "Bhutan", "Morocco", "Nepal", "Mongolia", "Romania", "Bermuda", "Oman", "Algeria", "Guam", "San Marino", "Greenland"), location_label := NA]
results[cause_name == "Maternal disorders" & location_name %in% c("Cabo Verde", "Mongolia", "Uzbekistan", "Poland", "Brunei Darussalam", "Costa Rica", "Solomon Islands", "Brazil"), location_label := NA]
results[cause_name == "Ovarian cancer" & location_name %in% c("Brunei Darussalam", "Bhutan", "Vanuatu", "Albania", "San Marino", "Greenland"), location_label := NA]
results[cause_name == "Uterine cancer" & location_name %in% c("Northern Mariana Islands", "Maldives", "India", "Turkmenistan", "Nicaragua", "Yemen", "Eswatini", "Greenland"), location_label := NA]

results[, cause_label_options := factor(cause_name, levels = c("All causes", "Cardiovascular diseases", "Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders"))]

pdf(paste0(pdf_fp, date, "_scatter_frontier.pdf"), width = 12, height = 10)
plot1 <- ggplot(results[cause_label_options %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders")]) +
  geom_point(aes(y = val*multiplier, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
  geom_line(aes(y = val_pred*multiplier, x = mean_predictor), alpha = 0.7) +
  geom_point(data = results[outliers == TRUE & cause_label_options %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders")], 
  aes(y = val*multiplier, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
  geom_text_repel(aes(y = val*100000, x = mean_predictor, label=location_label), size = 3, hjust = "left", max.overlaps = 100) +
  scale_color_manual(values = color_palette_region) +
  facet_wrap(~cause_label_options, scales = "free", ncol = 2) + 
  labs(title = paste0(""),
       y = "DALYs per 100K in 2023",
       x = "Socio-Demographic Index in 2023", color = "GBD super-region", 
       size = "Inverse standard error of\nDALY rates") +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
         color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = c(1, 0.05),
        legend.justification = c(1, 0), legend.box = "horizontal", 
        strip.text = element_text(face = "bold", size = 12), 
        text = element_text(size = 12))
print(plot1)

for(i in unique(results$cause_label_options)){
  plot2 <- ggplot(results[cause_label_options == i]) +
    geom_point(aes(y = val*multiplier, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
    geom_line(aes(y = val_pred*multiplier, x = mean_predictor), alpha = 0.7) +
    geom_point(data = results[outliers == TRUE & cause_label_options == i], aes(y = val*multiplier, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
    geom_text_repel(aes(y = val*100000, x = mean_predictor, label=location_label), size = 3, hjust = "left", max.overlaps = 100) +
    scale_color_manual(values = color_palette_region) +
    facet_wrap(~cause_label_options, scales = "free", ncol = 2) + 
    labs(title = paste0(""),
         y = "DALYs per 100K in 2023",
         x = "Socio-Demographic Index in 2023", color = "GBD super-region", 
         size = "Inverse standard error of\nDALY rates") +
    theme_minimal() +
    guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
           color = guide_legend(override.aes = list(size = 3))) +
    theme(legend.box = "vertical", 
          strip.text = element_text(face = "bold", size = 12), 
          text = element_text(size = 12))
  print(plot2)
}
dev.off()

### FIGURE 3 MAPS ##############################################################
ineff_results <- copy(results)
ineff_results[, mapvar := abs_ineff]

pdf(paste0(pdf_fp, date, "_map_ineff.pdf"), height = 6, width = 10)
for(i in c(unique(ineff_results$cause_name))){
  if(i %in% c("Ovarian cancer")){
    lims <- c(0, 10, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 500)
  } else if(i == "Breast cancer"){
    lims <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 3000)
  } else if(i == "Cervical cancer"){
    lims <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 3000)
  } else if(i == "Maternal disorders"){
    lims <- c(0, 25, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000)
  } else if(i == "Uterine cancer"){
    lims <- c(0, 10, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 500)
  } else {
    lims <- c(0, 25, 50, 75, 100, 150, 200, 250, 300, 350, 400, 450, 500, 750, 1000, 2000, 5000)
  }
  
  labels_auto <- paste0(lims[-length(lims)], "-", lims[-1]-1)
  labels_auto[length(labels_auto)] <- paste0(lims[length(lims) - 1], "+")

  gbd_map(ineff_results[cause_name == i], 
          limits = lims,
          labels = labels_auto,
          inset = TRUE,
          col.reverse = TRUE, 
          legend.cex = 1.2,
          legend.columns = 2,
          title.cex = 1.5,
          sub_nat = "none", 
          title = paste0("Absolute inefficiency for ", tolower(i)),
          legend.title = paste0("Distance from frontier\nin DALYs per 100K"))
}
dev.off()

## FIGURE 4 SCATTER OF SDI VERSUS INEFF SCORE ############################################
ineff_results[, `:=` (mean_ineff = mean(ineff_pkg), sd_ineff = sd(ineff_pkg)), by = c("cause_name")]
ineff_results[, `:=` (mean_ineff = mean(abs_ineff), sd_ineff = sd(abs_ineff)), by = c("cause_name")]

ineff_results[, location_label := ifelse(location_name %like% "Iran", "Iran", 
                                   ifelse(location_name %like% "Virgin Islands", "US Virgin Islands", 
                                          ifelse(location_name == "Democratic Republic of the Congo", "DRC", 
                                                 ifelse(location_name == "Republic of Korea", "South Korea", 
                                                        ifelse(location_name %like% "Syrian", "Syria", 
                                                               ifelse(location_name %like% "Bolivia", "Bolivia", 
                                                                      ifelse(location_name %like% "Venezuela", "Venezuela", 
                                                                             ifelse(location_name %like% "Lao People's", "Laos", 
                                                                                    ifelse(location_name %like% "United States of", "USA", 
                                                                                           ifelse(location_name == "Brunei Darussalam", "Brunei", location_name))))))))))]
ineff_results[abs_ineff < mean_ineff+(2*sd_ineff), location_label := NA]

pdf(paste0(pdf_fp, date, "_scatter_ineffsdi.pdf"), width = 12, height = 10)
ggplot(ineff_results[cause_label_options %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders")]) +
  geom_point(aes(y = abs_ineff, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
  geom_point(data = ineff_results[outliers == TRUE & cause_label_options %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders")], 
  aes(y = abs_ineff, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
  geom_text_repel(aes(y = abs_ineff, x = mean_predictor, label=location_label), size = 3, hjust = "left", max.overlaps = 100) +
  scale_color_manual(values = color_palette_region) +
  facet_wrap(~cause_label_options, scales = "free", ncol = 2) + 
  labs(title = paste0(""),
       y = "Distance from the frontier in DALYs per 100K",
       x = "Socio-Demographic Index in 2023", color = "GBD super-region", 
       size = "Inverse standard error of\nDALY rates") +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
         color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
  theme(legend.position = "bottom",
        legend.justification = c(1, 0), legend.box = "horizontal", 
        strip.text = element_text(face = "bold", size = 12), 
        text = element_text(size = 12))
dev.off()

## FIGURE 5 COMPARISON TO ALL CAUSE #################################################################
results[, location_name := ifelse(str_count(location_name, ' ') >= 2, ihme_loc_id, location_name)]
to_plot <- copy(results) 
to_plot[, female_health := ifelse(cause_name_label %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders"), "Female-dominant health outcome", "Other")]
other_toplot <- copy(to_plot[female_health == "Other"])
to_plot <- to_plot[female_health != "Other"]
other_toplot <- dcast.data.table(other_toplot, super_region_name + location_name ~ cause_name_label, value.var = c("rank_ineff_total", "abs_ineff"))
to_plot <- merge(to_plot, other_toplot, by = c("super_region_name", "location_name"), all = T)

### Scatter against all causes
pdf(paste0(pdf_fp, date, "_comparisons_allcause.pdf"), width = 16, height = 12)
to_plot[, rank_diff_allcause := `rank_ineff_total_All causes` - rank_ineff_total]
to_plot[abs(rank_diff_allcause) > 100, flag := "Difference in rank > 100 points"]

to_plot[is.na(flag), flag := "Difference in rank =< 100 points"]
p1 <- ggplot(to_plot) + 
  geom_point(aes(x = `rank_ineff_total_All causes`, y = rank_ineff_total, color = super_region_name, alpha = flag, shape = flag), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(data = to_plot[flag == "Difference in rank > 100 points"], aes(x = `rank_ineff_total_All causes`, y = rank_ineff_total, label = location_name), size = 3.5, max.overlaps = 100) +
  facet_wrap(~cause_name_label) +
  scale_alpha_manual(values = c("Difference in rank > 100 points" = 1, 
                               "Difference in rank =< 100 points" = 0.9)) + 
  scale_shape_manual(values = c("Difference in rank > 100 points" = 19, 
                                "Difference in rank =< 100 points" = 1)) + 
  labs(x = "Rank of inefficiency in female all-cause DALYs", y = "Rank of inefficiency in female-specific condition", title = "Inefficiencies in female-specific conditions vs all causes", 
       color = "", shape = "", alpha = "", subtitle = c("Ranked from 1 (lowest/no inefficiency) to 204 (highest inefficiency of all countries) by cause")) +
  theme_bw() + 
  scale_color_manual(values = color_palette_region) + 
  theme(legend.justification = c(1, 0), legend.box = "vertical", 
        strip.text = element_text(face = "bold", size = 12), 
        strip.background = element_rect(fill = "white"), 
        legend.position = c(0.9, 0.05),
        text = element_text(size = 12))
  print(p1)

to_plot[, rank_diff_cvd := `rank_ineff_total_Cardiovascular diseases` - rank_ineff_total]
to_plot[abs(rank_diff_cvd) > 100, flag := "Difference in rank > 100 points"]

to_plot[is.na(flag), flag := "Difference in rank =< 100 points"]
p2 <- ggplot(to_plot) + 
  geom_point(aes(x = `rank_ineff_total_Cardiovascular diseases`, y = rank_ineff_total, color = super_region_name, alpha = flag, shape = flag), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(data = to_plot[flag == "Difference in rank > 100 points"], aes(x = `rank_ineff_total_Cardiovascular diseases`, y = rank_ineff_total, label = location_name), size = 3.5, max.overlaps = 100) +
  facet_wrap(~cause_name_label) +
  scale_alpha_manual(values = c("Difference in rank > 100 points" = 1, 
                                "Difference in rank =< 100 points" = 0.9)) + 
  scale_shape_manual(values = c("Difference in rank > 100 points" = 19, 
                                "Difference in rank =< 100 points" = 1)) + 
  labs(x = "Rank of inefficiency in female cardiovascular disease DALYs", y = "Rank of inefficiency in female-specific condition", title = "Inefficiencies in female-specific conditions vs cardiovascular diseases", 
       color = "", shape = "", alpha = "", subtitle = c("Ranked from 1 (lowest/no inefficiency) to 204 (highest inefficiency of all countries) by cause")) +
  theme_bw() + 
  scale_color_manual(values = color_palette_region) + 
  # guides(alpha = "none", shape = "none")+
  theme(legend.justification = c(1, 0), legend.box = "vertical", 
        strip.text = element_text(face = "bold", size = 12), 
        strip.background = element_rect(fill = "white"), 
        legend.position = c(0.9, 0.05),
        text = element_text(size = 12))
print(p2)

p3 <- ggplot(to_plot) + 
  geom_point(aes(x = `abs_ineff_All causes`, y = abs_ineff, color = super_region_name)) +
  geom_smooth(aes(x = `abs_ineff_All causes`, y = abs_ineff), linetype = "solid", color = "black", method = "lm") +
  facet_wrap(~cause_name_label, scales = "free_y") +
  labs(x = "Inefficiency in female all-cause DALYs per 100K", y = "Inefficiency in female-specific condition DALYs per 100K", title = "Inefficiencies in female-specific conditions vs all causes", 
       color = "", subtitle = "Solid line corresponds to lm(y~x)") +
  theme_bw() +
  theme(legend.justification = c(1, 0), legend.box = "vertical", 
        strip.text = element_text(face = "bold", size = 12), 
        strip.background = element_rect(fill = "white"), 
        legend.position = c(0.9, 0.05),
        text = element_text(size = 12))
print(p3)

p5 <- ggplot(to_plot) + 
  geom_point(aes(x = `abs_ineff_Cardiovascular diseases`, y = abs_ineff, color = super_region_name)) +
  geom_smooth(aes(x = `abs_ineff_Cardiovascular diseases`, y = abs_ineff), linetype = "solid", color = "black", method = "lm") +
  facet_wrap(~cause_name_label, scales = "free_y") +
  labs(x = "Inefficiency in female CVD DALYs per 100K", y = "Inefficiency in female-specific condition DALYs per 100K", title = "Inefficiencies in female-specific conditions vs cardiovascular diseases", 
       color = "", subtitle = "Solid line corresponds to lm(y~x)") +
  theme_bw() +
  theme(legend.justification = c(1, 0), legend.box = "vertical", 
        strip.text = element_text(face = "bold", size = 12), 
        strip.background = element_rect(fill = "white"), 
        legend.position = c(0.9, 0.05),
        text = element_text(size = 12))
print(p5)
dev.off()

## APPENDIX/EXPLORATORY FIGURES ###########################################################################
# SDI Boxplots
pdf(paste0(pdf_fp, date, "_boxplot_ineffsdi.pdf"), width = 12, height = 10)
ggplot(ineff_results[cause_label_options %in% c("All causes", "Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders")]) +
  geom_boxplot(aes(y = abs_ineff, x = sdi_quintile, color = sdi_quintile)) +
  geom_jitter(aes(y = abs_ineff, x = sdi_quintile, color = sdi_quintile), alpha = 0.25) +
  facet_wrap(~cause_label_options, scales = "free_y", ncol = 2) + 
  labs(title = paste0(""),
       y = "Distance from the frontier in DALYs per 100K",
       x = "", color = "GBD SDI Quintiles", 
       size = "Inverse standard error of\nDALY rates") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 1)) +
  theme(legend.justification = c(1, 0), legend.box = "horizontal", 
        strip.text = element_text(face = "bold", size = 12), 
        legend.position = c(0.9, 0.05),
        text = element_text(size = 12))
dev.off()

# Heatmap of ranking
results[, super_region_label := ifelse(super_region_name %like% "Southeast Asia", "Southeast Asia, East Asia,\nand Oceania", 
                                       ifelse(super_region_name %like% "Central Europe, Eastern Europe, and Central Asia", "Central Europe, Eastern Europe,\nand Central Asia", super_region_name))]
results[, rank_ineff := rank(abs_ineff, ties.method = "min"), by = c("cause_name", "super_region_name")]
results[, rank_ineff_total := rank(abs_ineff, ties.method = "min"), by = c("cause_name")]
results[, cause_label := gsub(" ", "\n", cause_name)]

heatmap_results <- copy(results[cause_name != "All causes" & cause_name != "Cardiovascular diseases"])
heatmap_results[, ineff_label := round(abs_ineff, digits = 1)]
heatmap_results[, cause_label := factor(cause_label, levels = c("Maternal\ndisorders", "Gynecological\ndiseases", "Breast\ncancer", "Cervical\ncancer", "Ovarian\ncancer", "Uterine\ncancer"))]

pdf(paste0(pdf_fp, date, "_heatmap.pdf"), width = 14, height = 14)
temp <- copy(heatmap_results[cause_name == "Maternal disorders"]) 
temp[, location_name := factor(location_name, unique(location_name[order(abs_ineff, decreasing = TRUE)]))]
temp <- sort(unique(temp[, location_name]))
heatmap_results[, location_name := factor(location_name, levels = temp)]

plotting <- copy(heatmap_results[super_region_name %in% c("Sub-Saharan Africa", "Latin America and Caribbean", "Southeast Asia, East Asia, and Oceania")])
plotting[, super_region_label := factor(super_region_label, levels = c("Southeast Asia, East Asia,\nand Oceania", "Latin America and Caribbean", "Sub-Saharan Africa"))]
p1 <- ggplot(plotting) + 
  geom_tile(aes(x = cause_label, y = location_name, fill = rank_ineff_total), color = "white") +
  geom_text(aes(y = location_name, x = cause_label, label = ineff_label), color = "white", size = 2.5) +
  ggforce::facet_col(facets = vars(super_region_label), 
                     scales = "free_y", 
                     space = "free") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  scale_fill_gradient(low = "#1f77b4", high = "#d62728", limits = c(0, max(results$rank_ineff_total))) +
  labs(x = "", 
       y = "", 
       fill = "Ranking of Inefficiency") + 
  theme_minimal() + 
  theme(strip.text = element_text(face="bold", size = 12),
        text = element_text(size = 12), 
        axis.text = element_text(size = 7))+
  guides (fill = guide_colourbar(barwidth = 20, barheight = 0.5))

plotting <- copy(heatmap_results[!(super_region_name %in% c("Sub-Saharan Africa", "Latin America and Caribbean", "Southeast Asia, East Asia, and Oceania"))])
p2 <- ggplot(plotting) + 
  geom_tile(aes(x = cause_label, y = location_name, fill = rank_ineff_total), color = "white") +
  geom_text(aes(y = location_name, x = cause_label, label = ineff_label), color = "white", size = 2.5) +
  ggforce::facet_col(facets = vars(super_region_label), 
                     scales = "free_y", 
                     space = "free") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  scale_fill_gradient(low = "#1f77b4", high = "#d62728", limits = c(0, max(results$rank_ineff_total))) +
  labs(x = "", 
       y = "", 
       fill = "Ranking of Inefficiency") + 
  theme_minimal() + 
  theme(strip.text = element_text(face="bold", size = 12),
        text = element_text(size = 12), 
        axis.text = element_text(size = 7))+
  guides (fill = guide_colourbar(barwidth = 20, barheight = 0.5))

ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend="bottom")

dev.off()

## Scatter
all_results[, cause_name := factor(cause_name, levels = c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders", "All causes", "Cardiovascular diseases"))]
pdf(paste0(pdf_fp, date, "_scatter_frontier_allyears.pdf"), width = 12, height = 10)
ggplot(all_results[!(cause_name %in% c("All causes", "Cardiovascular diseases"))]) +
  geom_point(aes(y = val*multiplier, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
  geom_line(aes(y = val_pred*multiplier, x = mean_predictor), alpha = 0.7) +
  geom_point(data = all_results[outliers == TRUE & !(cause_name %in% c("All causes", "Cardiovascular diseases"))], aes(y = val*multiplier, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
  scale_color_manual(values = color_palette_region) +
  facet_wrap(~cause_name, scales = "free", ncol = 2) + 
  labs(title = paste0("Estimated SDI-dependent frontier for DALYs per 100K females scattered against all input data"),
       y = "DALYs per 100K",
       x = "SDI", color = "GBD super-region", 
       size = "Inverse standard error of\nDALY rates") +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
         color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = c(1, 0.05),
        legend.justification = c(1, 0), legend.box = "horizontal", 
        strip.text = element_text(face = "bold", size = 12), 
        text = element_text(size = 12))

for(i in unique(all_results$cause_name)){
  p1 <- ggplot(all_results[cause_name == i]) +
    geom_point(aes(y = val*multiplier, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
    geom_line(aes(y = val_pred*multiplier, x = mean_predictor), alpha = 0.7) +
    geom_point(data = all_results[outliers == TRUE & cause_name == i], aes(y = val*multiplier, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
    scale_color_manual(values = color_palette_region) +
    facet_wrap(~cause_name, scales = "free", ncol = 2) + 
    labs(title = paste0("Estimated SDI-dependent frontier for DALYs per 100K females scattered against all input data"),
         y = "DALYs per 100K",
         x = "SDI", color = "GBD super-region", 
         size = "Inverse standard error of\nDALY rates") +
    theme_minimal() +
    guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
           color = guide_legend(override.aes = list(size = 3)), ncol = 2) +
    theme(legend.position = "bottom",
          legend.justification = c(1, 0), legend.box = "horizontal", 
          strip.text = element_text(face = "bold", size = 12), 
          text = element_text(size = 12))
  print(p1)
}
dev.off()

## SENSITIVITY ANALYSES ############################################################################################
sens_analyses <- c("2spline_notrim", "2spline_nonlineartails", "2spline_xsectional", "3spline", "3spline_notrim")
alt_predictor_name <- "ldi"

for(i in sens_analyses){
  filename <- paste0(i, "_", measure_name, "_inefficiencies.csv")
  
  temp <- lapply(cause_names, function(x) fread(paste0(output_dir, x, "_", filename))) %>% rbindlist()
  temp <- temp[year_id == 2023]
  temp$analysis <- i
  assign(i, temp)
}

filename <- paste0(version, "_", alt_predictor_name, "_", measure_name, "_inefficiencies.csv")
ldi_results <- lapply(cause_names, function(x) fread(paste0(output_dir, x, "_", filename))) %>% rbindlist()
ldi_results <- ldi_results[year_id == 2023]
ldi_results$analysis <- "LDI"

results <- results[year_id == 2023]
results$analysis <- "Primary model"

results <- rbindlist(list(results, `2spline_notrim`, `2spline_nonlineartails`, `2spline_xsectional`, `3spline`, `3spline_notrim`, ldi_results), use.names = TRUE, fill = T)
results[, cause_name := factor(cause_name, levels = c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders"))]
results[, model_label := ifelse(analysis == "Primary model", "Primary model: 2nd degree splines on SDI;\n5% trimming; Panel inputs; Linear tails", 
                                ifelse(analysis == "LDI", "2nd degree splines on ln(LDI);\n5% trimming; Panel inputs; Linear tails",
                                     ifelse(analysis == "2spline_notrim", "2nd degree splines on SDI;\nNo trimming; Panel inputs; Linear tails",
                                            ifelse(analysis == "2spline_nonlineartails", "2nd degree splines on SDI;\n5% trimming; Panel inputs; Non-linear tails",
                                                   ifelse(analysis == "2spline_xsectional", "2nd degree splines on SDI;\n5% trimming; Cross-sectional 2023 inputs; Linear tails",
                                                          ifelse(analysis == "3spline", "3rd degree splines on SDI;\n5% trimming; Panel inputs; Linear tails",
                                                                 ifelse(analysis == "3spline_notrim", "3rd degree splines on SDI;\nNo trimming; Panel inputs; Linear tails", NA)))))))]
results[, model_label := factor(model_label, levels = c("Primary model: 2nd degree splines on SDI;\n5% trimming; Panel inputs; Linear tails",
                                                                 "2nd degree splines on ln(LDI);\n5% trimming; Panel inputs; Linear tails",
                                                                 "2nd degree splines on SDI;\nNo trimming; Panel inputs; Linear tails",
                                                                 "2nd degree splines on SDI;\n5% trimming; Panel inputs; Non-linear tails",
                                                                 "2nd degree splines on SDI;\n5% trimming; Cross-sectional 2023 inputs; Linear tails",
                                                                 "3rd degree splines on SDI;\n5% trimming; Panel inputs; Linear tails",
                                                                 "3rd degree splines on SDI;\nNo trimming; Panel inputs; Linear tails"))]
pdf(paste0(pdf_fp, date, "_scatter_sensitivity_analyses.pdf"), width = 12, height = 10)
for(i in unique(results$cause_name)){
  p1 <- ggplot(results[analysis != "LDI" & cause_name == i]) +
    geom_point(aes(y = val*multiplier, x = mean_predictor, color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
    geom_line(aes(y = val_pred*multiplier, x = mean_predictor), alpha = 0.7) +
    geom_point(data = results[outliers == TRUE & analysis != "LDI" & cause_name == i], aes(y = val*multiplier, x = mean_predictor, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
    scale_color_manual(values = color_palette_region) +
    facet_wrap(~model_label, scales = "free", ncol = 2) + 
    labs(title = paste0("Sensitivity analyses of frontier models scattered against 2023 DALY estimates for, ", i),
         y = "DALYs per 100K in 2023",
         x = "SDI in 2023", color = "GBD super-region", 
         size = "Inverse standard error of\nDALY rates") +
    theme_minimal() +
    guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
           color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
    theme(legend.position = "bottom", legend.box = "horizontal", 
          strip.text = element_text(face = "bold", size = 12), 
          text = element_text(size = 12))
  print(p1)
}

p2 <- ggplot(results[analysis == "LDI"]) +
  geom_point(aes(y = val*multiplier, x = exp(mean_predictor), color = super_region_name, size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), alpha = 0.7) +
  geom_line(aes(y = val_pred*multiplier, x = exp(mean_predictor)), alpha = 0.7) +
  geom_point(data = results[outliers == TRUE & analysis == "LDI"], aes(y = val*multiplier, x = exp(mean_predictor), size = 1/(((upper*multiplier)-(lower*multiplier)) / (2 * 1.96))), color = "black", shape = 4) +
  scale_color_manual(values = color_palette_region) +
  scale_x_log10() +
  facet_wrap(~cause_name, scales = "free", ncol = 2) + 
  labs(title = paste0("Sensitivity analysis using LDI"),
       y = "DALYs per 100K in 2023",
       x = "LDI in 2023", color = "GBD super-region", 
       size = "Inverse standard error of\nDALY rates") +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(shape = 16), ncol = 1), 
         color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = c(1, 0.05),
        legend.justification = c(1, 0), legend.box = "horizontal", 
        strip.text = element_text(face = "bold", size = 12), 
        text = element_text(size = 12))

print(p2)
dev.off()

## REGRESSIONS AGAINST ALL CAUSE AND CVD ##################################################################################################################
to_plot <- copy(results)
to_plot[, female_health := ifelse(cause_name_label %in% c("Breast cancer", "Cervical cancer", "Ovarian cancer", "Uterine cancer", "Maternal disorders"), "Female-dominant health outcome", "Other")]
other_toplot <- copy(to_plot[female_health == "Other"])
to_plot <- to_plot[female_health != "Other"]
other_toplot <- dcast.data.table(other_toplot, super_region_name + location_name ~ cause_name_label, value.var = c("rank_ineff_total", "abs_ineff"))
to_plot <- merge(to_plot, other_toplot, by = c("super_region_name", "location_name"), all = T)

for(i in unique(to_plot$cause_name_label)) {
  assign(i, lm(`abs_ineff_All causes` ~ abs_ineff, data = to_plot[cause_name_label == i]))
  print(i)
  print(summary(get(i)))
}
for(i in unique(to_plot$cause_name_label)) {
  assign(i, lm(`abs_ineff_Cardiovascular diseases` ~ abs_ineff, data = to_plot[cause_name_label == i]))
  print(i)
  print(summary(get(i)))
}
