## HEADER #################################################################
# Date: 10/4/2023
# Purpose: Creating final sets of plots for the gender gaps paper
#          

## SET-UP #################################################################
rm(list=ls())

# Load packages, and install if missing ========================================================================
packages <- c("data.table","tidyverse","ggplot2", "haven", "parallel", "gridExtra", "stringr", "tools", "ggpubr")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}
library(ggh4x, lib.loc = "FILEPATH")

# Read in shared functions =================================================
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

# Set parameters ===========================================================
gbd.version <- 7    
gbd.step <- "iterative"
release <- 9
compare_version <- 7987

save_plots <- T

palette_sex <- c("#00BFC4","#F8766D", "#DFDFDF")

# Set filepaths to save files ==============================================
data_fp <- "FILEPATH"
out_fp <- "FILEPATH"
analysis_fp <- "FILEPATH"

## SCRIPT ##################################################################
top_causes <- fread(paste0(data_fp, "long_top20_burden_estimates_version_", compare_version, ".csv"))
gap_dt <- fread(paste0(data_fp, "top20_absolute_relative_gaps_version_", compare_version, ".csv")) %>% .[cause_id %in% top_causes$cause_id]
full_dataset <- fread(paste0(data_fp, "wide_all_burden_estimates_version_", compare_version, ".csv")) %>% .[cause_id %in% top_causes$cause_id]

# Getting location data
locations <- get_location_metadata(location_set_id = 35, release_id = release) %>% .[level == 1 | level == 0]

# Getting age groups
ages <- get_age_metadata(release_id = release) %>% .[age_group_id %in% c(7:20, 30:32, 235)]
ages[, age_group_name := ifelse(age_group_years_start %in% c(10:24), "10-24 years old", 
                                     ifelse(age_group_years_start %in% c(25:49), "25-49 years old", 
                                            ifelse(age_group_years_start %in% c(50:69), "50-69 years old", 
                                                   "70+ years old")))]

# Getting list of GBD causes
causes <- get_cause_metadata(cause_set_id = 3, release_id = release) %>% .[cause_id %in% top_causes$cause_id]
cause_parents <- get_cause_metadata(cause_set_id = 3, release_id = release) %>% .[cause_id %in% unique(causes$parent_id)]
cause_majors <- get_cause_metadata(cause_set_id = 3, release_id = release) %>% .[cause_id %in% unique(cause_parents$parent_id)]
setnames(cause_parents, c("cause_name", "parent_id", "cause_id"), c("parent_name", "category_id",  "parent_id"))
setnames(cause_majors, c("cause_name", "cause_id"), c("category_name", "category_id"))

# Getting full sets
gap_dt <- merge(gap_dt, causes[, .(cause_id, acause, cause_name, acause_parent)], by = "cause_id", all.x = T)
gap_dt <- merge(gap_dt, locations[, .(location_id, location_name)], by = "location_id", all.x = T)
gap_dt[, signif := ifelse((lower_abs_rate > 0 & upper_abs_rate > 0) | (lower_abs_rate < 0 & upper_abs_rate < 0), TRUE, FALSE)]
gap_dt[, signif_rel := ifelse((lower_rel_rate > 0 & upper_rel_rate > 0) | (lower_rel_rate < 0 & upper_rel_rate < 0), TRUE, FALSE)]

full_dataset[, sex := toTitleCase(sex)]

# Getting options
age_group_options <- unique(full_dataset$age_group_name)
measure_options <- unique(gap_dt$measure_name)

### Figure 1: 
for(m in measure_options){
  for(age in age_group_options){
    to_plot <- copy(gap_dt) %>% .[measure_name == m & age_group_name == age & year_id == 2021]
    
    to_plot[, gap_type := ifelse(abs_rate > 0, "Higher rate among females\nrelative to males", "Higher rate among males\nrelative to females")]
    to_plot[, gap_type := ifelse(abs_rate > 0 & signif == TRUE, "Higher rate among females\nrelative to males",
                                 ifelse(abs_rate < 0 & signif == TRUE, "Higher rate among males\nrelative to females", "No difference in rate"))]
    
    to_plot[, cause_name := factor(cause_name, levels = unique(to_plot$cause_name[order(to_plot[location_id == 1, abs_rate])]))]
    to_plot[, location_name := factor(location_name, levels = c("Global", unique(to_plot[location_id != 1, location_name])))]
    to_plot[, signif_alpha := ifelse(signif == TRUE, 1, 1)]
    
    if(m == "DALYs"){
      title_option <- "Disability-Adjusted Life Year (DALY)"
    } else if(m == "YLDs"){
      title_option <- "Years Lived with Disability (YLD)"
    } else if(m == "YLLs"){
      title_option <- "Years of Life Lost (YLL)"
    }
    
    if(age == "10+ years old (standardized)"){
      age_name <- "10+"
      age_label <- "Age-standardized (10 years and older)"
      title1 <- text_grob(paste0("Global and regional absolute difference in ", title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021"), size = 20, face = "bold")
    } else {
      if(age == "10-24 years old"){
        age_name <- "1024"
      } else if(age == "25-49 years old"){
        age_name <- "2549"
      } else if(age == "50-69 years old"){
        age_name <- "5069"
      } else {
        age_name <- "70+"
      }
      age_label <- age
      title1 <- text_grob(paste0("Global and regional absolute difference in ", title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021"), size = 20, face = "bold")
    }
    
    figure_1.1 <- ggplot(to_plot, aes(x=cause_name, y=abs_rate*100000, label=gap_type)) + 
      geom_bar(stat='identity', aes(fill=gap_type, alpha = I(signif_alpha)), width=.5) +
      scale_fill_manual(name="",
                        values = palette_sex, limits = c("Higher rate among males\nrelative to females", "Higher rate among females\nrelative to males", "No difference in rate")) +
      labs(x="", y = paste0("Absolute difference (females - males) in ", m, " per 100,000"),
           title = paste0("Global and regional absolute difference in ", 
                          title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021")) +
      geom_hline(yintercept = 0, color = 1, lwd = 0.2)+
      theme_bw()+
      facet_wrap(~ location_name)+
      theme(text = element_text(size = 15), legend.position = c(0.8, 0.1), strip.text = element_text(face = "bold"), strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"),
            panel.spacing.x = unit(1.4, "lines"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches")) +
      coord_flip()

    layout_matrix <- rbind(c(1,1,2,2,2), 
                           c(1,1,2,2,2), 
                           c(1,1,2,2,2))
    xaxis1 <- text_grob(paste0("Absolute difference (females - males) in ", m, " per 100,000"), size = 15)
    
    if(save_plots){
      pdf(paste0(out_fp, m, "_", age_name, "_figure1_final2.pdf"), width = 18, height = 14)
      print(figure_1.1)
      dev.off()
    }
  }
}

### Figure 1 with relative gap
for(m in measure_options){
  for(age in age_group_options){
    to_plot <- copy(gap_dt) %>% .[measure_name == m & age_group_name == age & year_id == 2021]
    
    to_plot[, gap_type := ifelse(rel_rate > 0, "Higher rate among females\nrelative to males", "Higher rate among males\nrelative to females")]
    to_plot[, gap_type := ifelse(rel_rate > 0 & signif_rel == TRUE, "Higher rate among females\nrelative to males",
                                 ifelse(rel_rate < 0 & signif_rel == TRUE, "Higher rate among males\nrelative to females", "No difference in rate"))]
    
    to_plot[, cause_name := factor(cause_name, levels = unique(to_plot$cause_name[order(to_plot[location_id == 1, rel_rate])]))]
    to_plot[, location_name := factor(location_name, levels = c("Global", unique(to_plot[location_id != 1, location_name])))]
    
    if(m == "DALYs"){
      title_option <- "Disability-Adjusted Life Year (DALY)"
    } else if(m == "YLDs"){
      title_option <- "Years Lived with Disability (YLD)"
    } else if(m == "YLLs"){
      title_option <- "Years of Life Lost (YLL)"
    }
    
    if(age == "10+ years old (standardized)"){
      age_name <- "10+"
      age_label <- "Age-standardized (10 years and older)"
      title1 <- text_grob(paste0("Global and regional relative gaps in ", title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021"), size = 20, face = "bold")
      
    } else {
      if(age == "10-24 years old"){
        age_name <- "1024"
      } else if(age == "25-49 years old"){
        age_name <- "2549"
      } else if(age == "50-69 years old"){
        age_name <- "5069"
      } else {
        age_name <- "70+"
      }
      age_label <- age
      title1 <- text_grob(paste0("Global and regional relative gaps in ", title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021"), size = 20, face = "bold")
    }
    
    figure_1.1 <- ggplot(to_plot, aes(x=cause_name, y=rel_rate, label=gap_type)) + 
      geom_bar(stat='identity', aes(fill=gap_type), width=.5) +
      scale_fill_manual(name="",
                        values = palette_sex, limits = c("Higher rate among males\nrelative to females", "Higher rate among females\nrelative to males", "No difference in rate")) +
      labs(x="", y = paste0("Relative difference in ", m, " per 100,000\n(females - males)/males"),
           title = paste0("Global and regional relative gaps in ", title_option," rates (per 100,000 population)\nbetween females and males, ", age_label, ", 2021")) +
      geom_hline(yintercept = 0, color = 1, lwd = 0.2)+
      theme_bw()+
      facet_wrap(~ location_name)+
      theme(text = element_text(size = 15), legend.position = c(0.8, 0.1), strip.text = element_text(face = "bold"), strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"),
            panel.spacing.x = unit(1.4, "lines"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches")) +
      coord_flip()

    layout_matrix <- rbind(c(1,1,2,2,2), 
                           c(1,1,2,2,2), 
                           c(1,1,2,2,2))
    xaxis1 <- text_grob(paste0("Relative difference in ", m, " per 100,000\n(females - males)/males"), size = 15)
    
    if(save_plots){
      pdf(paste0(out_fp, "relative_", m, "_", age_name, "_figure1_final2.pdf"), width = 18, height = 14)
      print(figure_1.1)
      dev.off()
    }
  }
}

### Figure 2: 
for(loc in locations$location_name){
  for(age in age_group_options){
    to_plot <- copy(full_dataset) %>% .[age_group_name == age & year_id == 2021 & location_name == loc]
    to_plot[, ranking := base::rank(-DALYs_mean, ties.method = "min"), by = c("sex", "age_group_name", "year_id", "location_name")]
    to_plot[, cause_labels := paste0(ranking, ") ", cause_name)]

    to_plot <- melt(to_plot, measure.vars = c("DALYs_upper", "DALYs_mean", "DALYs_lower", "YLDs_upper", "YLDs_mean", "YLDs_lower", "YLLs_mean", "YLLs_lower", 'YLLs_upper'))
    to_plot[, measure_name := gsub("(_\\w+)", "", variable)]
    
    level_order <- to_plot[variable == "DALYs_mean" & sex_id == 3]
    setorder(level_order, value)
    to_plot[, temp := factor(cause_name, levels = level_order$cause_name)]
    level_order <- to_plot[variable == "DALYs_mean" & sex_id == 2]
    setorder(level_order, value)
    to_plot[, cause_name_female := factor(cause_name, levels = level_order$cause_name)]
    level_order <- to_plot[variable == "DALYs_mean" & sex_id == 1]
    setorder(level_order, value)
    to_plot[, cause_name_male := factor(cause_name, levels = level_order$cause_name)]

    temporary <- copy(gap_dt) %>% .[age_group_name == age & year_id == 2021 & location_name == loc & measure_name == "DALYs"]
    temporary[, gap_type := ifelse(abs_rate > 0, "Higher rate among females\nrelative to males", "Higher rate among males\nrelative to females")]
    temporary[, gap_type := ifelse(abs_rate > 0 & signif == TRUE, "Higher rate among females\nrelative to males",
                                 ifelse(abs_rate < 0 & signif == TRUE, "Higher rate among males\nrelative to females", "No difference in rate"))]
    
    to_plot <- merge(to_plot, temporary[, .(location_name, cause_name, gap_type, year_id, age_group_name)], by = c("location_name", "cause_name", "year_id", "age_group_name"), all.x = T)
    
    if(age == "10+ years old (standardized)"){
      age_name <- "10+"
      age_label <- "Age-standardized (10 years and older)"
    } else {
      if(age == "10-24 years old"){
        age_name <- "1024"
      } else if(age == "25-49 years old"){
        age_name <- "2549"
      } else if(age == "50-69 years old"){
        age_name <- "5069"
      } else {
        age_name <- "70+"
      }
      age_label <- age
    }
    
    upper_limit <- (max(to_plot[variable == "DALYs_mean" & sex_id != 3, value])*1.3)
    female_dalys <- to_plot[measure_name == "DALYs" & sex == "Female" & variable %like% "_mean"]
    setnames(female_dalys, "value", "female_dalys")
    male_dalys <- to_plot[measure_name == "DALYs" & sex == "Male" & variable %like% "_mean"]
    setnames(male_dalys, "value", "male_dalys")
    to_plot <- merge(to_plot, male_dalys[, .(location_name, cause_name, male_dalys, year_id, age_group_name)], by = c("location_name", "cause_name", "year_id", "age_group_name"), all.x = T)
    to_plot <- merge(to_plot, female_dalys[, .(location_name, cause_name, female_dalys, year_id, age_group_name)], by = c("location_name", "cause_name", "year_id", "age_group_name"), all.x = T)
    
    title1 <- text_grob(paste0("Rankings of the top 20 causes of Disability-Adjusted Life Years (DALYs) globally for females and males\n", loc, ", ", age_label, ", 2021"), size = 15, face = "bold")
    xaxis1 <- text_grob("DALYs per 100,000", size = 12)

    # Y axis causes ordered by sex-specific rankings with names on the outside
    figure_2.1.2 <- ggplot(to_plot[measure_name != "DALYs" & sex == "Female" & variable %like% "_mean"], aes(x = -value, y = cause_name_female, fill = interaction(gap_type, measure_name))) + 
      geom_bar(position = "stack", stat="identity") + 
      geom_text(aes(x = -female_dalys-100, y = cause_name_female, label = cause_labels), hjust = "right") +
      labs(x = "", y = "", alpha = "") + 
      scale_fill_manual(name="", values = c("Higher rate among males\nrelative to females.YLLs" = alpha(palette_sex[1], 1), 
                                            "Higher rate among males\nrelative to females.YLDs" = alpha(palette_sex[1], 0.4),
                                            "Higher rate among females\nrelative to males.YLLs" = alpha(palette_sex[2], 1), 
                                            "Higher rate among females\nrelative to males.YLDs" = alpha(palette_sex[2], 0.4), 
                                            "No difference in rate.YLLs" = alpha(palette_sex[3], 1), 
                                            "No difference in rate.YLDs" = alpha(palette_sex[3], 0.4)), 
                        breaks = c("Higher rate among males\nrelative to females.YLLs", 
                                   "Higher rate among males\nrelative to females.YLDs", 
                                   "Higher rate among females\nrelative to males.YLLs", 
                                   "Higher rate among females\nrelative to males.YLDs", 
                                   "No difference in rate.YLLs", 
                                   "No difference in rate.YLDs"), 
                        labels = c("YLLs in outcomes with higher DALY\nrate among males relative to females", 
                                   "YLDs in outcomes with higher DALY\nrate among males relative to females",
                                   "YLLs in outcomes with higher DALY\nrate among females relative to males",
                                   "YLDs in outcomes with higher DALY\nrate among females relative to males",
                                   "YLLs in outcomes with no difference\nin DALY rate", 
                                   "YLDs in outcomes with no difference\nin DALY rate")) +
      scale_x_continuous(expand = c(0, 0), limits = c(-upper_limit, 0), labels = abs) +
      scale_y_discrete(position = "right") +
      guides(fill = "none") +
      facet_wrap(~sex) + 
      theme_bw() +
      theme(text = element_text(size = 15), 
            legend.position = c(0.75, 0.2), legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(face = "bold"), strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"),
            panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), plot.margin=unit(c(1,-0.5,1,1), "cm"))
    figure_2.2.2 <- ggplot(to_plot[measure_name != "DALYs" & sex == "Male" & variable %like% "_mean"], aes(x = value, y = cause_name_male, fill = interaction(gap_type, measure_name))) + 
      geom_bar(position = "stack", stat="identity") + 
      geom_text(aes(x = male_dalys+100, y = cause_name_male, label = cause_labels), hjust = "left") +
      labs(x = "", y = "", alpha = "") + 
      scale_fill_manual(name="", values = c("Higher rate among males\nrelative to females.YLLs" = alpha(palette_sex[1], 1), 
                                            "Higher rate among males\nrelative to females.YLDs" = alpha(palette_sex[1], 0.4),
                                            "Higher rate among females\nrelative to males.YLLs" = alpha(palette_sex[2], 1), 
                                            "Higher rate among females\nrelative to males.YLDs" = alpha(palette_sex[2], 0.4), 
                                            "No difference in rate.YLLs" = alpha(palette_sex[3], 1), 
                                            "No difference in rate.YLDs" = alpha(palette_sex[3], 0.4)), 
                        breaks = c("Higher rate among males\nrelative to females.YLLs", 
                                   "Higher rate among males\nrelative to females.YLDs", 
                                   "Higher rate among females\nrelative to males.YLLs", 
                                   "Higher rate among females\nrelative to males.YLDs", 
                                   "No difference in rate.YLLs", 
                                   "No difference in rate.YLDs"), 
                        labels = c("YLLs in outcomes with higher DALY\nrate among males relative to females", 
                                   "YLDs in outcomes with higher DALY\nrate among males relative to females",
                                   "YLLs in outcomes with higher DALY\nrate among females relative to males",
                                   "YLDs in outcomes with higher DALY\nrate among females relative to males",
                                   "YLLs in outcomes with no difference\nin DALY rate", 
                                   "YLDs in outcomes with no difference\nin DALY rate")) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, upper_limit)) +
      facet_wrap(~sex) + 
      theme_bw() +
      theme(text = element_text(size = 15), 
            legend.position = c(0.75, 0.2), legend.title = element_blank(),
            legend.text = element_text(size = 10), 
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(face = "bold"), strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"),
            panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), plot.margin=unit(c(1,1,1,-0.5), "cm"))
    
    temp_numbers <- unique(to_plot[, .(cause_name_female, cause_name_male, cause_id)])
    temp_numbers[, female_rankings := as.numeric(cause_name_female)]
    temp_numbers[, male_rankings := as.numeric(cause_name_male)]
    temp_numbers[, change := ifelse(female_rankings > male_rankings, "Female ranking is higher", 
                                    ifelse(female_rankings == male_rankings, "Rank is the same", "Male ranking is higher"))]
    temp_numbers <- melt(temp_numbers, id.vars = c("cause_id", "change"), measure.vars = c("female_rankings", "male_rankings"), value.name = "rank", variable.name = "sex")
    setDT(temp_numbers)
    temp_numbers[, sex := ifelse(sex == "female_rankings", "Female", "Male")]
    
    to_plot <- merge(to_plot, temp_numbers, by = c("cause_id", "sex"), all.x = T)
    to_plot[, sex := factor(sex, levels = c("Female", "Male", "both"))]
    figure_2.3 <- ggplot(to_plot[measure_name != "DALYs" & variable %like% "_mean" & sex != "both"], aes(x = sex, y = rank, group = as.factor(cause_id), color = gap_type)) + 
      geom_line() + 
      labs(x = "", y = "", linetype = "")  + 
      scale_color_manual(values = c("Higher rate among males\nrelative to females" = palette_sex[1], 
                                    "Higher rate among females\nrelative to males" = palette_sex[2], 
                                    "No difference in rate" = palette_sex[3])) +
      scale_x_discrete(expand = c(0, 0)) +
      guides(linetype = "none", color = "none") + 
      theme_bw() +
      theme(text = element_text(size = 15), legend.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            strip.text = element_text(face = "bold"), 
            strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"), 
            plot.margin=unit(c(1.5,0,1.2,-0.75), "cm"))
    
    layout_matrix <- rbind(c(1,1,1,1,2,3,3,3,3),
                           c(1,1,1,1,2,3,3,3,3),
                           c(1,1,1,1,2,3,3,3,3))
    if(save_plots){
      pdf(paste0(out_fp, loc, "_", age_name, "_figure2_final2.pdf"), width = 15, height = 10)
      grid.arrange(grobs = list(figure_2.1.2, figure_2.3, figure_2.2.2), top = title1, bottom = xaxis1, layout_matrix = layout_matrix)
      dev.off()
    }
  }
}

### Figure 3: 
plot2_data <- copy(full_dataset[location_name == "Global" & sex != "both" & year_id == 2021 & age_group_name != "10+ years old (standardized)"])
plot2_data <- merge(plot2_data, gap_dt[, .(cause_id, age_group_name, location_id, year_id, signif, measure_name)], by = c("cause_id", "age_group_name", "location_id", "year_id"), all.x = T)

to_plot <- copy(plot2_data)
custom_order <- c("COVID-19", "Ischemic heart disease", "Road injuries", "Stroke", "Tracheal, bronchus, and lung cancer", "Cirrhosis and other chronic liver diseases", "Chronic obstructive pulmonary disease",
                  "Low back pain", "Depressive disorders", "Tuberculosis", "Headache disorders", "Anxiety disorders", "Other musculoskeletal disorders", "Falls", "Lower respiratory infections",
                  "Chronic kidney disease", "Alzheimer's disease and other dementias",  "Diabetes mellitus", "HIV/AIDS", "Age-related and other hearing loss")
to_plot$cause_name <- factor(to_plot$cause_name, levels = custom_order)
to_plot[, age_group_name := gsub(" years old", "", age_group_name)]
to_plot[, age_group_name := factor(age_group_name, levels = c("10-24", "25-49", "50-69", "70+"))]

plot2_alt <-
  ggplot(data = to_plot[!is.na(signif) & measure_name == "DALYs"]) + 
  geom_line(aes(x = age_group_name, y = DALYs_mean, color=factor(sex_id), group = factor(sex_id))) +
  geom_point(aes(x = age_group_name, y = DALYs_mean, shape=signif, color = factor(sex_id)),size = 1.5) +
  geom_ribbon(aes(x = age_group_name, ymin = DALYs_lower, ymax = DALYs_upper, group = factor(sex_id), fill = factor(sex_id)), alpha = 0.2) +
  scale_color_manual(values = palette_sex, labels = c("Male", "Female")) +
  scale_fill_manual(values = palette_sex, labels = c("Male", "Female")) +
  scale_shape_manual(values = c(4, 16), labels = c("No difference", "Observed difference")) + 
  labs(title = paste0("Global Disability-Adjusted Life Year (DALY) rates (per 100,000 population)\nfor females and males across age groups, 2021"),
       y = "DALYs per 100,000", x = "Age Groups (years)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=10,angle=45,hjust=1),axis.text.y = element_text(size=8),
        legend.text = element_text(size=9), legend.title=element_blank(),
        strip.text = element_text(size = 8, face = "bold"), strip.background = element_rect(fill = "white", color = "black", size = 1.5, linetype = "solid"), strip.text.y.right = element_text(angle = 0)) +
  facet_wrap(cause_name ~ ., strip.position = "right", ncol = 1, scale = "free_y")

pdf(paste0(out_fp, "DALYs_figure3_final2.pdf"), width = 7, height = 16)
print(plot2_alt)
dev.off()


## datasets
top20_data <- read_csv(paste0("FILEPATH/top20_draws_rates_version_", compare_version, ".csv"))
top20_data <- top20_data %>% 
  filter(sex_id != 3, measure_name == "DALYs" & year_id %in% c(1990, 2021))

top20_data <- top20_data[, colSums(is.na(top20_data)) < nrow(top20_data)]

data_long <- top20_data %>%
  pivot_longer(starts_with("draw"),
               names_to = "draw", 
               values_to = "value")

data_female <- data_long %>%
  filter(sex_id == 2) %>%
  rename(value_female = value) %>%
  select(age_group_id, location_id, year_id, cause_id, draw, value_female)

data_male <- data_long %>%
  filter(sex_id == 1) %>%
  rename(value_male = value) %>%
  select(age_group_id, location_id, year_id, cause_id, draw, value_male)

data_col <- left_join(data_female, data_male, 
                      by = c("age_group_id", "location_id", "year_id", "cause_id", "draw"))

data_col <- data_col %>% 
  mutate(abs_gap = value_female - value_male)
data_col <- data_col %>% 
  mutate(rel_gap = abs_gap/value_male)
setDT(data_col)
data_col <- dcast.data.table(data_col, cause_id + age_group_id + location_id + draw ~ year_id, value.var = c("abs_gap", "rel_gap"))
data_col[, diff := `abs_gap_2021` - `abs_gap_1990`]
data_col[, rel_diff := `rel_gap_2021` - `rel_gap_1990`]

data_col[, `:=` (mean_diff = mean(diff), lower_diff = quantile(diff, 0.025), upper_diff = quantile(diff, 0.975)), by = .(cause_id, age_group_id, location_id)]
data_col[, `:=` (mean_reldiff = mean(rel_diff, na.rm = T), lower_reldiff = quantile(rel_diff, 0.025, na.rm = T), upper_reldiff = quantile(rel_diff, 0.975, na.rm = T)), by = .(cause_id, age_group_id, location_id)]

time_signif <- copy(data_col) %>% .[, .(cause_id, age_group_id, location_id, mean_diff, lower_diff, upper_diff, mean_reldiff, lower_reldiff, upper_reldiff)]
time_signif <- unique(time_signif)
time_signif[, signif := ifelse((lower_diff > 0 & upper_diff > 0) | (lower_diff < 0 & upper_diff < 0), TRUE, FALSE)]
time_signif[, signif_rel := ifelse((lower_reldiff > 0 & upper_reldiff > 0) | (lower_reldiff < 0 & upper_reldiff < 0), TRUE, FALSE)]

time_signif[, age_group_name := ifelse(age_group_id == 1099, "10+ years old (standardized)", 
                                       ifelse(age_group_id == 1024, "10-24 years old", 
                                              ifelse(age_group_id == 2549, "25-49 years old", 
                                                     ifelse(age_group_id == 25, "50-69 years old", 
                                                            ifelse(age_group_id == 26, "70+ years old", NA)))))]
### Figure 4: Change in absolute difference in DALYs for the top twenty outcomes over time from 1990 to 2021
for(age in age_group_options){
  to_plot <- copy(gap_dt) %>% .[age_group_name == age]
  to_plot[, signif := NULL]
  
  temp <- dcast.data.table(to_plot, cause_id + age_group_name + measure_name + measure_id + location_id + location_name + cause_name + acause + acause_parent ~ year_id, value.var = c("abs_rate", "rel_rate", "lower_rel_count", "upper_rel_count", "lower_abs_rate", "upper_abs_rate"))
  
  temp <- merge(temp, time_signif[, .(cause_id, age_group_name, location_id, signif)], by = c("location_id", "cause_id", "age_group_name"), all.x = T)
  
  temp[, `:=` (increasing = ifelse((abs(abs_rate_2021) > abs(abs_rate_1990)) & signif == TRUE, "Absolute Gap Increased", 
                                   ifelse(abs(abs_rate_2021) == abs(abs_rate_1990) | signif == FALSE, "No Change in Absolute Gap", "Absolute Gap Decreased")), 
               flipped = ifelse((abs_rate_2021 > 0 & abs_rate_1990 < 0) | (abs_rate_1990 > 0 & abs_rate_2021 < 0), "Flipped Absolute Gap", "No Flip"))]
  temp <- unique(temp[, .(cause_id, age_group_name, measure_name, measure_id, location_id, cause_name, increasing, flipped, signif)])
  to_plot <- merge(to_plot, temp, by = c("cause_id", "age_group_name", "measure_name", "measure_id", "location_id", "cause_name"), all = T)
  to_plot[, location_name := factor(location_name, levels = c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))]
  
  to_plot_alt <- copy(to_plot)


  if(age == "10+ years old (standardized)"){
    age_name <- "10+"
    age_label <- "Age-standardized (10 years and older)"
    to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Tracheal, bronchus, and lung cancer"), (abs_rate*100000)-18,
                                            ifelse(cause_name %in% c("Falls"), (abs_rate*100000)-21, 
                                                   ifelse(cause_name %in% c("Diabetes mellitus"), (abs_rate*100000)+10, 
                                                          ifelse(cause_name %in% c("Cirrhosis and other chronic liver diseases"), (abs_rate*100000)+18, 
                                                                 ifelse(cause_name %in% c("Depressive disorders"), (abs_rate*100000)+12, (abs_rate*100000))))))]
  } else {
    if(age == "10-24 years old"){
      age_name <- "1024"

      to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Anxiety disorders"), (abs_rate*100000)+20,
                                              ifelse(cause_name %in% c("Diabetes mellitus"), (abs_rate*100000)+50, 
                                                     ifelse(cause_name %in% c("Lower respiratory infections"), (abs_rate*100000)+38, 
                                                            ifelse(cause_name %in% c("Tuberculosis"), (abs_rate*100000)+25, 
                                                                   ifelse(cause_name %in% c("Cirrhosis and other chronic liver diseases"), (abs_rate*100000)+10, 
                                                                          ifelse(cause_name %in% c("Alzheimer's disease and other dementias"), (abs_rate*100000)+0, 
                                                                                 ifelse(cause_name %in% c("Chronic obstructive pulmonary disease", "Falls"), (abs_rate*100000)-10,
                                                                                        ifelse(cause_name %in% c("Tracheal, bronchus, and lung cancer"), (abs_rate*100000)-25,
                                                                                               ifelse(cause_name %in% c("Stroke"), (abs_rate*100000)-37,
                                                                                                      ifelse(cause_name %in% c("COVID-19"), (abs_rate*100000)-47,
                                                                                                             ifelse(cause_name %in% c("Age-related and other hearing loss"), (abs_rate*100000)-55,
                                                                                                                    ifelse(cause_name %in% c("Chronic kidney disease"), (abs_rate*100000)-60,
                                                                                                                           ifelse(cause_name %in% c("Ischemic heart disease"), (abs_rate*100000)-65, (abs_rate*100000))))))))))))))]
    } else if(age == "25-49 years old"){
      age_name <- "2549"
      to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Depressive disorders"), (abs_rate*100000)-5,
                                              ifelse(cause_name %in% c("Age-related and other hearing loss"), (abs_rate*100000)+5, 
                                                     ifelse(cause_name %in% c("Lower respiratory infections"), (abs_rate*100000)-5, (abs_rate*100000))))]
    } else if(age == "50-69 years old"){
      age_name <- "5069"
      to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Anxiety disorders"), (abs_rate*100000)-5,
                                              ifelse(cause_name %in% c("HIV/AIDS", "Diabetes mellitus"), (abs_rate*100000)+5, 
                                                     ifelse(cause_name %in% c("Chronic kidney disease", "Chronic obstructive pulmonary disease"), (abs_rate*100000)+25, (abs_rate*100000))))]
    } else {
      age_name <- "70+"
      to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Falls"), (abs_rate*100000)+45,
                                              ifelse(cause_name %in% c("Depressive disorders", "Road injuries"), (abs_rate*100000)+35, 
                                                     ifelse(cause_name %in% c("Anxiety disorders", "Chronic kidney disease"), (abs_rate*100000)+25, 
                                                            ifelse(cause_name %in% c("Headache disorders", "Other musculoskeletal disorders", "Age-related and other hearing loss", "Cirrhosis and other chronic liver diseases"), (abs_rate*100000)-15, 
                                                                   ifelse(cause_name %in% c("Tuberculosis"), (abs_rate*100000)-20, (abs_rate*100000))))))]
    }
    age_label <- age
  }
  
  to_plot[, signif_shapes := ifelse(signif == TRUE, 16, 4)]

  figure_4.3 <- ggplot() + 
    geom_point(data = to_plot[location_id == 1 & measure_name == "DALYs" & !(cause_name == "COVID-19" & year_id == 1990) & year_id %in% c(1990, 2021)], aes(x = year_id, y = abs_rate*100000, color = cause_name, shape = increasing), alpha = 0.75, size = 5) +
    geom_line(data = to_plot[location_id == 1 & measure_name == "DALYs" & !(cause_name == "COVID-19" & year_id == 1990) & year_id %in% c(1990, 2021)], aes(x = year_id, y = abs_rate*100000, color = cause_name, linetype = increasing), alpha = 0.75, size = 1.2) + 
    geom_text(data = to_plot_alt[location_id == 1 & measure_name == "DALYs" & year_id == 2021], aes(x = 2021.5, y = abs_rate_jitter, label = cause_name), hjust = 0, size = 5) +
    geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") + 
    scale_x_continuous("Year", breaks = c(1990, 2021), labels = c("1990", "2021"), limits = c(1989, 2037)) + 
    scale_shape_manual(breaks = c("No Change in Absolute Gap", "Absolute Gap Increased", "Absolute Gap Decreased"), values=c(4, 16, 16), labels = c("No Change in Absolute Gap", "Absolute Gap Increased", "Absolute Gap Decreased")) +
    scale_linetype_manual(breaks = c("No Change in Absolute Gap", "Absolute Gap Increased", "Absolute Gap Decreased"), values=c("dotted", "dashed", "solid"), labels = c("No Change in Absolute Gap", "Absolute Gap Increased", "Absolute Gap Decreased")) +
    labs(title = paste0("Change in global absolute difference in Disability-Adjusted Life Year (DALY) rates (per 100,000 population)\namong females and males between 1990 and 2021,\n", age_label), y = "Absolute Difference (females - males) in DALYs per 100,000", linetype = "", color = "Health outcome", shape = "") + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), 
          text = element_text(size=20),
          panel.grid.major.x = element_blank(), 
          panel.border = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = "bottom") +
    guides(color = "none", shape = "none", linetype = guide_legend(override.aes = list(size = 15)))
  

  if(save_plots){
    pdf(paste0(out_fp, age_name, "_figure4_final2.pdf"), width = 15, height = 20)
    print(figure_4.3)
    dev.off()
  }
}

age <- "10+ years old (standardized)"
  to_plot <- copy(gap_dt) %>% .[age_group_name == age & year_id %in% c(1990, 2021)]
  to_plot[, signif := NULL]
  
  temp <- dcast.data.table(to_plot, cause_id + age_group_name + measure_name + measure_id + location_id + location_name + cause_name + acause + acause_parent ~ year_id, value.var = c("abs_rate", "rel_rate", "lower_rel_count", "upper_rel_count", "lower_abs_rate", "upper_abs_rate"))
  
  temp <- merge(temp, time_signif[, .(cause_id, age_group_name, location_id, signif_rel)], by = c("location_id", "cause_id", "age_group_name"), all.x = T)
  
  temp[, `:=` (increasing = ifelse((abs(rel_rate_2021) > abs(rel_rate_1990)) & signif_rel == TRUE, "Relative Gap Increased", 
                                   ifelse(abs(rel_rate_2021) == abs(rel_rate_1990) | signif_rel == FALSE, "No Change in Relative Gap", "Relative Gap Decreased")), 
               flipped = ifelse((rel_rate_2021 > 0 & rel_rate_1990 < 0) | (rel_rate_1990 > 0 & rel_rate_2021 < 0), "Flipped Relative Gap", "No Flip"))]
  temp <- unique(temp[, .(cause_id, age_group_name, measure_name, measure_id, location_id, cause_name, increasing, flipped, signif_rel)])
  to_plot <- merge(to_plot, temp, by = c("cause_id", "age_group_name", "measure_name", "measure_id", "location_id", "cause_name"), all = T)
  to_plot[, location_name := factor(location_name, levels = c("Global", "Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", "North Africa and Middle East", "South Asia", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa"))]
  to_plot_alt <- copy(to_plot)
  
  
  if(age == "10+ years old (standardized)"){
    age_name <- "10+"
    age_label <- "Age-standardized (10 years and older)"
    to_plot_alt[, abs_rate_jitter := ifelse(cause_name %in% c("Ischemic heart disease"), rel_rate-1.5, 
                                            ifelse(cause_name == "Tracheal, bronchus, and lung cancer", rel_rate-0.5,
                                            ifelse(cause_name == "Cirrhosis and other chronic liver diseases", rel_rate+1, rel_rate)))]
  } 

  figure_4.3 <- ggplot() + 
    geom_point(data = to_plot[location_id == 1 & measure_name == "DALYs" & !(cause_name == "COVID-19" & year_id == 1990) & !is.na(rel_rate)], aes(x = year_id, y = rel_rate, color = cause_name, shape = increasing), alpha = 0.75, size = 5) +
    geom_line(data = to_plot[location_id == 1 & measure_name == "DALYs" & !(cause_name == "COVID-19" & year_id == 1990) & !is.na(increasing)], aes(x = year_id, y = rel_rate, color = cause_name, linetype = increasing), alpha = 0.75, size = 1.2) + 
    geom_text(data = to_plot_alt[location_id == 1 & measure_name == "DALYs" & year_id == 2021 & !is.na(rel_rate)], aes(x = 2021.5, y = abs_rate_jitter, label = cause_name), hjust = 0, size = 5) +
    geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") + 
    scale_x_continuous("Year", breaks = c(1990, 2021), labels = c("1990", "2021"), limits = c(1989, 2037)) + 
    scale_shape_manual(breaks = c("No Change in Relative Gap", "Relative Gap Increased", "Relative Gap Decreased"), values=c(4, 16, 16), labels = c("No Change in Relative Gap", "Relative Gap Increased", "Relative Gap Decreased")) +
    scale_linetype_manual(breaks = c("No Change in Relative Gap", "Relative Gap Increased", "Relative Gap Decreased"), values=c("dotted", "dashed", "solid"), labels = c("No Change in Relative Gap", "Absolute Relative Increased", "Relative Gap Decreased")) +
    labs(title = paste0("Change in global relative gaps in Disability-Adjusted Life Year (DALY) rates (per 100,000 population)\namong females and males between 1990 and 2021,\n", age_label), y = "Relative Difference ((females - males)/males) in DALYs per 100,000", linetype = "", color = "Health outcome") + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), 
          text = element_text(size=20),
          panel.grid.major.x = element_blank(), 
          panel.border = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = "bottom") +
    guides(color = "none")
  
  if(save_plots){
    pdf(paste0(out_fp, age_name, "_figure4_relative_final2.pdf"), width = 15, height = 20)
    print(figure_4.3)
    dev.off()
  }


  
### Figure 5 of time trend
for(i in unique(full_dataset$location_name)){
  to_plot <- copy(full_dataset) %>% .[location_name == i & age_group_name %like% "standardized" & sex != "both"]
  to_plot <- melt(to_plot, measure.vars = c("DALYs_upper", "DALYs_mean", "DALYs_lower", "YLDs_upper", "YLDs_mean", "YLDs_lower", "YLLs_mean", "YLLs_lower", 'YLLs_upper'))
  to_plot[, measure_name := gsub("(_\\w+)", "", variable)]
  to_plot <- to_plot[measure_name == "DALYs"]
  to_plot <- merge(to_plot, gap_dt[, .(cause_id, age_group_name, location_id, year_id, signif, measure_name)], by = c("cause_id", "age_group_name", "location_id", "year_id", "measure_name"), all.x = T)
  
  temp <- dcast.data.table(to_plot, cause_id + age_group_name + measure_name + location_id + location_name + cause_name + acause + year_id + variable + signif ~ sex, value.var = c("value"))
  
  temp[, cause_name := ifelse(cause_name %like% "Age-related and", "Age-related and\nother hearing loss", 
                              ifelse(cause_name %like% "dementias", "Alzheimer's disease and\n other dementias", 
                                     ifelse(cause_name %like% "obstructive", "Chronic obstructive\npulmonary disease", 
                                            ifelse(cause_name %like% "chronic liver", "Cirrhosis and other\nchronic liver diseases", 
                                                   ifelse(cause_name %like% "bronchus", "Tracheal, bronchus, and\nlung cancer", cause_name)))))]
  temp[, fill_value := ifelse(Female > Male & signif == TRUE, "Rate among females >\nRate among males", 
                              ifelse(Female < Male & signif == TRUE, "Rate among males >\nRate among females", "No difference in rates"))]
  figure_5 <- ggplot(temp[variable %like% "mean"]) + 
    geom_ribbon(aes(x = year_id, ymin=Female, ymax=Male, fill=fill_value), alpha=0.3) +
    geom_line(aes(x = year_id, y = Female, color = "Female"), linewidth = 0.6) + 
    geom_line(aes(x = year_id, y = Male, color = "Male"), linewidth = 0.6) + 
    scale_color_manual(limits = c("Male", "Female"), values = c("#00BFC4", "#F8766D"), labels = c("Male", "Female")) +
    scale_fill_manual(limits = c("Rate among males >\nRate among females", "Rate among females >\nRate among males", "No difference in rates"), values = c("#00BFC4", "#F8766D", "#DFDFDF"), labels = c("Rate among males >\nRate among females", "Rate among females >\nRate among males", "No difference in rates")) +
    facet_wrap(~cause_name) + 
    labs(title = paste0("10+ age-standardized DALYs per 100,000 among males and females over time,\n", i), color = "", y = "DALYs per 100,000", x = "Year", fill = "") +
    theme_bw() + 
    theme(axis.text.x = element_text(size=10,angle=45,hjust=1),
          axis.text.y = element_text(size=8),
          legend.text = element_text(size=9), 
          strip.text = element_text(size = 8, face = "bold"), strip.background = element_rect(fill = "white", color = "black", linewidth = 1.5, linetype = "solid"), strip.text.y.right = element_text(angle = 0))
  
  if(save_plots){
    pdf(paste0(out_fp, i, "_figure5_final2.pdf"), width = 12, height = 10)
    print(figure_5)
    dev.off()
  }
  
}
  