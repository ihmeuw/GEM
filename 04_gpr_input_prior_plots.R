#############################################################################################################################################################################
# Purpose: To create plots of raked and un-raked GPR input and output data
#############################################################################################################################################################################

rm(list=ls())

output_dir <- paste0('/FILEPATH/')


##### 0. SET UP #############################################################################################################################################################

#libraries
pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4,boot)

#location metadata
source(file.path("/FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)

#not functions
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

#plotting format
aesth <- theme_bw() + theme(axis.title = element_text(size=12,face='bold'),axis.text =element_text(size=10,face='bold'),plot.title =element_text(size=13,face='bold'),
                            strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top',legend.title=element_blank())


##### 1. GET + FORMAT DATA ##################################################################################################################################################

#pull data
gpr_final <- fread('/FILEPATH/monthly_gpr_output_draws.csv')
gpr_final$V1 <- NULL
gpr_final$location_id <- NULL

#fill in blanks
gpr_final[toupper(sex) == 'MALE', sex := 'Male']
gpr_final[toupper(sex) == 'FEMALE', sex := 'Female']
gpr_final[toupper(sex) == 'BOTH', sex := 'Both']

gpr_final[toupper(sex) == 'MALE', sex_id := 1]
gpr_final[toupper(sex) == 'FEMALE', sex_id := 2]
gpr_final[toupper(sex) == 'BOTH', sex_id := 3]

#logit transform + multiple for offsets
gpr_final[, gpr_pred := inv.logit(gpr_mean)*0.95]
gpr_final[, gpr_lower_ci := inv.logit(gpr_lower)*0.95]
gpr_final[, gpr_upper_ci := inv.logit(gpr_upper)*0.95]


##### 2. RAKE DATA ##########################################################################################################################################################

#get covid population data
pop_full <- fread("/FILEPATH/")
adults <- c(66, 67, 9:19,20,30:32,235) #18 and older
adult_pop <- pop_full[age_group_id %in% adults, lapply(.SD, function(x) sum(x)), by=c("location_id", "sex_id"), .SDcols = "population"]
adult_pop <- merge(adult_pop, hierarchy[, .(location_id, ihme_loc_id)], by = c('location_id'))

gpr_final <- merge(gpr_final, adult_pop, by=c("ihme_loc_id", "sex_id"), all.x = T)

#split into both and male+female
rake <- gpr_final[indicator %like% 'vaccinated' & sex_id == 3]
rake_sex <- gpr_final[indicator %like% 'vaccinated' & sex_id != 3]

#rake male+female
rake_sex[, wt_mean_sex := weighted.mean(gpr_pred, w=population), by=.(month, ihme_loc_id, indicator)]

#merge
scalars <- merge(rake_sex[, c('month', 'ihme_loc_id', 'sex_id', 'indicator', 'wt_mean_sex')],
                 rake[, c('month', 'ihme_loc_id', 'indicator', 'gpr_pred')],
                 by = c('month', 'ihme_loc_id', 'indicator'))

#caclulate scalar/ratio
scalars[, scale := gpr_pred/wt_mean_sex] 
scalars <- as.data.table(unique(scalars[, c('ihme_loc_id', 'month', 'indicator', 'scale')]))

#merge onto data
gpr_final <- merge(gpr_final, scalars, by = c('month', 'ihme_loc_id', 'indicator'), all = T)

gpr_final[!is.na(scale) & sex != 'Both', raked_pred := gpr_pred*scale]
gpr_final[is.na(scale) | sex == 'Both', raked_pred := gpr_pred]

gpr_final[!is.na(scale) & sex != 'Both', raked_lower := gpr_lower_ci*scale]
gpr_final[is.na(scale) | sex == 'Both', raked_lower := gpr_lower_ci]

gpr_final[!is.na(scale) & sex != 'Both', raked_upper := gpr_upper_ci*scale]
gpr_final[is.na(scale) | sex == 'Both', raked_upper := gpr_upper_ci]

gpr_final[raked_upper >= 1, raked_upper := 1]
gpr_final[raked_lower >= 1, raked_upper := 1]
gpr_final[raked_lower <= 0, raked_lower := 0]


##### 3. PLOT + VET REGRESSION ##############################################################################################################################

#create plot data
plot_data <- gpr_final[month < 22 & month >= 3] 
plot_data[data_source %like% 'CDC' | data_source %like% 'Our World in Data', data_source := 'Official']
plot_data[data_source %like% 'Facebook Global Symptoms Survey', data_source := 'COVID-19 Trends and Impact Survey']

#fix gender vs. sex
plot_data[sex == "Female", gender := 'Women']
plot_data[sex == "Male", gender := 'Men']
plot_data[sex == "Both", gender := 'Both']

##### input data vs. linear regression vs. gpr -------------------

gg <-list()

for (i in unique(plot_data$indicator)) {
  for (r in unique(plot_data[indicator == i, super_region_name])){
    if (i == 'hesitancy') {
      for (a in unique(plot_data[indicator == i & super_region_name == r, age])) {
        gg[[paste0(i,r,a)]] <- ggplot(plot_data[indicator == i & super_region_name == r & age == a], 
                                      aes(x = month, y = proportion, color = gender, fill = gender, group = interaction(gender, age))) +
          geom_point(data = plot_data[indicator == i & super_region_name == r & age == a & data_source != ''], 
                     aes(x = month, y = proportion, color = gender, fill = gender, shape = data_source, group = interaction(gender, age))) +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(aes(y = prop_pred, linetype = 'regression')) +
          geom_line(aes(y = gpr_pred, linetype = 'gpr')) +
          geom_ribbon(aes(ymin = gpr_lower_ci, ymax = gpr_upper_ci), alpha = 0.3, colour = NA) +
          scale_color_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue')) +
          scale_fill_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue')) +
          facet_wrap(~ihme_loc_id) +
          labs(title = paste0(r, ": ", i),
               caption = 'January 2020 - September 2021 (Months 1-21)') +
          aesth
      }
    }else {
     gg[[paste0(i,r)]] <- ggplot(plot_data[indicator == i & super_region_name == r], 
                                 aes(x = month, y = proportion, color = gender, fill = gender, group = interaction(gender, age))) +
       geom_point(data = plot_data[indicator == i & super_region_name == r & data_source != ""], 
                  aes(x = month, y = proportion, color = gender, fill = gender, shape = data_source, group = interaction(gender, age))) +
       geom_errorbar(aes(ymin = lower, ymax = upper)) +
       geom_line(aes(y = prop_pred, linetype = 'regression')) +
       geom_line(aes(y = gpr_pred, linetype = 'gpr')) +
       geom_ribbon(aes(ymin = gpr_lower_ci, ymax = gpr_upper_ci), alpha = 0.3, colour = NA) +
       scale_color_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue', 'Both' = 'green3')) +
       scale_fill_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue', 'Both' = 'green3')) +
       facet_wrap(~ihme_loc_id) +
       labs(title = paste0(r, ": ", i),
            caption = 'January 2020 - September 2021 (Months 1-21)') +
       aesth
    }
  }
}

pdf(paste0(output_dir, 'FILEPATH.pdf'),14,8)
print(gg[1:length(gg)])
dev.off()

rm(gg)


##### input data vs raked gpr ------------------------------------

gg <-list()

for (i in unique(plot_data$indicator)) {
  for (r in unique(plot_data[indicator == i, super_region_name])){
    if (i == 'hesitancy') {
      for (a in unique(plot_data[indicator == i & super_region_name == r, age])) {
        gg[[paste0(i,r,a)]] <- ggplot(plot_data[indicator == i & super_region_name == r & age == a], 
                                      aes(x = month, y = proportion, color = gender, fill = gender, group = interaction(gender, age))) +
          geom_point(data = plot_data[indicator == i & super_region_name == r & age == a & data_source != ""], 
                     aes(x = month, y = proportion, color = gender, fill = gender, shape = data_source, group = interaction(gender, age))) +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(aes(y = raked_pred, linetype = 'raked')) +
          geom_ribbon(aes(ymin = raked_lower, ymax = raked_upper), alpha = 0.3, colour = NA) +
          scale_color_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue')) +
          scale_fill_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue')) +
          facet_wrap(~ihme_loc_id) +
          labs(title = paste0(r, ": ", i),
               caption = 'January 2020 - September 2021 (Months 1-21)') +
          aesth
      }
    }else {
      gg[[paste0(i,r)]] <- ggplot(plot_data[indicator == i & super_region_name == r], 
                                  aes(x = month, y = proportion, color = gender, fill = gender, group = interaction(gender, age))) +
        geom_point(data = plot_data[indicator == i & super_region_name == r & data_source != ""], 
                   aes(x = month, y = proportion, color = gender, fill = gender, shape = data_source, group = interaction(gender, age))) +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        geom_line(aes(y = raked_pred, linetype = 'raked')) +
        geom_ribbon(aes(ymin = raked_lower, ymax = raked_upper), alpha = 0.3, colour = NA) +
        scale_color_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue', 'Both' = 'green3')) +
        scale_fill_manual(values = c('Women' = 'deeppink', 'Men' = 'dodgerblue', 'Both' = 'green3')) +
        facet_wrap(~ihme_loc_id) +
        labs(title = paste0(r, ": ", i),
             caption = 'January 2020 - September 2021 (Months 1-21)') +
        aesth
    }
  }
}

pdf(paste0(output_dir, 'FILEPATH.pdf'),14,8)
print(gg[1:length(gg)])
dev.off()

