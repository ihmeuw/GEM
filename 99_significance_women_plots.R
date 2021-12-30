##########################################################################################################################################################################
#
# Author: USER
# Date: June 30, 2021
# Purpose: Create a heatmap that shows whether or not the women predictor is significant
#
##########################################################################################################################################################################

rm(list=ls())

##### 0. SET UP ##########################################################################################################################################################

#libraries
pacman::p_load(data.table, dplyr, ggplot2, parallel, stringr, gridExtra, cowplot, lme4, boot, knitr, kableExtra, rmarkdown)

#functions
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

#fike paths
input_dir <- "FILEPATH"
output_dir <- "FILEPATH"

aesth <- theme_bw() + theme(axis.title = element_text(size=10,face='bold'),axis.text =element_text(size=10,face='bold'),plot.title =element_text(size=10,face='bold'),
                            strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')


##### 1. GET + FORMAT DATA ###############################################################################################################################################

#get codebook indicators
codebook <- fread("FILEPATH")
codebook[ind_name == 'getvax', ind_name := 'hesitancy']
indicators <- unique(codebook$ind_name)

#get figure data
dt <- fread('FILEPATH')
dt <- dt[predictor == 'Female' | predictor == 'Female learner']
dt[, c('N', 'n_countries', 'N_female', 'N_male') := NULL]
dt <- unique(dt)
dt$covs <- NULL

#tag significance
dt[OR_lwr > 1 & OR_upr > 1, sig := 'Higher Odds for Women']
dt[OR_lwr < 1 & OR_upr < 1, sig := 'Higher Odds for Men']
dt[is.na(sig), sig := 'Not Significant']

#fix
dt[ind_label == 'Disruption health product access', ind_label := "Disruption in health products access"]

#factor locations, indicators, and labels
dt[, super_region_name := factor(super_region_name, levels = c('Total', sort(unique(dt$super_region_name)[unique(dt$super_region_name) != 'Total'])))]
dt[, ind_label:=factor(ind_label, levels=c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                "Any disruption in health care", "Disruption in reproductive health", "Disruption in preventative care",
                                "Disruption in medication access","Disruption in health products access",
                                "Employment loss", "Income loss", "Not working to care for others", "Increase in care for others", "Increase in chores",
                                "School drop out" , "Adequate remote learning", 
                                "Perception of GBV Increase", "Feeling unsafe at home"))]
dt[, sig := factor(sig, levels = c('Higher Odds for Women', 'Not Significant', 'Higher Odds for Men', 'No Results Available'))]

#create unique combinations to identify missing values
all_combinations <- expand.grid(unique(dt$ind_label), unique(dt$super_region_name)) %>%
  merge(unique(dt$survey_name), all = TRUE) %>%
  setnames("Var1", 'ind_label') %>%
  setnames("Var2", 'super_region_name') %>%
  setnames('y', 'survey_name')

#get combinations that exist
existing_combinations <- as.data.table(unique(dt[, c('ind_label', 'super_region_name', 'survey_name')]))

#subset to what does NOT exist
missing_combinations <- anti_join(all_combinations, existing_combinations) %>%
  as.data.table()

#tag missing
missing_combinations[, predictor := 'Female']
missing_combinations[, sig := 'No Results Available']
missing_combinations <- merge(missing_combinations, codebook[, c('ind_label', 'ind_category')], by = 'ind_label')

#subset + merge missing and available data
dt_plot <- dt[, c('super_region_name', 'ind_label', 'ind_category', 'predictor', 'survey_name', 'sig')]
dt_plot <- rbind(dt_plot, missing_combinations)

##### 2. PLOT DATA #######################################################################################################################################################

for (i in unique(dt_plot$super_region_name)) {
  for (s in unique(dt_plot$survey_name)) {
    if (length(unique(dt_plot[super_region_name == i & survey_name == s]$sig)) == 1) {
      if (unique(dt_plot[super_region_name == i & survey_name == s]$sig) == 'No Results Available') {
        dt_plot[super_region_name == i & survey_name == s, drop := 1]
      }
    }
  }
}

dt_plot <- dt_plot[is.na(drop) | drop != 1]

dt_plot <- dt_plot[!(ind_label == 'Employment loss' & survey_name == 'RECOVR')] #drop RECOVR

dt_plot <- dt[, c('super_region_name', 'ind_label', 'ind_category', 'predictor', 'survey_name', 'sig')]
dt_plot <- rbind(dt_plot, missing_combinations)
for (i in unique(dt_plot$ind_label)) {
  for (s in unique(dt_plot$survey_name)) {
    if (length(unique(dt_plot[ind_label == i & survey_name == s]$sig)) == 1) {
      if (unique(dt_plot[ind_label == i & survey_name == s]$sig) == 'No Results Available') {
        dt_plot[ind_label == i & survey_name == s, drop := 1]
      }
    }
  }
}

dt_plot <- dt_plot[is.na(drop) | drop != 1]

dt_plot <- dt_plot[!(ind_label == 'Employment loss' & survey_name == 'RECOVR')] #drop RECOVR

gg2 <- ggplot(dt_plot[ind_label %in% c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                                 "Any disruption in health care","Disruption in preventative care",
                                                 "Disruption in medication access","Disruption in health products access", "Disruption in reproductive health")], aes(x = super_region_name, y = survey_name, fill = as.factor(sig), alpha = as.factor(sig))) +
  geom_tile(color='black') + 
  facet_wrap(~ind_label, ncol = 4) +
  scale_x_discrete(position = "top") +
  labs(y="", x="") +
  aesth + 
  theme(legend.position = "top",
        plot.background = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'= "gray50","No Results Available"="white")) + 
  scale_color_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'="gray50","No Results Available"="white")) +
  scale_alpha_manual(name="",values=c("Higher Odds for Women"=1,"Higher Odds for Men"=1,'Not Significant'=.3,"No Results Available"=1)) +
  theme(axis.text.y=element_text(size=8),axis.text.x = element_text(size=8,angle=0,hjust=0.5),legend.text = element_text(size=8)) +
  ggforce::facet_col(vars(ind_label), scales = 'free_y', space = 'free')

gg3 <- ggplot(dt_plot[ind_label %not in% c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                       "Any disruption in health care","Disruption in preventative care",
                                       "Disruption in medication access","Disruption in health products access", "Disruption in reproductive health")], aes(x = super_region_name, y = survey_name, fill = as.factor(sig), alpha = as.factor(sig))) +
  geom_tile(color='black') + 
  facet_wrap(~ind_label, ncol = 4) +
  scale_x_discrete(position = "top") +
  labs(y="", x="") +
  aesth + 
  theme(legend.position = "top",
        plot.background = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'= "gray50","No Results Available"="white")) + 
  scale_color_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'="gray50","No Results Available"="white")) +
  scale_alpha_manual(name="",values=c("Higher Odds for Women"=1,"Higher Odds for Men"=1,'Not Significant'=.3,"No Results Available"=1)) +
  theme(axis.text.y=element_text(size=8),axis.text.x = element_text(size=8,angle=0,hjust=0.5),legend.text = element_text(size=8)) +
  ggforce::facet_col(vars(ind_label), scales = 'free_y', space = 'free')

gg4 <- ggplot(dt_plot, aes(x = super_region_name, y = survey_name, fill = as.factor(sig), alpha = as.factor(sig))) +
  geom_tile(color='black') + 
  facet_wrap(~ind_label, ncol = 4) +
  scale_x_discrete(position = "top") +
  labs(y="", x="") +
  aesth + 
  theme(legend.position = "top",
        plot.background = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'= "gray50","No Results Available"="white")) + 
  scale_color_manual(name="",values=c("Higher Odds for Women"="#9e0142", "Higher Odds for Men"="#5e4fa2", 'Not Significant'="gray50","No Results Available"="white")) +
  scale_alpha_manual(name="",values=c("Higher Odds for Women"=1,"Higher Odds for Men"=1,'Not Significant'=.3,"No Results Available"=1)) +
  theme(axis.text.y=element_text(size=8),axis.text.x = element_text(size=8,angle=0,hjust=0.5),legend.text = element_text(size=8)) +
  ggforce::facet_col(vars(ind_label), scales = 'free_y', space = 'free')




pdf('FILEPATH', width = 12, height = 8)
print(gg2)
print(gg3)
dev.off()

#save jpgs
jpeg("FILEPATH", width = 10, height = 8, units = 'in', res = 1600)
print(gg2)
dev.off()

jpeg("FILEPATH", width = 10, height = 8, units = 'in', res = 1600)
print(gg3)
dev.off()


