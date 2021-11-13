#############################################################################################################
#
# Author: USERNAME
# Purpose: Create forest plots of regression results by time period for appendix
#
#############################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(survey)
library(openxlsx)
library(readstata13)
library(stringr)
library(lme4)
library(stargazer, lib.loc='FILEPATH')

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]
aesth <- theme_bw() + theme(axis.title = element_text(size=8,face='plain'),axis.text =element_text(size=10,face='bold'),plot.title =element_text(size=10,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')

#create one large file
read_draw <- function(c.file){
  print(c.file)
  fread(c.file) -> dat
  dat[, V1:=NULL]
  return(dat)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Read in super-region specific and aggregate result files ---------------------------------------------------

time_in <- 'FILEPATH'
main_in <- 'FILEPATH'
out_dir <- 'FILEPATH'
dir.create(out_dir)

#mapping
variable_maps <- data.table(variable=c('female', 'rural', 'higher_edu', 
                                       'child_female', 'age45more', 'age25to45', 'child_female:higher_edu',
                                       'age35to64', 'age65plus', 'age_dummy25-34', 'age_dummy35-44', 'age_dummy45+', 'date2021-05',
                                       'edu_dummyPrimary', 'edu_dummyNone', 'edu_dummyTertiary or higher', 'employmentfull_time',
                                       'employmentpart_time', 'financiallowest', 'financialhigher', 'age18-24', 'age35-44',
                                       'urban_binCapital city', 'urban_binNon-capital city', 
                                       'data_collection2021'),
                            predictor=c('Woman', 'Rural', 'Some tertiary education',
                                        'Female child', 'Age 45+', 'Age 25-45', 'Female child: Some tertiary education',
                                        'Age 35-64', 'Age 65+', 'Age 25-34', 'Age 35-44', 'Age 45+', 'Data Collection 2021',
                                        'Primary education', 'No education', 'Tertiary+ education', 'Employed full-time', 
                                        'Employed part-time', 'Lowest financial status', 'Higher financial status', 'Age 18-24',
                                        'Age 35-44', 'Capital city', 'Urban, non-capital city',
                                        'Data Collection 2021'))

variable_maps[, predictor:=factor(predictor, levels=rev(c('Woman', 'Age 1', 'Age 2', 'Age 18-24', 'Age 25-45', 'Age 25-34',  'Age 35-44', 'Age 45+', 'Age 35-64', 'Age 65+',
                                                          'No education', 'Primary education', 'Tertiary+ education',
                                                          'Some tertiary education', 'Rural', 'Capital city', 'Urban, non-capital city',
                                                          'Female child', 'Female child: Some tertiary education','Data Collection 2021',
                                                          'Employed part-time', 'Employed full-time',
                                                          'Lowest financial status', 'Higher financial status')))]

source_map <- data.table(source=c('Facebook Gender Equality at Home', 'Goalkeepers', 'UN Women RGA', 'FB Symptoms', 'FB symptoms', 'FinMark Trust',
                                  'WB HFPS', 'Yougov', 'IPA recovr trend', 'IPA recovr'),
                         survey_name=c('Survey on Gender Equality at Home', 
                                       'COVID-19 Health Services Disruption Survey', 'RGA',
                                       'UMD Global CTIS', 'UMD Global CTIS', 'FINMRK',
                                       'COVID-19 High Frequency Phone Survey', 'YouGov', 'RECOVR', 'RECOVR'))

outcome_map <- fread('FILEPATH')
outcome_map[ind_name=='getvax', ind_name:='hesitancy']

#create master file of super-region specific results
time1 <- fread(paste0(time_in, 'econhealth_minus_emp_lost_estimations_July2020_Nov2020_coefs_unweighted.csv'))[, time_period:='June2020-Nov2020']
time2 <- fread(paste0(time_in, 'econhealth_minus_emp_lost_estimations_Dec2020_Mar2021_coefs_unweighted.csv'))[, time_period:='Dec2020-Mar2021']
time3 <- fread(paste0(time_in, 'econhealth_minus_emp_lost_estimations_Apr2021+_coefs_unweighted.csv'))[, time_period:='Apr2021+']
time4 <- fread(paste0(time_in, 'emp_lost_combined_estimations_July2020_Nov2020_coefs_unweighted.csv'))[, time_period:='June2020-Nov2020']
time5 <- fread(paste0(time_in, 'emp_lost_combined_estimations_Dec2020_Mar2021_coefs_unweighted.csv'))[, time_period:='Dec2020-Mar2021']
time6 <- fread(paste0(time_in, 'emp_lost_combined_estimations_Apr2021+_coefs_unweighted.csv'))[, time_period:='Apr2021+']
time7 <- fread(paste0(time_in, 'vax_estimations_Jan2021_Mar2021_coefs_unweighted.csv'))[, time_period:='Jan2021-Mar2021']
time8 <- fread(paste0(time_in, 'vax_estimations_Apr2021+_coefs_unweighted.csv'))[, time_period:='Apr2021+']
time <- rbind(time1, time2, time3, time4, time5, time6, time7, time8)

#create one file and clean up
all <- time
all[, V1:=NULL]

#merge on indicator codebook
all <- merge(all, outcome_map, by.x='outcome', by.y='ind_name', all.x=T)
all <- all[variable!='(Intercept)']
all[, (c('OR', 'OR_95upr', 'OR_95lwr')):=lapply(.SD, function(x) as.numeric(x)), .SDcols=c('OR', 'OR_95upr', 'OR_95lwr')]
all[variable=='higher_educ', variable:='higher_edu']
all <- merge(all, variable_maps, by='variable', all.x=T)

#merge on official survey names
all <- merge(all, source_map, by.x='source_dat', by.y='source', all.x=T)

letter_map <- data.table(ind_label=c('Vaccine hesitancy', 'Fully vaccinated', 'Vaccine received',
                                     'Any disruption in health care', 
                                     'Disruption in preventative care', 'Disruption in medication access',
                                     'Disruption in health products access',
                                     'Employment loss', 'Not working to care for others'),
                         letter=c('A', 'B', 'C',
                                  'A', 'B', 'C', 'D',
                                  'A', 'B'))
#start building plots
plot_list <- list()

  for (i in unique(all$ind_label)){
    
    print(i)
    p <- ggplot(data=all[ind_label==i])+
      geom_pointrange(aes(y=predictor, x=OR, xmax=OR_95upr, xmin=OR_95lwr, color=time_period), alpha=0.75, position=position_dodge(0.7))+
      theme_bw()+
      # facet_wrap(~ind_label, ncol=1)+
      scale_x_log10()+
      scale_color_manual(values=c('May2021+'='black', 'Aug2021+'='black', 'Apr2021+'='black', 'Jan2021-Mar2021'='#DB72FB', 
                                  'Dec2020-Mar2021'='#DB72FB', 'June2020-Nov2020'='#00B9E3'))+
      geom_vline(xintercept=1, linetype='longdash')+
      labs(title=paste0(toupper(letter_map[ind_label==i]$letter), ') ', i),
           y='Model Covariates', x='OR (95% UI)',
           color='Time period')+
      scale_y_discrete(labels = function(x) str_wrap(x, width = 15), position='left')+
      aesth+
      theme(legend.position='none', axis.title.y=element_blank(), plot.title=element_text(face='bold'))
    
    if (!i %in% c('Employment loss', 'Any disruption in health care', 'Vaccine hesitancy')){
      p <- p + theme(axis.text.y=element_blank())
    }
    
    if (unique(all[ind_label==i]$ind_category)=='Healthcare'){
      p <- ggplot(data=all[ind_label==i])+
        geom_pointrange(aes(y=predictor, x=OR, xmax=OR_95upr, xmin=OR_95lwr, color=time_period), alpha=0.75, position=position_dodge(0.7))+
        theme_bw()+
        scale_x_log10()+
        scale_color_manual(values=c('May2021+'='black', 'Aug2021+'='black', 'Apr2021+'='black', 'Jan2021-Mar2021'='#DB72FB', 
                                    'Dec2020-Mar2021'='#DB72FB', 'June2020-Nov2020'='#00B9E3'))+
        geom_vline(xintercept=1, linetype='longdash')+
        labs(title=str_wrap(paste0(toupper(letter_map[ind_label==i]$letter), ') ', i),30),
             y='Model Covariates', x='OR (95% UI)',
             color='Time period')+
        scale_y_discrete(labels = function(x) str_wrap(x, width = 15), position='left')+
        aesth+
        theme(legend.position='none', axis.title.y=element_blank(), plot.title=element_text(face='bold'))
      
      if (!i %in% c('Employment loss', 'Any disruption in health care', 'Vaccine hesitancy')){
        p <- p + theme(axis.text.y=element_blank())
      }
    }
    plot_list[[paste0(i)]] <- p
    
  }

p <- ggplot(data=all[ind_label=='Vaccine hesitancy'])+
  geom_pointrange(aes(y=predictor, x=OR, xmax=OR_95upr, xmin=OR_95lwr, color=time_period), alpha=0.75, position=position_dodge(0.7))+
  theme_bw()+
  scale_x_log10()+
  scale_color_manual(values=c('May2021+'='black', 'Aug2021+'='black', 'Apr2021+'='black', 'Jan2021-Mar2021'='#DB72FB', 
                              'Dec2020-Mar2021'='#DB72FB', 'June2020-Nov2020'='#00B9E3'))+
  guides(color = guide_legend(ncol = 1, reverse=TRUE))+
  aesth+
  labs(color=str_wrap('Vaccination indicator time periods',30))+
  theme(legend.position='right', axis.title.y=element_blank(), plot.title=element_text(face='plain'), legend.title=element_text(size=10, face='bold'),
        legend.text=element_text(size=8))

lg1 <- get_legend(p)

p2 <- ggplot(data=all[ind_label=='Any disruption in health care'])+
  geom_pointrange(aes(y=predictor, x=OR, xmax=OR_95upr, xmin=OR_95lwr, color=time_period), alpha=0.75, position=position_dodge(0.7))+
  theme_bw()+
  scale_x_log10()+
  scale_color_manual(values=c('May2021+'='black', 'Aug2021+'='black', 'Apr2021+'='black', 'Jan2021-Mar2021'='#DB72FB', 
                              'Dec2020-Mar2021'='#DB72FB', 'June2020-Nov2020'='#00B9E3'))+
  guides(color = guide_legend(ncol = 1, reverse=TRUE))+
  aesth+
  labs(color=str_wrap('Healthcare and Economic indicator time periods',30))+
  theme(legend.position='right', axis.title.y=element_blank(), plot.title=element_text(face='bold'), legend.title=element_text(size=10, face='bold'),
        legend.text=element_text(size=8))

lg2 <- get_legend(p2)

row_titles <- ggdraw() + draw_label('1. Healthcare', fontface='bold', angle=90, size=10)
row_titles1 <- ggdraw() + draw_label('3. Vaccine hesitancy and uptake', fontface='bold', angle=90, size=10)
row_titles2 <- ggdraw() + draw_label('2. Economic and work-related concerns', fontface='bold', angle=90, size=10)

#vaccination
pdf(paste0(out_dir,"regressions_by_time_appendix_figure.pdf"),width=22,height=14)
plot_grid(row_titles,
          plot_list$`Any disruption in health care`, plot_list$`Disruption in preventative care`, 
          plot_list$`Disruption in medication access`, plot_list$`Disruption in health products access`,
          row_titles2,plot_list$`Employment loss`, plot_list$`Not working to care for others`, lg2, NA,
          row_titles1, plot_list$`Vaccine hesitancy`, plot_list$`Fully vaccinated`, plot_list$`Vaccine received`, lg1,
          ncol=5,
          rel_widths = c(0.05,1.45,1,1,1))
dev.off()

png(paste0(out_dir, 'fbsymptoms_regressions_by_time_appendix_figure.png'), res=1600, units='in', width=10, height=10)
plot_grid(row_titles,
          plot_list$`Any disruption in health care`, plot_list$`Disruption in preventative care`, 
          plot_list$`Disruption in medication access`, plot_list$`Disruption in health products access`,
          row_titles2,plot_list$`Employment loss`, plot_list$`Not working to care for others`, lg2, NA,
          row_titles1, plot_list$`Vaccine hesitancy`, plot_list$`Fully vaccinated`, plot_list$`Vaccine received`, lg1,
          ncol=5,
          rel_widths = c(0.05,1.45,1,1,1))
dev.off()