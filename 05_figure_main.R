#############################################################################################################
#
# Author: USERNAME
# Purpose: Create main paper figures by topic
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
library(gridExtra)
library(grid)
library(cowplot)

invisible(sapply(list.files("FILEPATH", full.names = T), source))
locs <- get_location_metadata(location_set_id = 115, location_set_version_id = 746,release_id=9)[,c("location_id","ihme_loc_id","location_name","super_region_name")]

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

# (1) Prep regression data --------------------------------------------------------------------------------------

#dirs
sr_in <- 'FILEPATH'
appendix_in <- 'FILEPATH'
main_in <- 'FILEPATH'
out_dir <- 'FILEPATH'
out_dir <- paste0(out_dir, gsub('-', '_', Sys.Date()), '/')
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
                                        'Woman or girl learner', 'Age 45+', 'Age 25-45', 'Female child: Some tertiary education',
                                        'Age 35-64', 'Age 65+', 'Age 25-34', 'Age 35-44', 'Age 45+', 'Data Collection 2021',
                                        'Primary education', 'No education', 'Tertiary+ education', 'Employed full-time', 
                                        'Employed part-time', 'Lowest financial status', 'Higher financial status', 'Age 18-24',
                                        'Age 35-44', 'Capital city', 'Urban, non-capital city',
                                        'Data Collection 2021'))

variable_maps[, predictor:=factor(predictor, levels=rev(c('Woman or girl learner', 'Woman', 'Age 1', 'Age 2', 'Age 18-24', 'Age 25-45', 'Age 25-34',  'Age 35-44', 'Age 45+', 'Age 35-64', 'Age 65+',
                                                          'No education', 'Primary education', 'Tertiary+ education',
                                                          'Some tertiary education', 'Rural', 'Capital city', 'Urban, non-capital city',
                                                          'Female child: Some tertiary education','Data Collection 2021',
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
sr_results <- list.files(paste0(sr_in), full.names = T, recursive=T)
sr_results <- sr_results[sr_results %like% '.csv']
sr_results <- sr_results[!sr_results %like% 'archive']
sr_results %>% lapply(read_draw) %>% rbindlist(fill=T) -> srs
srs[is.na(super_region_name), super_region_name:=super_region]
srs[super_region=='All', super_region_name:='Total']
srs <- srs[!source_dat %in% c("IPA recovr no trend'")]

#pull in global appendix and main reg results
main_results <- list.files(paste0(main_in), full.names=T, recursive=F)
main_results <- main_results[main_results %like% '.csv']
global <- main_results
global %>% lapply(read_draw) %>% rbindlist(fill=T) -> globals 
globals[, super_region_name:='Total']
globals[is.na(source), source:=source_dat]

#create one file and clean up
all <- rbind(globals, srs, fill=T)
all[source_dat=='FB GE', source:='Facebook Gender Equality at Home']
all[source_dat=='UN women', source:='UN Women RGA']
all[source_dat=='Finmark', source:='FinMark Trust']
all[is.na(source), source:=source_dat]
all[is.na(OR_lwr), OR_lwr:=OR_95lwr]
all[is.na(OR_upr), OR_upr:=OR_95upr]
all <- all[source!='IPA recovr no trend']

#merge on indicator codebook
all <- merge(all, outcome_map, by.x='outcome', by.y='ind_name', all.x=T)
all <- all[variable!='(Intercept)']
all[, (c('OR', 'OR_upr', 'OR_lwr')):=lapply(.SD, function(x) as.numeric(x)), .SDcols=c('OR', 'OR_upr', 'OR_lwr')]
all[variable=='higher_educ', variable:='higher_edu']
all <- merge(all, variable_maps, by='variable', all.x=T)

#merge on official survey names
all <- merge(all, source_map, by='source', all.x=T)

#Take out non-main manuscript
all <- all[main==1]

#edit ages to 1 and 2 categories
all[predictor=='Age 25-45', predictor:='Age 35-64']
all[predictor=='Age 45+', predictor:='Age 65+']

#take out time period predictors for now
all <- all[!is.na(predictor)]
all <- all[predictor!='Data Collection 2021']

#factor super-region-name in alphabetical order
all[, super_region_name:=factor(super_region_name, levels=rev(c('Total', "Central Europe, Eastern Europe, and Central Asia",
                                                            'High-income', 'Latin America and Caribbean',
                                                            'North Africa and Middle East', 'South Asia', 'Southeast Asia, East Asia, and Oceania',
                                                            'Sub-Saharan Africa')))]

#set order of indicators
setorder(all, ind_category)
all[ind_label=="Disruption in health products access",ind_label:="Disruption health product access"]
outcome_map[ind_label=="Disruption in health products access",ind_label:="Disruption health product access"]
all[, ind_label:=factor(ind_label, levels=unique(all$ind_label))]

#incorporate other formatting
all[source=='Goalkeepers' & ind_category=='Education' & predictor=='Woman', predictor:='Woman respondent']
all[survey_name=='UMD Global CTIS' & outcome %in% c('hesitancy', 'fullvax'), survey_name:='UMD Global & Delphi US CTIS']
all[, predictor:=factor(predictor, levels=rev(c('Woman or girl learner', 'Woman respondent', 'Woman', 'Age 1', 'Age 2', 'Age 18-24', 'Age 25-45', 'Age 25-34',  'Age 35-44', 'Age 45+', 'Age 35-64', 'Age 65+',
                                          'Some tertiary education', 'Rural')))]

#change super-region to countries actually represented in total for certain aggregate results
all[source=='FinMark Trust', super_region_name:='Sub-Saharan Africa']
all[source=='UN Women RGA' & outcome %in% c('srhc_covid_among_need', 'healthcare_covid'), super_region_name:='Sub-Saharan Africa']
all[source=='Yougov', super_region_name:='High-income']

#exclude fb ssa
all <- all[!(source=='FB Symptoms' & super_region_name=='Sub-Saharan Africa' & outcome=='preventative_health_covid')]
all <- all[!(source=='FB Symptoms' & super_region_name=='Sub-Saharan Africa' & outcome=='healthcare_covid')]
number_plugging <- all[, .(N, covs, n_countries, OR, OR_lwr, OR_upr, N_female, N_male, super_region_name, ind_label, ind_category, predictor, survey_name)]
write.csv(number_plugging, paste0(out_dir, 'number_plugging_ORs.csv'), row.names=F)


# (2) Prep cross-sectional results -------------------------------------------------------------------------------------------------------------------
cross_dir <- 'FILEPATH'
r.srg <- fread(paste0(cross_dir, 'srg.csv'))
r.tg <- fread(paste0(cross_dir, 'tg.csv'))

indic_labs <- fread("/FILEPATH/indicators_codebook.csv")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

x_plt <- rbind(r.srg,r.tg,fill=T)
x_plt[is.na(super_region_name),super_region_name:="Total"]
x_plt <- x_plt[!(indicator%in%c("income_lost_combined_all", "emp_lost_combined_all"))]
x_plt <- merge(x_plt,indic_labs,by="indicator")
x_plt <- x_plt[main==1]

x_plt[,ilab:=factor(ilab,levels=c("Disruption in reproductive health","Income loss","Increase in chores","Increase in care for others","School drop out","Adequate remote learning","Perception of GBV Increase","Feeling unsafe at home" ))]

#factor super-region-name
x_plt[, super_region_name:=factor(super_region_name, levels=rev(c('Total', "Central Europe, Eastern Europe, and Central Asia",
                                                            'High-income', 'Latin America and Caribbean',
                                                            'North Africa and Middle East', 'South Asia', 'Southeast Asia, East Asia, and Oceania',
                                                            'Sub-Saharan Africa')))]
x_plt[, sex:=ifelse(sex=='Male', 'Men', 'Women')]
x_plt[, sex:=factor(sex, levels=c('Men', 'Women'))]

# (3) Prep time-series results -----------------------------------------------------------------------------------------------------------------------
ts_dir <- 'FILEPATH'
r.srgm.gaps <- fread(paste0(ts_dir, 'srgm.csv'))
r.tgm.gaps <- fread(paste0(ts_dir, 'tgm.csv'))

plt <- rbind(r.srgm.gaps,r.tgm.gaps,fill=T)
plt[is.na(super_region_name),super_region_name:="Total"]
plt <- plt[month%in%c(21)] #changed 18 to 21 because we want September

indic_labs <- fread("FILEPATH")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

plt[indicator=="hesitancy",indicator:="getvax"]
plt[indicator=="vaccinated",indicator:="V1"]
plt[indicator=="fully_vaccinated",indicator:="fullvax"]
plt <- plt[ilab!='Vaccine received']

plt <- merge(plt,indic_labs,by="indicator",all.x=T)
plt <- plt[main==1]

plt[ilab=="Disruption in health products access",ilab:="Disruption health product access"]
plt[,ilab:=factor(ilab,levels=c("Vaccine hesitancy" ,"Fully vaccinated",
                                "Employment loss","Not working to care for others",
                                "Any disruption in health care", "Disruption in medication access",
                                "Disruption health product access","Disruption in preventative care"))]

plt[, sex:=ifelse(sex=='Male', 'Men', 'Women')]
plt[, sex:=factor(sex, levels=c('Men', 'Women'))]

plt2 <- fread(paste0(ts_dir, 'tgm.csv')) 
plt2[indicator=="hesitancy",indicator:="getvax"]
plt2[indicator=="vaccinated",indicator:="V1"]
plt2[indicator=="fully_vaccinated",indicator:="fullvax"]
plt2 <- merge(plt2,indic_labs,by="indicator",all.x=T)
plt2 <- plt2[main==1]
plt2[ilab=="Disruption in health products access",ilab:="Disruption health product access"]
plt2[,ilab:=factor(ilab,levels=c("Vaccine hesitancy","Vaccine received","Fully vaccinated",
                                 "Employment loss","Not working to care for others",
                                 "Any disruption in health care", "Disruption in medication access","Disruption health product access","Disruption in preventative care"))]
#factor super-region-name
plt[, super_region_name:=factor(super_region_name, levels=rev(c('Total', "Central Europe, Eastern Europe, and Central Asia",
                                                            'High-income', 'Latin America and Caribbean',
                                                            'North Africa and Middle East', 'South Asia', 'Southeast Asia, East Asia, and Oceania',
                                                            'Sub-Saharan Africa')))]

plt2[, sex:=ifelse(sex=='Male', 'Men', 'Women')]
plt2[, sex:=factor(sex, levels=c('Men', 'Women'))]


# (4) Set up for all plots ------------------------------------------------------------------------------------------------------------------------------------
aesth <- theme_bw() + theme(axis.text.x = element_text(size=14,face="bold",angle=0,hjust=1),
                            axis.text.y = element_text(size=14,face="bold"),
                            strip.text = element_text(size=14,face="bold"),
                            plot.background = element_blank(),
                            plot.title =element_text(size=14,face='bold'),
                            legend.text=element_text(size=14),
                            legend.title=element_text(size=14, face='bold'))

colors <- scale_color_manual(values=c('Total'='black','High-income'='#D39200', 'Latin America and Caribbean'='#93AA00',
                                      'North Africa and Middle East'='619CFF', 'South Asia'='#00C19F', 'Southeast Asia, East Asia, and Oceania'='#00B9E3',
                                      'Sub-Saharan Africa'='#DB72FB', "Central Europe, Eastern Europe, and Central Asia"='#F8766D'), drop=FALSE)
fills <- scale_fill_manual(values=c('Total'='black','High-income'='#D39200', 'Latin America and Caribbean'='#93AA00',
                                    'North Africa and Middle East'='619CFF', 'South Asia'='#00C19F', 'Southeast Asia, East Asia, and Oceania'='#00B9E3',
                                    'Sub-Saharan Africa'='#DB72FB', "Central Europe, Eastern Europe, and Central Asia"='#F8766D'), drop=FALSE)

letter_map <- data.table(ind_label=c('Vaccine hesitancy', 'Fully vaccinated',
                                     'Any disruption in health care', 'Disruption in reproductive health',
                                     'Disruption in preventative care', 'Disruption in medication access',
                                     'Disruption health product access',
                                     'Employment loss', 'Income loss', 'Not working to care for others',
                                     'Increase in care for others', 'Increase in chores',
                                     'School drop out', 'Adequate remote learning',
                                     'Perception of GBV Increase', 'Feeling unsafe at home'),
                         letter=c('A', 'B', 
                                  'A', 'B', 'C', 'D', 'E',
                                  'A', 'B', 'E', 'C', 'D',
                                  'A', 'B',
                                  'A', 'B'))

legends <- c('Fully vaccinated', 'Disruption health product access',
             'Not working to care for others', 'Adequate remote learning', 'Feeling unsafe at home')
axes <- c('Vaccine hesitancy', 'Any disruption in health care', 'Employment loss', 'School drop out', 'Perception of GBV Increase')

# (5) set up CS plots -------------------------------------------------------------------------------------------------------------------------------------------

gg_CrossList <- list()
for (ic in unique(x_plt$ind_category)){
  x_plt_tmp <- x_plt[ind_category==ic]
  print(ic)
  
  if (ic %in% c("Healthcare", "Economic and work-related concerns")){
    min_val <- min(x_plt[ind_category==ic]$value_lower*100, plt[ind_category==ic]$raked_lower*100)
    max_val <- max(x_plt[ind_category==ic]$value_upper*100, plt[ind_category==ic]$raked_upper*100)
  } else {
    min_val <- min(x_plt[ind_category==ic]$value_lower*100)
    max_val <- 80
  }
  
  for (i in unique(x_plt_tmp[!is.na(ilab)]$ilab)){
    gg_cross <- ggplot(x_plt_tmp[ilab == i],aes(y=super_region_name,x=value_mean*100,xmin=value_lower*100,xmax=value_upper*100,fill=super_region_name,color=super_region_name,shape=sex)) +
      geom_point(position=position_dodge(width=.05),color="black",stroke=1.1,size=4)+
      geom_errorbar(position=position_dodge(width=.2),size=1,alpha=.3)+
      aesth +
      scale_shape_manual(name="Gender",values=c("Men"=21,"Women"=22)) +
      geom_hline(yintercept=7.5,linetype='longdash',alpha=.7)+
      guides(color=F,fill=F)+
      labs(x="Percent of Respondents")+
      theme(legend.position='none')+
      guides(shape=guide_legend(ncol=1, reverse=TRUE))+
      scale_y_discrete(limits=c(levels(x_plt$super_region_name)))+
      scale_x_continuous(limits=c(min_val, max_val))+
      fills+
      colors+
      theme(axis.title.y=element_blank())
    if (!i %in% axes){
      gg_cross <- gg_cross + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
    } 
    if (i %in% legends){
      gg_cross <- gg_cross + theme(legend.position = 'right')
    }
    gg_CrossList[[paste0(i)]] <- gg_cross
  }
}

# (6) set up TS plots -------------------------------------------------------------------------------------------------------------------------------------------

gg_SepList <- list()
gg_TrendsList <- list()

for (ic in unique(plt$ind_category)){
  plt_tmp <- plt[ind_category==ic]
  plt2_tmp <- plt2[ind_category==ic] 
  
  if (ic %in% c("Healthcare", "Economic and work-related concerns")){
    min_val <- min(x_plt[ind_category==ic]$value_lower*100, plt[ind_category==ic]$raked_lower*100)
    max_val <- max(x_plt[ind_category==ic]$value_upper*100, plt[ind_category==ic]$raked_upper*100)
  } else {
    min_val <- min(plt[ind_category==ic]$raked_lower*100)
    max_val <- max(plt[ind_category==ic]$raked_upper*100)
  }
  
  if (ic %in% c("Healthcare")){
    ts_max_val <- 0.25
  } else {
    ts_max_val <- max(plt2_tmp$raked_upper)
  }
  
  
  for (i in unique(plt_tmp[!is.na(ilab)]$ilab)){
    gg_septmp <- ggplot(plt_tmp[month==21 & ilab == i],aes(y=super_region_name,x=raked_pred*100,xmin=raked_lower*100,xmax=raked_upper*100,
                                                       fill=super_region_name,color=super_region_name,shape=sex)) +
      geom_point(position=position_dodge(width=.05),color="black",stroke=1.1,size=4)+
      geom_errorbar(position=position_dodge(width=.2),size=1,alpha=.3)+
      aesth +
      scale_shape_manual(name="Gender",values=c("Men"=21,"Women"=22)) +
      geom_hline(yintercept=7.5,linetype='longdash',alpha=.7)+
      guides(color=F,fill=F)+
      labs(x="Percent of Respondents")+
      theme(legend.position='none')+
      guides(shape=guide_legend(ncol=1, reverse=TRUE))+
      scale_y_discrete(limits=c(levels(plt$super_region_name)))+
      scale_x_continuous(limits=c(min_val, max_val))+
      fills+
      colors+
      theme(axis.title.y=element_blank())
    if (!i %in% axes){
      gg_septmp <- gg_septmp + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
    } 
    if (i %in% legends){
      gg_septmp <- gg_septmp + theme(legend.position = 'right')
    }
    gg_SepList[[paste0(i)]] <- gg_septmp
    
    #time trend
    gg_trend_tmp <- ggplot(plt2_tmp[ilab==i], aes(x = month, y = raked_pred,ymin=raked_lower,ymax=raked_upper, color = sex, fill = sex)) +
      geom_line() +
      geom_point(color="black",shape=21)+ 
      geom_ribbon(alpha = 0.3, colour = NA) +
      scale_color_manual(values = c('Women' = '#a50026', 'Men' = '#313695')) +
      scale_fill_manual(values = c('Women' = '#a50026', 'Men' = '#313695')) +
      guides(color=guide_legend(reverse=TRUE), fill=guide_legend(reverse=TRUE))+
      labs(y="Percent of Respondents",x="", shape='Gender', color='Gender', fill='Gender')+
      aesth + 
      scale_x_continuous(breaks=seq(3,21,3),labels=paste0(format(seq(as.Date("2020/3/1"), by = "3 months", length.out = 7),"%b"), '\n', format(seq(as.Date("2020/3/1"), by = "3 months", length.out = 7),"%Y"))
                         , limits=c(3,21))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),position = "left",
                         limits=c(min(plt2_tmp$raked_lower), ts_max_val)) + 
      theme(axis.text.x = element_text(size=14,face="bold",angle=45,hjust=1),
            legend.position='none',axis.text.y = element_text(size=14,face="bold"),
            strip.text = element_text(size=14,face="bold"))
    
    if (!i %in% axes){
      gg_trend_tmp <- gg_trend_tmp + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
    }
    if (i %in% legends){
      gg_trend_tmp <- gg_trend_tmp + theme(legend.position='right')
    }
    
    gg_TrendsList[[paste0(i)]] <- gg_trend_tmp
  }
}


# (7) Regressions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
all[, survey_name:=as.factor(survey_name)]

#to make all predictors appear on plot, filler data for srhc disruption
row <- data.table(survey_name='RGA', predictor='Age 65+', super_region_name='Sub-Saharan Africa', 
                  ind_category='Healthcare', OR=1000, OR_lwr=1000, OR_upr=1000, ind_label='Disruption in reproductive health')
row2 <- data.table(survey_name='RGA', predictor='Rural', super_region_name='Sub-Saharan Africa',
                   ind_category='Economic and work-related concerns', OR=1000, OR_lwr=1000, OR_upr=1000, ind_label='Income loss')
all <- rbind(all, row, row2, fill=T)

gg_RegList <- list()
for (i in levels(all$ind_label)){
  c <- outcome_map[ind_label==i]$ind_category
  all_sub <- all[ind_category==c]
  all_sub[, survey_name:=factor(survey_name, levels=unique(all_sub$survey_name))]
  gg_reg <- ggplot() +
    geom_pointrange(data=all_sub[ind_label==i & super_region_name!='Total'],
                    aes(x=OR,xmax=OR_upr,xmin=OR_lwr,y=predictor,color=super_region_name,shape=survey_name), alpha=0.8, position=position_dodge(0.5))+
    geom_pointrange(data=all_sub[ind_label==i & super_region_name=='Total'],
                    aes(x=OR,xmax=OR_upr,xmin=OR_lwr,y=predictor,color=super_region_name,shape=survey_name), alpha=1, size=1.1, position=position_dodge(0.3))+
    geom_vline(xintercept=1,linetype='longdash') +
    scale_x_log10(limits=c(min(all_sub[ind_label!='Disruption in reproductive health' & ind_label != 'Income loss']$OR_lwr), max(all_sub[ind_label!='Disruption in reproductive health' & ind_label != 'Income loss']$OR_upr)))+
    aesth +
    colors +
    theme(legend.position = 'none')+
    labs(x="OR (95%UI)",color='World Region', shape='Survey')+
    scale_shape_manual(values=c("UMD Global & Delphi US CTIS"=16, "UMD Global CTIS"=16, "COVID-19 Health Services Disruption Survey"=15,
                                "COVID-19 High Frequency Phone Survey"=7,
                                "YouGov"=11,
                                "Survey on Gender Equality at Home"=18,
                                "RECOVR"=8,
                                "FINMRK"=14,
                                "RGA"=25), drop=FALSE)+
    scale_y_discrete(labels = function(x) str_wrap(x, width = 15), position='left')+
    guides(color = guide_legend(ncol = 1, order=1, reverse=TRUE), shape=guide_legend(ncol=1, order=2))+
    theme(axis.title.y=element_blank())
  
  if (!i %in% axes){
    gg_reg <- gg_reg + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
  }
  if (i %in% legends){
    gg_reg <- gg_reg  + theme(legend.position='right')
  }
  
  gg_RegList[[paste0(i)]] <- gg_reg
}

# (7) Arrange plots! ------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Cross-sectional 
#title object

plot_grids_cs <- list()
for (i in levels(x_plt$ilab)){
  title_tmp <- ggdraw() + draw_label(paste0(toupper(letter_map[ind_label==i]$letter), ') ', i), fontface='bold')
  tmp <- plot_grid(title_tmp, gg_CrossList[[paste0(i)]], gg_RegList[[paste0(i)]], 
                   align='v',
                   axis='rl',
                   ncol=1,
                   rel_heights=c(0.1,1,1))
  plot_grids_cs[[paste0(i)]] <- tmp
}

## Cross-sectional to be combined with TS
plot_grids_cs2 <- list()
for (i in levels(x_plt$ilab)){
  title_tmp <- ggdraw() + draw_label(paste0(toupper(letter_map[ind_label==i]$letter), ') ', i), fontface='bold')
  tmp <- plot_grid(title_tmp, NA, gg_CrossList[[paste0(i)]], gg_RegList[[paste0(i)]], 
                   align='v',
                   axis='rl',
                   ncol=1,
                   rel_heights=c(0.1,1,1,1))
  plot_grids_cs2[[paste0(i)]] <- tmp
}

## Time series 
plot_grids_ts <- list()
for (i in unique(plt$ilab)){
  title_tmp <- ggdraw() + draw_label(paste0(toupper(letter_map[ind_label==i]$letter), ') ', i), fontface='bold')
  tmp <- plot_grid(title_tmp, gg_TrendsList[[paste0(i)]], gg_SepList[[paste0(i)]], gg_RegList[[paste0(i)]],  
                   align='v',
                   axis='rl',
                   ncol=1,
                   rel_heights=c(0.1,1,1,1))
  plot_grids_ts[[paste0(i)]] <- tmp
}

#row labels
row_titles <- ggdraw() + draw_label('3. Multivariate regressions                                       2. Cross-sectional gender gaps                                       1. Time series analysis', fontface='bold', angle=90, size=14)
row_titles_cs <- ggdraw() + draw_label('2. Multivariate regressions                                       1. Cross-sectional gender gaps', fontface='bold', angle=90, size=14)

#fig titles
vax_fig_title <- ggdraw() + draw_label('Figure 2. Time series, September 2021 cross-sectional, and multivariate logistic regression analyses results for vaccination hesitancy and uptake indicators.', fontface='plain', size=14)
hc_fig_title <- ggdraw() + draw_label('Figure 3. Time series analysis, cross-sectional analyses, and multivariate logistic regression analysis results for healthcare access indicators.', fontface='plain', size=14)
edu_fig_title <- ggdraw() + draw_label('Figure 4. Cross-sectional and multivariate logistic regression analyses results for education indicators.', fontface='plain', size=14)
econ_fig_title <- ggdraw() + draw_label('Figure 5. Time series, cross-sectional, and multivariate logistic regression analyses results for economic and work-related concerns indicators.', fontface='plain', size=14)
safety_fig_title <- ggdraw() + draw_label('Figure 6. Cross-sectional and multivariate logistic regression analyses results for safety at home and in the community indicators.', fontface='plain', size=14)

#vaccination
pdf(paste0(out_dir,"Fig2_vax.pdf"),width=20,height=14)
plot_grid(vax_fig_title,
          plot_grid(row_titles,
          plot_grids_ts$`Vaccine hesitancy`,
          plot_grids_ts$`Fully vaccinated`,
          ncol=3,
          rel_widths=c(0.05,1,1.05)),
          ncol=1,
          rel_heights=c(0.05, 1),
          hjust='0')
dev.off()

#education
pdf(paste0(out_dir,"Fig4_educ.pdf"),width=20,height=9.3338)
plot_grid(edu_fig_title,
          plot_grid(row_titles_cs,
          plot_grids_cs$`School drop out`,
          plot_grids_cs$`Adequate remote learning`,
          ncol=3,
          rel_widths=c(0.05,1,1.05)),
          ncol=1,
          rel_heights=c(0.05, 1),
          hjust='0')
dev.off()

#safety at home
pdf(paste0(out_dir,"Fig6_safety.pdf"),width=20,height=9.3338)
plot_grid(safety_fig_title,
          plot_grid(row_titles_cs,
          plot_grids_cs$`Perception of GBV Increase`,
          plot_grids_cs$`Feeling unsafe at home`,
          ncol=3,
          rel_widths=c(0.05,1,1.05)),
          ncol=1,
          rel_heights=c(0.05, 1),
          hjust='0')
dev.off()

#Econ
pdf(paste0(out_dir,"Fig5_econ.pdf"),width=30,height=14)
plot_grid(econ_fig_title,
          plot_grid(row_titles,
          plot_grids_ts$`Employment loss`,
          plot_grids_cs2$`Income loss`,
          plot_grids_cs2$`Increase in care for others`,
          plot_grids_cs2$`Increase in chores`,
          plot_grids_ts$`Not working to care for others`,
          ncol=6,
          rel_widths=c(0.05,2.3,1,1,1,2.3)),
          ncol=1,
          rel_heights=c(0.05, 1),
          hjust='0')
dev.off()



#health
pdf(paste0(out_dir,"Fig3_health.pdf"),width=30,height=14)
plot_grid(hc_fig_title,
plot_grid(row_titles,
          plot_grids_ts$`Any disruption in health care`,
          plot_grids_cs2$`Disruption in reproductive health`,
          plot_grids_ts$`Disruption in preventative care`,
          plot_grids_ts$`Disruption in medication access`,
          plot_grids_ts$`Disruption health product access`,
          ncol=6,
          rel_widths=c(0.05,2.3,1,1,1,2.3)),
ncol=1,
rel_heights=c(0.05, 1),
hjust='0')
dev.off()
