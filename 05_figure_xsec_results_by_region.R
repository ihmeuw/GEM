#############################################################################################################
#
# Author: USERNAME
# Purpose: Create cross-sectional results summary by super-region and topic
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
                                        'Female learner', 'Age 45+', 'Age 25-45', 'Female child: Some tertiary education',
                                        'Age 35-64', 'Age 65+', 'Age 25-34', 'Age 35-44', 'Age 45+', 'Data Collection 2021',
                                        'Primary education', 'No education', 'Tertiary+ education', 'Employed full-time', 
                                        'Employed part-time', 'Lowest financial status', 'Higher financial status', 'Age 18-24',
                                        'Age 35-44', 'Capital city', 'Urban, non-capital city',
                                        'Data Collection 2021'))

variable_maps[, predictor:=factor(predictor, levels=rev(c('Female learner', 'Female', 'Age 1', 'Age 2', 'Age 18-24', 'Age 25-45', 'Age 25-34',  'Age 35-44', 'Age 45+', 'Age 35-64', 'Age 65+',
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
outcome_map[ind_name=='income_lost_combined', ind_name:='income_lost_combined_all']

# (2) Prep cross-sectional results -------------------------------------------------------------------------------------------------------------------
cross_dir <- 'FILEPATH'
r.srg <- fread(paste0(cross_dir, 'srg.csv'))
r.tg <- fread(paste0(cross_dir, 'tg.csv'))

indic_labs <- fread("FILEPATH")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

x_plt <- rbind(r.srg,r.tg,fill=T)
x_plt[is.na(super_region_name),super_region_name:="Total"]
x_plt <- merge(x_plt,indic_labs,by="indicator")
x_plt <- x_plt[main==1]
x_plt <- x_plt[!(indicator%in%c("income_lost_combined_all", "emp_lost_combined_all", "emp_lost_combined"))]
x_plt[,ilab:=factor(ilab,levels=c("Disruption in reproductive health","Income loss","Increase in chores","Increase in care for others","School drop out","Adequate remote learning","Perception of GBV Increase","Feeling unsafe at home" ))]

#factor super-region-name
x_plt[, super_region_name:=factor(super_region_name, levels=rev(c('Total', "Central Europe, Eastern Europe, and Central Asia",
                                                                  'High-income', 'Latin America and Caribbean',
                                                                  'North Africa and Middle East', 'South Asia', 'Southeast Asia, East Asia, and Oceania',
                                                                  'Sub-Saharan Africa')))]
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
#factor super-region-name
plt[, super_region_name:=factor(super_region_name, levels=rev(c('Total', "Central Europe, Eastern Europe, and Central Asia",
                                                                'High-income', 'Latin America and Caribbean',
                                                                'North Africa and Middle East', 'South Asia', 'Southeast Asia, East Asia, and Oceania',
                                                                'Sub-Saharan Africa')))]
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
alphas <- scale_alpha_manual(values=c('Total'=0.3,'High-income'=1, 'Latin America and Caribbean'=1,
                                    'North Africa and Middle East'=1, 'South Asia'=1, 'Southeast Asia, East Asia, and Oceania'=1,
                                    'Sub-Saharan Africa'=1, "Central Europe, Eastern Europe, and Central Asia"=1), drop=FALSE)

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

setnames(plt, c("raked_pred", "raked_lower", "raked_upper"), c('value_mean', 'value_lower', 'value_upper'))
x_plt <- rbind(x_plt, plt, fill=T)
x_plt <- x_plt[!(ilab=='Disruption in reproductive health' & super_region_name=='Total')]
x_plt[, sex:=ifelse(sex=='Male', 'Men', 'Women')]
x_plt[, sex:=factor(sex, levels=c('Men', 'Women'))]

gg_CrossList <- list()
for (ic in unique(x_plt$ind_category)){
  x_plt_tmp <- x_plt[ind_category==ic]
  print(ic)
  x_plt_tmp[, ilab:=factor(ilab, levels=unique(x_plt_tmp$ilab))]
  for (sr in unique(x_plt_tmp[super_region_name!='Total']$super_region_name)){
    print(sr)
    gg_cross <- ggplot(x_plt_tmp[(super_region_name==sr | super_region_name=='Total')],aes(y=ilab,x=value_mean*100,xmin=value_lower*100,xmax=value_upper*100,fill=super_region_name,color=super_region_name,shape=sex, alpha=super_region_name)) +
      geom_point(position=position_dodge(width=.5),color="black",stroke=1.1,size=4)+
      geom_errorbar(position=position_dodge(width=.5),size=1)+
      aesth +
      scale_shape_manual(name="Gender",values=c("Men"=21,"Women"=22)) +
      labs(x="Percent of Respondents", shape='Gender', color='World Region', fill='World Region', alpha='World Region')+
      theme(legend.position='none')+
      theme(axis.title.y=element_blank())+
      xlim(c(min(x_plt$value_lower*100), max(x_plt$value_upper*100)))+
      colors+
      fills+
      alphas+
      scale_y_discrete(labels = function(x) str_wrap(x, width = 15), drop=FALSE)
    if (sr != 'Central Europe, Eastern Europe, and Central Asia'){
      gg_cross <- gg_cross + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
    }
    if (ic=='Vaccine hesitancy and uptake'){
      gg_cross <- gg_cross + labs(title=str_wrap(paste0(sr),26))
    }
    if (ic != 'Safety at home and in the community'){
      gg_cross <- gg_cross+theme(axis.title.x=element_blank())
    }
    if (ic == 'Education' & sr=='Sub-Saharan Africa'){
      gg_cross <- gg_cross + guides(shape = guide_legend(ncol = 1, order=1, reverse=TRUE), alpha=guide_legend(ncol=1, order=2, reverse=TRUE), color=F, fill=F)+
                                    # color=guide_legend(ncol=1, order=3, reverse=TRUE), fill=F) +
        scale_alpha_manual(name = 'World Region',
                           values = c(1, 0.3),
                           breaks = c("Total"))+
        theme(legend.position = 'right') 
    }
    
    gg_CrossList[[paste0(ic, '_', sr)]] <- gg_cross
  }
}

#Arrange plots 
plot_grids <- list()
for (sr in levels(x_plt$super_region_name)){
  # title_tmp <- ggdraw() + draw_label(paste0(toupper(letter_map[ind_label==i]$letter), ') ', i), fontface='bold')
  tmp <- plot_grid(gg_CrossList[[paste0('Vaccine hesitancy and uptake_', sr)]], gg_CrossList[[paste0('Healthcare_', sr)]],
                   gg_CrossList[[paste0('Education_', sr)]], gg_CrossList[[paste0('Economic and work-related concerns_', sr)]],
                   gg_CrossList[[paste0('Safety at home and in the community_', sr)]],
                   align='v',
                   axis='rl',
                   ncol=1)
  plot_grids[[paste0(sr)]] <- tmp
}

# final plot 
row_titles <- ggdraw() + draw_label('5.Safety, home and community                 4. Economic and work-related                                3. Education                                      2. Healthcare                                           1. Vaccine hesitancy and uptake', fontface='plain', angle=90, size=14)
fig_title <- ggdraw() + draw_label('Supplemental Figure 14. Cross-sectional gender gaps, presented by world region and indicator.', fontface='plain', size=16) 

pdf(paste0(out_dir,"Cross_sectional_results_by_SR.pdf"),width=26,height=20)
plot_grid(fig_title,
          plot_grid(row_titles,
          plot_grids$`Central Europe, Eastern Europe, and Central Asia`,
          plot_grids$`High-income`,
          plot_grids$`Latin America and Caribbean`,
          plot_grids$`North Africa and Middle East`, 
          plot_grids$`South Asia`,
          plot_grids$`Southeast Asia, East Asia, and Oceania`,
          plot_grids$`Sub-Saharan Africa`,
          ncol=8,
          rel_widths=c(0.05,1.5,1,1,1,1,1,1.5)),
          align='h',
          axis='lr',
          ncol=1,
          rel_heights=c(0.05,1))
dev.off()

