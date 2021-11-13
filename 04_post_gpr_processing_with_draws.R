#################################################################################################################################################################################################################################
# Purpose: Process + clean + plot GPR results
#################################################################################################################################################################################################################################

rm(list=ls())

output_dir <- paste0('/FILEPATH/')

##### 0. SET UP #################################################################################################################################################################################################################

#load general libraries
pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4,boot,withr)

#install + load ggplus, which lets us split ggplots across multiple pages
library(devtools)
with_libpaths(new = "/FILEPATH/", code = devtools::install_github("guiastrennec/ggplus"))
library(ggplus, lib.loc = '/FILEPATH/') 
library(ggforce)

#location metadata              
source(file.path("/FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)

#not functions
"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

#plotting aesthetic
aesth <- theme_bw() + theme(axis.title = element_text(size=8,face='bold'),axis.text =element_text(size=8,face='bold'),plot.title =element_text(size=12,face='bold'),
                            strip.background = element_rect(fill="white"),strip.text=element_text(size=8,face='bold'),legend.position = 'top')


##### 1. GET + FORMAT OUTPUT DRAWS DATA #########################################################################################################################################################################################

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

#long on draw
gpr_final <- melt.data.table(gpr_final,id.vars=names(gpr_final)[!names(gpr_final)%like%"draw"],variable.name='draw',value.name = "gpr_draw")

#logit transform + multiple for offsets
gpr_final[, gpr_draw := inv.logit(gpr_draw)*0.95]


##### 2. RAKING #################################################################################################################################################################################################################

#get covid population data
pop_full <- fread("/FILEPATH/")
adults <- c(66, 67, 9:19,20,30:32,235) #18 and older
adult_pop <- pop_full[age_group_id %in% adults, lapply(.SD, function(x) sum(x)), by=c("location_id", "sex_id"), .SDcols = "population"]
adult_pop <- merge(adult_pop, hierarchy[, .(location_id, ihme_loc_id)], by = c('location_id'))

#merge population data
gpr_final <- merge(gpr_final, adult_pop, by=c("ihme_loc_id", "sex_id"), all.x = T)

#split into both and male+female
rake <- gpr_final[indicator %like% 'vaccinated' & sex_id == 3]
rake_sex <- gpr_final[indicator %like% 'vaccinated' & sex_id != 3]

#rake male+female
rake_sex[, wt_mean_sex := weighted.mean(gpr_draw, w=population), by=.(month, ihme_loc_id, indicator,draw)]

#merge
scalars <- merge(rake_sex[, c('month', 'ihme_loc_id', 'sex_id', 'indicator', 'wt_mean_sex','draw')],
                 rake[, c('month', 'ihme_loc_id', 'indicator', 'gpr_draw','draw')],
                 by = c('month', 'ihme_loc_id', 'indicator','draw'))

#caclulate scalar/ratio
scalars[, scale := gpr_draw/wt_mean_sex] #scale is same for males and females in same location-month-indicator
scalars <- as.data.table(unique(scalars[, c('ihme_loc_id', 'month', 'indicator', 'scale','draw')]))

#merge onto data
gpr_final <- merge(gpr_final, scalars, by = c('month', 'ihme_loc_id', 'indicator','draw'), all = T)

gpr_final[!is.na(scale) & sex != 'Both', gpr_draw := gpr_draw*scale]

gpr_final[gpr_draw >= 1, gpr_draw := 1]
gpr_final[gpr_draw <= 0, gpr_draw := 0]


##### 3. AGGREGATE RESULTS BY LOCATION ##########################################################################################################################################################################################

#drop timeseries that have 0 input data points
gpr_final[,input:=0]
gpr_final[!is.na(proportion),input:=1]
gpr_final[,num_inputs:=sum(input),by=.(indicator,ihme_loc_id,sex)]
gpr_final <- gpr_final[num_inputs>0]

#start vaccine timeseries in Jan 
gpr_final <- gpr_final[!(month<12&indicator%in%c("fully_vaccinated","vaccinated","hesitancy"))]
gpr_final <- gpr_final[!(indicator%in%c("emp_2_combined"))]

#truncate at month 22
gpr_final <- gpr_final[month<22&month>2]

#drop aggregate sex data
gpr_final <- gpr_final[sex != 'Both']

#country level
r.cgmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                  by = .(ihme_loc_id,super_region_name, sex, month, indicator,draw)]

r.cgm <-  r.cgmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                     by = .(ihme_loc_id,super_region_name, sex, month, indicator)]

r.cmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                     by = .(ihme_loc_id,super_region_name, month, indicator,draw)]

r.cm <-  r.cmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                 by = .(ihme_loc_id,super_region_name, month, indicator)]

r.cgm.gaps <- dcast.data.table(r.cgmd,ihme_loc_id+super_region_name+month+indicator+draw~sex)
r.cgm.gaps[,gap_abs:=(Female-Male)*100]
r.cgm.gaps[,gap_rel:=((Female/Male)-1)*100]

r.cgm.gaps <- r.cgm.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                             gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                             gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                             gap_rel_mean=mean(gap_rel,na.rm=T),
                             gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                             gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                              by = .(ihme_loc_id,super_region_name, month, indicator)]


#super region level
r.srgmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                     by = .(super_region_name, sex, month, indicator,draw)]

r.srgm <-  r.srgmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                 by = .(super_region_name, sex, month, indicator)]

r.srmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                      by = .(super_region_name, month, indicator,draw)]

r.srm <-  r.srmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                   by = .(super_region_name, month, indicator)]



r.srgm.gaps <- dcast.data.table(r.srgmd,super_region_name+month+indicator+draw~sex)
r.srgm.gaps[,gap_abs:=(Female-Male)*100]
r.srgm.gaps[,gap_rel:=((Female/Male)-1)*100]

r.srgm.gaps <- r.srgm.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                             gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                             gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                             gap_rel_mean=mean(gap_rel,na.rm=T),
                             gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                             gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                         by = .(super_region_name, month, indicator)]




#total level
r.tgmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                     by = .(sex, month, indicator,draw)]

r.tgm <-  r.tgmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                 by = .(sex, month, indicator)]

r.tmd <-  gpr_final[, .(gpr_draw = weighted.mean(x = gpr_draw, w = population)),
                     by = .(month, indicator,draw)]

r.tm <-  r.tmd[, .(raked_pred=mean(gpr_draw,na.rm=T),raked_lower=quantile(gpr_draw,0.025,na.rm=T),raked_upper=quantile(gpr_draw,0.975,na.rm=T)),
                 by = .(month, indicator)]

r.tgm.gaps <- dcast.data.table(r.tgmd,month+indicator+draw~sex)
r.tgm.gaps[,gap_abs:=(Female-Male)*100]
r.tgm.gaps[,gap_rel:=((Female/Male)-1)*100]

r.tgm.gaps <- r.tgm.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                             gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                             gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                             gap_rel_mean=mean(gap_rel,na.rm=T),
                             gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                             gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                         by = .(month, indicator)]


#save results
fwrite(r.cgm, paste0(output_dir, 'cgm.csv'), row.names = F)
fwrite(r.cm, paste0(output_dir, 'cm.csv'), row.names = F)
fwrite(r.srgm, paste0(output_dir, 'srgm.csv'), row.names = F)
fwrite(r.srm, paste0(output_dir, 'srm.csv'), row.names = F)
fwrite(r.tgm, paste0(output_dir, 'tgm.csv'), row.names = F)
fwrite(r.tm, paste0(output_dir, 'tm.csv'), row.names = F)
fwrite(r.cgm.gaps, paste0(output_dir, 'cgm_gaps.csv'), row.names = F)
fwrite(r.srgm.gaps, paste0(output_dir, 'srgm_gaps.csv'), row.names = F)
fwrite(r.tgm.gaps, paste0(output_dir, 'tgm_gaps.csv'), row.names = F)


##### 4. GENDER GAPS FIGURE ####################################################################################################################################################################################################

#get + subset data
plt <- rbind(r.srgm.gaps,r.tgm.gaps,fill=T)
plt[is.na(super_region_name),super_region_name:="Total"]
plt <- plt[month%in%c(13,21)] #january and september

#get indicators
indic_labs <- fread("/FILEPATH/")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

#format indicators
plt[indicator=="hesitancy",indicator:="getvax"]
plt[indicator=="vaccinated",indicator:="V1"]
plt[indicator=="fully_vaccinated",indicator:="fullvax"]

#merge codebook
plt <- merge(plt,indic_labs,by="indicator",all.x=T)

#subset to main results
plt <- plt[main==1]

#fix ilabl for heath products
plt[ilab=="Disruption in health products access",ilab:="Disruption in health product access"]

#order indicators
plt[,ilab:=factor(ilab,levels=c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                "Any disruption in health care","Disruption in preventative care",
                                "Disruption in medication access","Disruption in health product access",
                                "Employment loss","Not working to care for others"))]

ordered_labs <- c("Vaccine hesitancy","Fully vaccinated",
                  "Any disruption in health care","Disruption in preventative care",
                  "Disruption in medication access","Disruption in health product access",
                  "Employment loss","Not working to care for others")

#tag significance
plt[,sig:="Not Significant"]
plt[gap_rel_mean<0&gap_rel_upper<0,sig:="Significant"]
plt[gap_rel_mean>0&gap_rel_lower>0,sig:="Significant"]

plt[gap_abs_mean<0,sig2:="Men Higher, Not Significant"]
plt[gap_abs_mean>0,sig2:="Women Higher, Not Significant"]
plt[gap_abs_mean<0&gap_abs_upper<0,sig2:="Men Higher, Significant"]
plt[gap_abs_mean>0&gap_abs_lower>0,sig2:="Women Higher, Significant"]

#order significance for legend
plt[,sig2:=factor(sig2,levels=c("Women Higher, Significant", "Women Higher, Not Significant", "Men Higher, Not Significant", "Men Higher, Significant"))]

#make all the indicator axes the same
gg <- list()
pg <- 1

plt[month==13, month_name := 'January 2021']
plt[month==21, month_name := 'September 2021']
plt[!is.na(month_name), facet_label := paste0(month_name, ' Gender Disparities: \n', ilab)]
for (i in ordered_labs) {
   if (pg == 1) {
    gg[[i]] <- ggplot(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')], aes(y = super_region_name, x=gap_rel_mean, fill=sig2,color=sig2,alpha=sig2)) +
      geom_bar(stat='identity',color='white') +
      facet_wrap(~facet_label,ncol=2) +
      xlim(min(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean, 0), max(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean)) +
      labs(y="",x="")+
      aesth +
      theme(legend.position = "none",
            axis.text.x = element_text(size=7.5,face="bold",angle=0,hjust=1),
            axis.text.y = element_text(size=7.5,face="bold"),
            strip.text = element_text(size=7.5,face="bold"),
            plot.background = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm")) +
      geom_hline(yintercept=7.5,linetype='longdash',alpha=.7) +
      geom_vline(xintercept=0) +
      guides(fill=guide_legend(nrow=1,byrow=TRUE))+
      scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
      scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
      scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))
  } else if (pg != 4 & pg != 1) {
    gg[[i]] <- ggplot(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')], aes(y = super_region_name, x=gap_rel_mean, fill=sig2,color=sig2,alpha=sig2)) +
      geom_bar(stat='identity',color='white') +
      facet_wrap(~month_name,ncol=2,labeller = labeller(month_name = c('January 2021'=paste0(i),'September 2021'=paste0(i)))) +
      xlim(min(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean, 0), max(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean)) +
      labs(y="",x="")+
      aesth +
      theme(legend.position = "none",
            axis.text.x = element_text(size=7.5,face="bold",angle=0,hjust=1),
            axis.text.y = element_text(size=7.5,face="bold"),
            strip.text = element_text(size=7.5,face="bold"),
            plot.background = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm")) +
      geom_hline(yintercept=7.5,linetype='longdash',alpha=.7) +
      geom_vline(xintercept=0) +
      guides(fill=guide_legend(nrow=1,byrow=TRUE))+
      scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
      scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
      scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))
  } else if (pg == 4) {
      gg[[i]] <- ggplot(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')], aes(y = super_region_name, x=gap_rel_mean, fill=sig2,color=sig2,alpha=sig2)) +
        geom_bar(stat='identity',color='white') +
        facet_wrap(~month_name,ncol=2,labeller = labeller(month_name = c('January 2021'=paste0(i),'September 2021'=paste0(i)))) +
        xlim(min(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean, 0), max(plt[ilab == i & (month_name=='January 2021'|month_name=='September 2021')]$gap_rel_mean)) +
        labs(y="",x="Female / Male, Relative % Difference")+
        aesth +
        theme(axis.text.x = element_text(size=7.5,face="bold",angle=0,hjust=1),
              axis.text.y = element_text(size=7.5,face="bold"),
              strip.text = element_text(size=7.5,face="bold"),
              plot.background = element_blank(),
              plot.margin = unit(c(0, 0, 0, 0), "cm")) +
        geom_hline(yintercept=7.5,linetype='longdash',alpha=.7) +
        geom_vline(xintercept=0) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
        scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
        scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))
      
    }
    pg <- pg+1
}

legend <- get_legend(gg[[4]] + 
                       guides(fill=guide_legend(nrow=1,byrow=TRUE)))

gg[[4]] <- gg[[4]] + theme(legend.position = "none")

temp <- plot_grid(gg[[1]],gg[[2]], gg[[3]],gg[[4]],gg[[5]],gg[[6]],gg[[7]],gg[[8]],ncol=1,align='h',axis='tb',rel_widths = c(2,1),vjust = 0.2, hjust = 0, label_x = 0.01,  label_size = 10)


                  
jpeg("FILEPATH.jpeg", width = 8, height = 12.5, units = 'in', res = 1600)
plot_grid(temp, legend, nrow=2,align='h',axis='tb',rel_heights = c(15, .2))
dev.off()


##### 5. JANUARY HEATMAP ##########################################################################################################################################################################################################

#get + subset data to january
plt <- copy(r.cgm.gaps)
plt <- plt[month==13]

#get indicators
indic_labs <- fread("/FILEPATH/")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

#format indicator names
plt[indicator=="hesitancy",indicator:="getvax"]
plt[indicator=="vaccinated",indicator:="V1"]
plt[indicator=="fully_vaccinated",indicator:="fullvax"]

#merge codebook
plt <- merge(plt,indic_labs,by="indicator",all.x=T)

#subset to main results
plt <- plt[main==1]

#order indicators
plt[,ilab:=factor(ilab,levels=c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                "Any disruption in health care","Disruption in preventative care",
                                "Disruption in medication access","Disruption in health products access",
                                "Employment loss","Not working to care for others"))]

#tag significance
plt[,sig:="No Comparison Possible"]
plt[gap_abs_mean<0,sig:="Men Higher, Not Significant"]
plt[gap_abs_mean>0,sig:="Women Higher, Not Significant"]
plt[gap_abs_mean<0&gap_abs_upper<0,sig:="Men Higher, Significant"]
plt[gap_abs_mean>0&gap_abs_lower>0,sig:="Women Higher, Significant"]

#add location metadata
plt <- merge(plt,hierarchy[,c("ihme_loc_id","location_name","lancet_label")],by="ihme_loc_id")
plt <- plt[order(super_region_name,lancet_label)]
plt[,lancet_label:=factor(lancet_label,levels=unique(plt$lancet_label))]
plt <- merge(plt,data.table(merge(expand.grid(location_name=unique(plt[,location_name]),ilab=unique(plt$ilab)),hierarchy[,c("location_name","super_region_name"),with=T]),by='location_name'),by=c("location_name","ilab","super_region_name"),all=T)

plt[,obs:=0]
plt[is.finite(gap_abs_mean),obs:=1]
plt[,num_obs:=sum(obs,na.rm=T),by=.(location_name)]

#tag missing significance
plt[is.na(sig),sig:="No Comparison Possible"]

#order significance levels for legend
plt[,sig:=factor(sig,levels=c("Women Higher, Significant", "Men Higher, Significant", "No Comparison Possible",  "Women Higher, Not Significant", "Men Higher, Not Significant"))]

#page 1
gg1 <- ggplot(plt[num_obs>3&super_region_name%in%c("High-income","Central Europe, Eastern Europe, and Central Asia","South Asia")],aes(y = location_name, x=ilab, fill=sig,alpha=sig,label=paste0(round(gap_rel_mean),"%"))) +
  geom_tile(color='black')+ scale_x_discrete(position = "top")+
  labs(y="",x="",subtitle="Relative Gender Gap, Female/Male",title="Gender Gaps in January 2021")+
  aesth + theme(legend.position = "top",plot.background = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3)) +
  theme(axis.text.y=element_text(size=8),axis.text.x = element_text(size=8,angle=0,hjust=0.5),legend.text = element_text(size=8))

gg1 <- gg1+ ggforce::facet_col(vars(super_region_name), scales = 'free_y', space = 'free')

#page 2
gg2 <- ggplot(plt[num_obs>3&!(super_region_name%in%c("High-income","Central Europe, Eastern Europe, and Central Asia","South Asia"))],aes(y = location_name, x=ilab, fill=sig,alpha=sig,label=paste0(round(gap_rel_mean),"%"))) +
  geom_tile(color='black')+scale_x_discrete(position = "top")+
  labs(y="",x="",subtitle="Relative Gender Gap, Female/Male",title="Gender Gaps in January 2021")+
  aesth + theme(legend.position = "top",plot.background = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))+
  theme(axis.text.y=element_text(size=8),axis.text.x = element_text(size=8,angle=0,hjust=0.5),legend.text = element_text(size=8))

gg2 <- gg2+ ggforce::facet_col(vars(super_region_name), scales = 'free_y', space = 'free')


#save plots
jpeg("FILEPATH.jpeg", width = 8, height = 10, units = 'in', res = 1600)
plot(gg1)
dev.off()

jpeg("FILEPATH.jpeg", width = 8, height = 10, units = 'in', res = 1600)
plot(gg2)
dev.off()



##### 6. SEPTEMBER HEATMAP #####################################################################################################################################################################################################

#subset data to september
plt <- copy(r.cgm.gaps)
plt <- plt[month==21]

#get indicator labels
indic_labs <- fread("/FILEPATH/")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

#format indicator names
plt[indicator=="hesitancy",indicator:="getvax"]
plt[indicator=="vaccinated",indicator:="V1"]
plt[indicator=="fully_vaccinated",indicator:="fullvax"]

#merge data with codebook
plt <- merge(plt,indic_labs,by="indicator",all.x=T)

#subset to main results
plt <- plt[main==1]

#order indicators
plt[,ilab:=factor(ilab,levels=c("Vaccine hesitancy" ,"Vaccine received","Fully vaccinated",
                                "Any disruption in health care","Disruption in preventative care",
                                "Disruption in medication access","Disruption in health products access",
                                "Employment loss","Not working to care for others"))]

#tag significance
plt[,sig:="No Comparison Possible"]
plt[gap_abs_mean<0,sig:="Men Higher, Not Significant"]
plt[gap_abs_mean>0,sig:="Women Higher, Not Significant"]
plt[gap_abs_mean<0&gap_abs_upper<0,sig:="Men Higher, Significant"]
plt[gap_abs_mean>0&gap_abs_lower>0,sig:="Women Higher, Significant"]

#add location metadata
plt <- merge(plt,hierarchy[,c("ihme_loc_id","location_name","lancet_label")],by="ihme_loc_id")
plt <- plt[order(super_region_name,lancet_label)]
plt[,lancet_label:=factor(lancet_label,levels=unique(plt$lancet_label))]
plt <- merge(plt,data.table(merge(expand.grid(location_name=unique(plt[,location_name]),ilab=unique(plt$ilab)),hierarchy[,c("location_name","super_region_name"),with=T]),by='location_name'),by=c("location_name","ilab","super_region_name"),all=T)

plt[,obs:=0]
plt[is.finite(gap_abs_mean),obs:=1]
plt[,num_obs:=sum(obs,na.rm=T),by=.(location_name)]

#tag missing significance
plt[is.na(sig),sig:="No Comparison Possible"]

#order significance levels for legend
plt[,sig:=factor(sig,levels=c("Women Higher, Significant", "Men Higher, Significant", "No Comparison Possible",  "Women Higher, Not Significant", "Men Higher, Not Significant"))]

#format long labels
lineBreak = function(x, linewidth = 20) {
  str_wrap(str_replace_all(gsub("in ","",x), "_", " "), width = linewidth)
}

#page 1
gg1 <- ggplot(plt[num_obs>3&super_region_name%in%c("High-income","Central Europe, Eastern Europe, and Central Asia","South Asia")],aes(y = location_name, x=ilab, fill=sig,alpha=sig,label=paste0(round(gap_rel_mean),"%"))) +
  geom_tile(color='black')+
  labs(y="",x="",subtitle="Relative Gender Disparity, Female/Male",title="Gender Disparities in September 2021")+
  aesth + theme(legend.position = "top",plot.background = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))+
  theme(axis.text.y=element_text(size=8),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, l = 0, b = 8)),
        axis.text.x = element_text(size=8,angle=0,hjust=0.5),
        legend.text = element_text(size=8))

gg1 <- gg1+ ggforce::facet_col(vars(super_region_name), scales = 'free_y', space = 'free')

#page 2
gg2 <- ggplot(plt[num_obs>3&!(super_region_name%in%c("High-income","Central Europe, Eastern Europe, and Central Asia","South Asia"))],aes(y = location_name, x=ilab, fill=sig,alpha=sig,label=paste0(round(gap_rel_mean),"%"))) +
  geom_tile(color='black')+
  labs(y="",x="",subtitle="Relative Gender Disparity, Female/Male",title="Gender Disparities in September 2021")+
  aesth + theme(legend.position = "top",plot.background = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = 'top') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_fill_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_color_manual(name="",values=c("Women Higher, Significant"="#9e0142","Women Higher, Not Significant"="#d73027","Men Higher, Not Significant"="#3288bd","Men Higher, Significant"="#5e4fa2","No Comparison Possible"="white"))+
  scale_alpha_manual(name="",values=c("Women Higher, Significant"=1,"Women Higher, Not Significant"=.3,"Men Higher, Not Significant"=.3,"Men Higher, Significant"=1,"No Comparison Possible"=.3))+
  theme(axis.text.y=element_text(size=8),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, l = 0, b = 8)),
        axis.text.x = element_text(size=8,angle=0,hjust=0.5),
        legend.text = element_text(size=8))

gg2 <- gg2+ ggforce::facet_col(vars(super_region_name), scales = 'free_y', space = 'free')


#save plots
jpeg("/FILEPATH.jpeg", width = 8, height = 10, units = 'in', res = 1600)
plot(gg1)
dev.off()

jpeg("/FILEPATH.jpeg", width = 8, height = 10, units = 'in', res = 1600)
plot(gg2)
dev.off()
