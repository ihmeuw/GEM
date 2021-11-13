#############################################################################################################################################################################
# Purpose: Process + clean + plot cross sectional results
#############################################################################################################################################################################

rm(list=ls())
run_date <- gsub('-', '_', Sys.Date())

output_dir <- paste0('/FILEPATH/')

##### 0. SET UP #############################################################################################################################################################
pacman::p_load(data.table, dplyr, ggplot2,parallel,stringr,gridExtra,cowplot,lme4,boot)

source(file.path("/FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(111, 771, release_id = 9)

pop_full <- fread("/FILEPATH/all_populations.csv")
adults <- c(66, 67, 9:19,20,30:32,235)
adult_pop <- pop_full[age_group_id %in% adults, lapply(.SD, function(x) sum(x)), by=c("location_id", "sex_id"), .SDcols = "population"]

"%not in%" <- Negate("%in%")
"%not like%" <- Negate("%like%")

aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text =element_text(size=13,face='bold'),plot.title =element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')


##### 1. GET + FORMAT + MERGE DATA ##########################################################################################################################################

dt <- rbindlist(lapply(list.files("/FILEPATH/",full.names = T,recursive = T),fread),fill=T)
dt <- dt[!is.na(location_name)]
dt <- dt[is.na(sex),sex:="Female"]
dt[,location_id:=NULL]

#locs 
dt <- merge(dt,hierarchy[,c("location_id","super_region_name","ihme_loc_id","location_name"),with=T],by="location_name")
dt[toupper(sex)=="MALE",sex:='Male']
dt[toupper(sex)=="FEMALE",sex:='Female']
dt[toupper(sex)=="MALE",sex_id:=1]
dt[toupper(sex)=="FEMALE",sex_id:=2]
dt <- merge(dt,adult_pop,by=c('location_id','sex_id'))

#calculate se
dt[,variance:=(((proportion*(1-proportion))/sample)^2)]
dt[,logit_variance:=variance * (1/((proportion)*(1-(proportion))))^2]
dt[,logit_se:=sqrt(logit_variance)]

#set max value
dt[proportion > 0.949, proportion := 0.949]
dt[, proportion := proportion/0.95]
dt[proportion > .999, proportion := .999]
dt[proportion < .001, proportion := .001]
dt[, logit_proportion := logit(proportion)]

#make 1000 draws
drw.list <- list()
for (i in 1:nrow(dt)){
print(i)
drw.list[[i]]  <-  data.table(t(rnorm(1000,dt[i,logit_proportion],dt[i,logit_se])))
}
drws <- rbindlist(drw.list)
names(drws) <- paste0("draw",0:999)

dt <- cbind(dt,drws)

#long on draw
dt.l <- melt.data.table(dt,id.vars=names(dt)[!names(dt)%like%"draw"],variable.name='draw',value.name = "logit_value")

#revert to normal space
dt.l[, value := inv.logit(logit_value)*0.95]


##### 2. AGGREGATE DATA ####################################################################################################################################################

r.cgd <- dt.l[,.(value=mean(value,na.rm=T)),by=.(ihme_loc_id,super_region_name,sex,indicator,draw)]
r.cg <-  r.cgd[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T)),
               by = .(ihme_loc_id,super_region_name, sex, indicator)]

r.cd <- dt.l[,.(value=mean(value,na.rm=T)),by=.(ihme_loc_id,super_region_name,indicator,draw)]
r.c <-  r.cd[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T)),
               by = .(ihme_loc_id,super_region_name,  indicator)]


r.cgd.gaps <- dcast.data.table(r.cgd,ihme_loc_id+super_region_name+indicator+draw~sex)
r.cgd.gaps[,gap_abs:=(Female-Male)*100]
r.cgd.gaps[,gap_rel:=((Female/Male)-1)*100]

r.cg.gaps <- r.cgd.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                               gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                               gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                               gap_rel_mean=mean(gap_rel,na.rm=T),
                               gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                               gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                           by = .(ihme_loc_id,super_region_name, indicator)]


#super region aggregates
r.srgd <- dt.l[,.(value=weighted.mean(x=value,w=population,na.rm=T),numloc=uniqueN(ihme_loc_id)),by=.(super_region_name,sex,indicator,draw)]

r.srg <-  r.srgd[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T),numloc=mean(numloc,na.rm=T)),
               by = .(super_region_name, sex, indicator)]

r.srd <- dt.l[,.(value=weighted.mean(x=value,w=population,na.rm=T),numloc=uniqueN(ihme_loc_id)),by=.(super_region_name,indicator,draw)]

r.sr <-  r.srd[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T),numloc=mean(numloc,na.rm=T)),
                 by = .(super_region_name,  indicator)]


r.srgd.gaps <- dcast.data.table(r.srgd,super_region_name+indicator+draw~sex)
r.srgd.gaps[,gap_abs:=(Female-Male)*100]
r.srgd.gaps[,gap_rel:=((Female/Male)-1)*100]

r.srg.gaps <- r.srgd.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                            gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                            gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                            gap_rel_mean=mean(gap_rel,na.rm=T),
                            gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                            gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                        by = .(super_region_name, indicator)]


##total aggregates
r.tgd <- dt.l[,.(value=weighted.mean(x=value,w=population,na.rm=T),numloc=uniqueN(ihme_loc_id)),by=.(sex,indicator,draw)]

r.tg <-  r.tgd[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T),numloc=mean(numloc,na.rm=T)),
                 by = .( sex, indicator)]

r.td <- dt.l[,.(value=weighted.mean(x=value,w=population,na.rm=T),numloc=uniqueN(ihme_loc_id)),by=.(indicator,draw)]

r.t <-  r.td[, .(value_mean=mean(value,na.rm=T),value_lower=quantile(value,0.025,na.rm=T),value_upper=quantile(value,0.975,na.rm=T),numloc=mean(numloc,na.rm=T)),
               by = .( indicator)]

r.tgd.gaps <- dcast.data.table(r.tgd,indicator+draw~sex)
r.tgd.gaps[,gap_abs:=(Female-Male)*100]
r.tgd.gaps[,gap_rel:=((Female/Male)-1)*100]

r.tg.gaps <- r.tgd.gaps[, .(gap_abs_mean=mean(gap_abs,na.rm=T),
                              gap_abs_lower=quantile(gap_abs,0.025,na.rm=T),
                              gap_abs_upper=quantile(gap_abs,0.975,na.rm=T),
                              gap_rel_mean=mean(gap_rel,na.rm=T),
                              gap_rel_lower=quantile(gap_rel,0.025,na.rm=T),
                              gap_rel_upper=quantile(gap_rel,0.975,na.rm=T)),
                          by = .(indicator)]


#save aggregated for number plugging
fwrite(r.c,'/FILEPATH/c.csv',row.names = F)
fwrite(r.cg,'/FILEPATH/cg.csv',row.names = F)
fwrite(r.cg.gaps,'/FILEPATH/cg_gaps.csv',row.names = F)
fwrite(r.sr,'/FILEPATH/sr.csv',row.names = F)
fwrite(r.srg,'/FILEPATH/srg.csv',row.names = F)
fwrite(r.srg.gaps,'/FILEPATH/srg_gaps.csv',row.names = F)
fwrite(r.t,'/FILEPATH/t.csv',row.names = F)
fwrite(r.tg,'/FILEPATH/tg.csv',row.names = F)
fwrite(r.tg.gaps,'/FILEPATH/tg_gaps.csv',row.names = F)


indic_labs <- fread("/FILEPATH/indicators_codebook.csv")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))


##### 3. MAKE FIGURES ####################################################################################################################################################

indic_labs <- fread("/FILEPATH/indicators_codebook.csv")
setnames(indic_labs,old=c("ind_name","ind_label"),new=c("indicator","ilab"))

plt <- copy(r.cg)
plt <- merge(plt, indic_labs, by="indicator", all=T)

plt[indicator == 'income_lost_combined_all', main := 1]
plt[indicator == 'income_lost_combined_all', ilab := "Income loss"]
plt[indicator == 'income_lost_combined', main := 0]

plt <- plt[main==1]

plt[, ilab:=factor(ilab, levels=c("Vaccine hesitancy" ,"Vaccine received", "Fully vaccinated",
                                  "Any disruption in health care", "Disruption in reproductive health", "Disruption in preventative care",
                                  "Disruption in medication access","Disruption in health products access",
                                  "Employment loss", "Income loss", "Not working to care for others", "Increase in care for others", "Increase in chores",
                                  "School drop out", "Adequate remote learning",
                                  "Perception of GBV Increase", "Feeling unsafe at home"))]

ordered_list <- c("Vaccine hesitancy" ,"Vaccine received", "Fully vaccinated",
                  "Any disruption in health care", "Disruption in reproductive health", "Disruption in preventative care",
                  "Disruption in medication access","Disruption in health products access",
                  "Employment loss", "Income loss", "Not working to care for others", "Increase in care for others", "Increase in chores",
                  "School drop out", "Adequate remote learning",
                  "Perception of GBV Increase", "Feeling unsafe at home")


plt <- merge(plt,hierarchy[,c("ihme_loc_id","location_name","lancet_label")],by="ihme_loc_id")
plt <- plt[order(super_region_name,lancet_label)]
plt[,lancet_label:=factor(lancet_label,levels=unique(plt$lancet_label))]

#sex vs. gender fix
plt[sex=='Female', Gender:='Women']
plt[sex=='Male', Gender:='Men']

#make + save one big figure
gg <- ggplot(plt,aes(x=lancet_label,y=value_mean*100,ymin=value_lower*100,ymax=value_upper*100,fill=Gender,color=Gender)) +
  geom_point(position=position_dodge(width=.05), color="black", stroke=.9, shape=21)+
  aesth +
  theme(axis.text.x=element_text(size=7,angle=90,hjust=1),
        axis.text.y = element_text(size=7),
        strip.text = element_text(size=7),
        legend.position = 'top') +
  geom_hline(yintercept=7.5,linetype='longdash',alpha=.7)+
  guides(color=F,fill="legend")+
  scale_size_continuous(name="Number of Countries of Data") +
  labs(x="",y="Percent of Respondents",title="All Cross-Sectional Outcomes by Gender and Country/Territory")+
  facet_grid(ilab~super_region_name,scales="free",space="free_x",
             labeller = label_wrap_gen(width = 10, multi_line = TRUE))

jpeg('/FILEPATH/Fig11_cross_sectional_all_together.jpg', width = 10.5, height = 8, units = 'in', res = 1600)
plot(gg)
dev.off()



#split across two pages --------------------
gg1 <- ggplot(plt[ilab %in% ordered_list[1:13]],aes(x=lancet_label,y=value_mean*100,ymin=value_lower*100,ymax=value_upper*100,fill=Gender,color=Gender)) +
  geom_point(position=position_dodge(width=.05),color="black",stroke=1.1,shape=21)+
  aesth +
  theme(axis.text.x=element_text(size=7,angle=90,hjust=1),
        axis.text.y = element_text(size=7),
        strip.text = element_text(size=7)) + 
  guides(color=F,fill="legend")+
  geom_hline(yintercept=7.5,linetype='longdash',alpha=.7)+
  scale_size_continuous(name="Number of Countries of Data") +labs(x="",y="Percent of Respondents",title="All Cross-Sectional Outcomes by Gender and Country/Territory")+
  facet_grid(ilab~super_region_name,scales="free",space="free_x",
             labeller = label_wrap_gen(width = 10, multi_line = TRUE))

gg2 <- ggplot(plt[ilab %in% ordered_list[14:length(ordered_list)]],aes(x=lancet_label,y=value_mean*100,ymin=value_lower*100,ymax=value_upper*100,fill=Gender,color=Gender)) +
  geom_point(position=position_dodge(width=.05),color="black",stroke=1.1,shape=21) +
  aesth +
  theme(axis.text.x=element_text(size=7,angle=90,hjust=1),
        axis.text.y = element_text(size=7),
        strip.text = element_text(size=7)) + 
  guides(color=F,fill="legend")+
  geom_hline(yintercept=7.5,linetype='longdash',alpha=.7) +
  scale_size_continuous(name="Number of Countries of Data") +
  labs(x="",y="Percent of Respondents",title="All Cross-Sectional Outcomes by Gender and Country/Territory") +
  facet_grid(ilab~super_region_name,scales="free",space="free_x",
             labeller = label_wrap_gen(width = 10, multi_line = TRUE))

#save page 1 + 2 separately 
jpeg('/FILEPATH/Fig11_cross_sectional_1.jpg', width = 10.5, height = 8, units = 'in', res = 1600)
plot(gg1)
dev.off()

jpeg('/FILEPATH/Fig11_cross_sectional_2.jpg', width = 10.5, height = 8, units = 'in', res = 1600)
plot(gg2)
dev.off()