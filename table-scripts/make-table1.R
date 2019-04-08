##############################################
# WASH Benefits 
# STH outcome analysis

# prevalence and intensity

# by Ayse
##############################################
rm(list=ls())

source(here::here("0-config.R"))

#---------------------------------------------
# load data
#---------------------------------------------
sth=read.csv(paste0(data_path,"washb-bangladesh-sth-psth-kk-public.csv"),
             stringsAsFactors=TRUE)

sth = read.csv(sth_data_path)

sth = sth %>% 
  filter(hasoutcome==1) %>%
  mutate(elec = ifelse(elec == "Has electricity", 1, 0))

#---------------------------------------------
# Reorder study arms
#---------------------------------------------
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,6,5,2,7,3,4)])
  return(x)}
sth=reord(sth)

#---------------------------------------------
# Functions to obtain mean, min, max
#---------------------------------------------
print_mean_range = function(vector){
  paste0(sprintf("%0.1f", vector[1]), " (", 
         sprintf("%0.1f", vector[2]), ", ", 
         sprintf("%0.1f", vector[3]), ")")
}

est_mean_range_tr = function(data, y){
  out = data %>% 
    group_by(tr) %>%
    summarise(mean = mean(!!sym(y), na.rm=TRUE),
              min = min(!!sym(y), na.rm=TRUE),
              max = max(!!sym(y), na.rm=TRUE))
 
  out.f = matrix(NA, nrow = nrow(out), ncol = 1)
  for(i in 1:nrow(out)){
    out.f[i,1] = print_mean_range(out[i,2:4])
  }
  
  return(out.f)

}


t(est_mean_range_tr(data = sth, y = "momage"))

#---------------------------------------------
# Functions to obtain percent, n
#---------------------------------------------
print_percent_n = function(vector){
  paste0(sprintf("%0.1f", vector[1]), " (", 
         sprintf("%0.1f", vector[2]), ")")
}

percent_n_tr = function(data, y){
  out = data %>% 
    group_by(tr) %>%
    summarise(percent = mean(!!sym(y), na.rm=TRUE)*100,
              n = sum(!!sym(y)==1, na.rm=TRUE))
  
  out.f = matrix(NA, nrow = nrow(out), ncol = 1)
  for(i in 1:nrow(out)){
    out.f[i,1] = print_percent_n(out[i,2:4])
  }
  
  return(out.f)
}

percent_n_tr(data = sth, y = "dadagri")

#---------------------------------------------
# Tabulate baseline variables 
#---------------------------------------------
t1=(t(aggregate(momage~tr,sth,mean))[2,])
t2=(t(aggregate(momeduy~tr,sth,mean))[2,])
t3=(t(aggregate(dadeduy~tr,sth,mean))[2,])
t4=100*as.numeric((t(aggregate(dadagri~tr,sth,mean))[2,]))
t5=(t(aggregate(Nhh~tr,sth,mean))[2,])
t6=100*as.numeric((t(aggregate(elec~tr,sth,mean))[2,]))
t7=100*as.numeric((t(aggregate(cement~tr,sth,mean))[2,]))
t8=(t(aggregate(landacre~tr,sth,mean))[2,])
t9=100*as.numeric((t(aggregate(tubewell~tr,sth,mean))[2,]))
t10=100*as.numeric((t(aggregate(storewat~tr,sth,mean))[2,]))
t11=100*as.numeric((t(aggregate(treatwat~tr,sth,mean))[2,]))
t12=100*as.numeric((t(aggregate(odmen~tr,sth,mean))[2,])) 
t13=100*as.numeric((t(aggregate(odwom~tr,sth,mean))[2,]))
t14=100*as.numeric((t(aggregate(odch815~tr,sth,mean))[2,]))
t15=100*as.numeric((t(aggregate(odch38~tr,sth,mean))[2,]))
t16=100*as.numeric((t(aggregate(odchu3~tr,sth,mean))[2,]))
t17=100*as.numeric((t(aggregate(latown~tr,sth,mean))[2,]))
t18=100*as.numeric((t(aggregate(latslab~tr,sth,mean))[2,]))
t19=100*as.numeric((t(aggregate(latseal~tr,sth,mean))[2,]))
t20=100*as.numeric((t(aggregate(latfeces~tr,sth,mean))[2,]))
t21=100*as.numeric((t(aggregate(potty~tr,sth,mean))[2,]))
t22=100*as.numeric((t(aggregate(humfeces~tr,sth,mean))[2,]))
t23=100*as.numeric((t(aggregate(humfecesch~tr,sth,mean))[2,]))
t24=100*as.numeric((t(aggregate(hwlatwat~tr,sth,mean))[2,]))
t25=100*as.numeric((t(aggregate(hwlatsoap~tr,sth,mean))[2,]))
t26=100*as.numeric((t(aggregate(hwkitwat~tr,sth,mean))[2,]))
t27=100*as.numeric((t(aggregate(hwkitsoap~tr,sth,mean))[2,]))

table1_a=rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27)
colnames(table1_a)=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition+WSH")
rownames(table1_a)=c("Age","Years of education","Years of education","Works in agriculture","Number of persons",
                   "Has electricity","Has a cement floor","Acres of agricultural land owned",
                   "Shallow tubewell primary water source","Stored water observed at home",
                   "Reported treating water yesterday","Adult men","Adult women","Children: 8-<15 years",
                   "Children: 3-<8 years", "Children: 0-<3 years","Owned","Concete slab","Functional water seal",
                   "Visible stool on slab or floor","Owned a potty","House","Chils's play area",
                   "Has water","Has soap","Has water","Has soap")

#---------------------------------------------
# save table
#---------------------------------------------

save(table1_a,file=paste0(table_path, "table1.RData"))
write.csv(table1_a,file=paste0(table_path, "table1.csv"))

