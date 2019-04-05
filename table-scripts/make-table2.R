##############################################
# WASH Benefits 
# STH outcome analysis

# prevalence and intensity

# by Ayse
##############################################
rm(list=ls())
sth=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Ayse/2-washb-bangladesh-psth-table2.csv",stringsAsFactors=TRUE)
sth$index=ifelse(sth$indexchild=="Index child",1,0)

# Reorder study arms
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,6,5,2,7,3,4)])
  return(x)}
sth=reord(sth)

# Tabulate baseline variables 
tind=100*as.numeric((t(aggregate(index~delta,sth,mean))[2,]))
t1=(t(aggregate(momage~delta,sth,mean))[2,])
t2=(t(aggregate(momeduy~delta,sth,mean))[2,])
t3=(t(aggregate(dadeduy~delta,sth,mean))[2,])
t4=100*as.numeric((t(aggregate(dadagri~delta,sth,mean))[2,]))
t5=(t(aggregate(Nhh~delta,sth,mean))[2,])
t6=100*as.numeric((t(aggregate(elec~delta,sth,mean))[2,]))
t7=100*as.numeric((t(aggregate(cement~delta,sth,mean))[2,]))
t8=(t(aggregate(landacre~delta,sth,mean))[2,])
t9=100*as.numeric((t(aggregate(tubewell~delta,sth,mean))[2,]))
t10=100*as.numeric((t(aggregate(storewat~delta,sth,mean))[2,]))
t11=100*as.numeric((t(aggregate(treatwat~delta,sth,mean))[2,]))
t12=100*as.numeric((t(aggregate(odmen~delta,sth,mean))[2,])) 
t13=100*as.numeric((t(aggregate(odwom~delta,sth,mean))[2,]))
t14=100*as.numeric((t(aggregate(odch815~delta,sth,mean))[2,]))
t15=100*as.numeric((t(aggregate(odch38~delta,sth,mean))[2,]))
t16=100*as.numeric((t(aggregate(odchu3~delta,sth,mean))[2,]))
t17=100*as.numeric((t(aggregate(latown~delta,sth,mean))[2,]))
t18=100*as.numeric((t(aggregate(latslab~delta,sth,mean))[2,]))
t19=100*as.numeric((t(aggregate(latseal~delta,sth,mean))[2,]))
t20=100*as.numeric((t(aggregate(latfeces~delta,sth,mean))[2,]))
t21=100*as.numeric((t(aggregate(potty~delta,sth,mean))[2,]))
t22=100*as.numeric((t(aggregate(humfeces~delta,sth,mean))[2,]))
t23=100*as.numeric((t(aggregate(humfecesch~delta,sth,mean))[2,]))
t24=100*as.numeric((t(aggregate(hwlatwat~delta,sth,mean))[2,]))
t25=100*as.numeric((t(aggregate(hwlatsoap~delta,sth,mean))[2,]))
t26=100*as.numeric((t(aggregate(hwkitwat~delta,sth,mean))[2,]))
t27=100*as.numeric((t(aggregate(hwkitsoap~delta,sth,mean))[2,]))

table2_a=rbind(tind,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,
             t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27)
colnames(table2_a)=c("Missing","Observed")
rownames(table2_a)=c("Index child","Age","Years of education","Years of education","Works in agriculture","Number of persons",
                   "Has electricity","Has a cement floor","Acres of agricultural land owned",
                   "Shallow tubewell primary water source","Stored water observed at home",
                   "Reported treating water yesterday","Adult men","Adult women","Children: 8-<15 years",
                   "Children: 3-<8 years", "Children: 0-<3 years","Owned","Concete slab","Functional water seal",
                   "Visible stool on slab or floor","Owned a potty","House","Chils's play area",
                   "Has water","Has soap","Has water","Has soap")

save(table2_a,file="~/Dropbox/WASHB Parasites/Results/Ayse/table2.RData")		
save(table2_a,file="~/Box Sync/WASHB Parasites/Results/Ayse/table2.RData")
write.csv(table2_a,file="~/Box Sync/WASHB Parasites/Results/Tables/table2.csv")
