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
sth = read.csv(sth_data_path)

sth = sth %>% 
  mutate(delta = hasoutcome,
         elec = ifelse(elec == "Has electricity", 1, 0))

sth$index=ifelse(sth$indexchild=="Index child",1,0)

#---------------------------------------------
# Reorder study arms
#---------------------------------------------
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,6,5,2,7,3,4)])
  return(x)}
sth=reord(sth)

#---------------------------------------------
# Tabulate baseline variables 
#---------------------------------------------
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

tables1=rbind(tind,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,
             t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27)
colnames(tables1)=c("Missing","Observed")

tables1 = as.data.frame(tables1)

tables1 = tables1 %>%
  mutate(Missing = sprintf("%0.01f", as.numeric(as.character(Missing))),
         Observed = sprintf("%0.01f", as.numeric(as.character(Observed))))

mylabel = c(
  "Index child, $\\%$",
  "Age, mean",
  "Years of education, mean",
  "Years of education, mean",
  "Works in agriculture, $\\%$",
  "Number of persons, mean",
  "Has electricity, $\\%$",
  "Has a cement floor, $\\%$",
  "Acres of agricultural land owned, mean",
  "Shallow tubewell primary water source, $\\%$",
  "Stored water observed at home, $\\%$",
  "Reported treating water yesterday, $\\%$",
  "Adult men",
  "Adult women",
  "Children: 8-<15 years",
  "Children: 3-<8 years",
  "Children: 0-<3 years",
  "Owned",
  "Concete slab",
  "Functional water seal",
  "Visible stool on slab or floor",
  "Owned a potty, $\\%$",
  "House",
  "Childs's play area",
  "Has water",
  "Has soap",
  "Has water",
  "Has soap"
)

# add indentation
mylabel[c(2:12,22)] = paste0("\\hspace{3mm}", mylabel[c(2:12,22)])
mylabel[c(13:21, 23:28)] = paste0("\\hspace{6mm}", mylabel[c(13:21, 23:28)])

tables1 = tables1 %>% 
  mutate(label = mylabel) %>%
  select(label, Missing, Observed)

# create subheaders
make_subheader = function(name){
  return(data.frame(label = paste0("\\textbf{",name,"}"), 
                        Missing = "",
                        Observed = ""))
}
maternal = make_subheader("Maternal")
paternal = make_subheader("Paternal")
household = make_subheader("Household")
water = make_subheader("Drinking water")
sanitation = make_subheader("Sanitation")
handwashing = make_subheader("Handwashing")

make_subsubheader = function(name){
  return(data.frame(label = paste0("\\hspace{3mm}", name), 
                    Missing = "",
                    Observed = ""))
}

s1 = make_subsubheader("Daily defecating in the open, $\\%$")
s2 = make_subsubheader("Latrine, $\\%$")
s3 = make_subsubheader("Human feces observed in, $\\%$")
h1 = make_subsubheader("Has within 6 steps of latrine, $\\%$")
h2 = make_subsubheader("Has within 6 steps of kitchen, $\\%$")

tables1_f = bind_rows(
  tables1[1,],
  maternal,
  tables1[2:3,],
  paternal,
  tables1[4:5,],
  household,
  tables1[6:9,],
  water,
  tables1[10:12,],
  sanitation,
  s1,
  tables1[13:17,],
  s2,
  tables1[18:21,],
  tables1[22,],
  s3,
  tables1[23:24,],
  handwashing,
  h1,
  tables1[25:26,],
  h2,
  tables1[27:28,]
) 

write.csv(tables1_f,file=paste0(table_path, "table-s1.csv"), row.names=FALSE)
