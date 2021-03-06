##############################################
# WASH Benefits Bangladesh
# STH Kato-Katz analysis

# CONSORT flow chart
# Corresponds to Manuscript Fig 1 
# Creates dataset used in flow chart

# by Jade
##############################################
rm(list=ls())
# library(plyr)

source(here::here("0-config.R"))

# ---------------------------------------------
# load data
# ---------------------------------------------
wd=read.csv(paste0(sth_data_path,"washb-bangladesh-sth-consort-endline-withdraw-public.csv"))
enr=read.csv(paste0(sth_data_path,"washb-bangladesh-sth-consort-endline-enroll-public.csv"))
kk=read.csv(paste0(sth_data_path,"washb-bangladesh-sth-consort-endline-kato-katz-public.csv"))

# ---------------------------------------------
# reorder tr labels
# ---------------------------------------------
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,6,5,2,7,3,4)])
  return(x)
}

wd=reord(wd)
enr=reord(enr)
kk=reord(kk)

# ---------------------------------------------
# Follow-up at year 2 
# (compounds lost, moved, absent, withdrew, no LB, child death)
# ---------------------------------------------
dropout=table(wd$tr[wd$hhstatus!="E"])
nolb=table(wd$tr[wd$sthnolb==1])
withdrew=table(wd$tr[wd$sthwd==1])
moved=table(wd$tr[wd$sthmoved==1])
cdeath=table(wd$tr[wd$sthcd==1])
absent=table(wd$tr[wd$sthabsent==1])

compound_lost_j = rbind(dropout,nolb,cdeath,moved,withdrew,absent)
rownames(compound_lost_j)=c("Compounds lost","No live birth","Child death","Moved","Withdrew","Absent")

# ---------------------------------------------
# Compounds at year 2 in STH analysis
# ---------------------------------------------
sth.comp.tab=table(enr$dataid[enr$hhstatus=="E"],enr$tr[enr$hhstatus=="E"])
sth.comp.tab[sth.comp.tab>1]=1
CompoundsSTH=colSums(sth.comp.tab)

# ---------------------------------------------
# Has sample T1, O1, C1
# ---------------------------------------------
kk.samp.T12=table(kk$tr[!is.na(kk$originalAL) & !is.na(kk$originalTT) & 
        !is.na(kk$originalHW) & (kk$personid=="T1" | kk$personid=="T2")])
kk.samp.C1=table(kk$tr[!is.na(kk$originalAL) & !is.na(kk$originalTT) & 
        !is.na(kk$originalHW) & kk$personid=="C1"])
kk.samp.O1=table(kk$tr[!is.na(kk$originalAL) & !is.na(kk$originalTT) & 
        !is.na(kk$originalHW) & kk$personid=="O1"])

kk.samp=rbind(kk.samp.T12, kk.samp.C1, kk.samp.O1)
rownames(kk.samp)=c("T1/T2 has sample","C1 has sample","O1 has sample")

# ---------------------------------------------
# Enrolled T1, O1, C1

# EWS = enrolled in WASH benefits parasites assessment
# and STH add on
# EWSB = same as above but blood refusal 
# ---------------------------------------------
enr.T12=table(enr$tr[enr$enroll=="E" & (enr$personid=="T1" | enr$personid=="T2")])

enr.C1=table(enr$tr[enr$enroll=="E" & enr$personid=="C1"])

enr.O1=table(enr$tr[enr$enroll=="E" & enr$personid=="O1"])

enr.all=rbind(enr.T12,enr.C1,enr.O1)
rownames(enr.all)=c("T1/T2 Enrolled","C1 Enrolled","O1 Enrolled")

# ---------------------------------------------
# Combine all
# ---------------------------------------------
flowchart_j=rbind(compound_lost_j,CompoundsSTH,enr.all,kk.samp)

flowchart_j

save(flowchart_j,file=paste0(save_data_path,"flowchart.RData"))
