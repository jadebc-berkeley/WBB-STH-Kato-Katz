##############################################
# WASH Benefits Bangladesh
# STH Kato-Katz analysis

# CONSORT flow chart

# by Jade
##############################################
rm(list=ls())
library(plyr)

wd=read.csv("~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_withdraw.csv")
enr=read.csv("~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_enroll_sth.csv")
kk=read.csv("~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_kk.csv")

# reorder tr labels
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
dropout=table(wd$tr[wd$dropout==1])
nolb=table(wd$tr[wd$nolb==1 & wd$dropout==1])
withdrew=table(wd$tr[wd$withdrew==1 & wd$dropout==1])
moved=table(wd$tr[wd$moved==1 & wd$dropout==1])
cdeath=table(wd$tr[wd$cdeath==1 & wd$dropout==1])
absent=table(wd$tr[wd$absent==1 & wd$dropout==1])

compound_lost_j = rbind(dropout,moved,absent,withdrew,nolb,cdeath)
rownames(compound_lost_j)=c("Compounds lost","Moved","Absent","Withdrew","No live birth","Child death")

# ---------------------------------------------
# Compounds at year 2 in STH analysis
# ---------------------------------------------
sth.comp.tab=table(enr$dataid[enr$hhstatus=="E"],enr$tr[enr$hhstatus=="E"])
sth.comp.tab[sth.comp.tab>1]=1
CompoundsSTH=colSums(sth.comp.tab)

# kk.comp.tab=table(kk$dataid,kk$tr)
# kk.comp.tab[kk.comp.tab>1]=1
# CompoundsSTH=colSums(kk.comp.tab)


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

save(flowchart_j,file="~/Dropbox/WASHB Parasites/Results/Jade/flowchart.RData")
