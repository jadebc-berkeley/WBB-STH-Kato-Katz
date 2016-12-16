##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH unadjusted analysis

# by Jade
##############################################
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv")
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)
  
# subset to columns needed for unadjusted PR
df = d[,c("block","clusterid","tr","almh","ttmh","hwmh","sthmh")]
df$block=as.factor(df$block)

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

# Poisson regression for RRs
al_mh_rr_h1_unadj_j=t(apply(matrix(trlist), 1,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

hw_mh_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

tt_mh_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

sth_mh_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

# Linear regression for RDs
al_mh_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

hw_mh_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

tt_mh_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

sth_mh_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))


rownames(al_mh_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_mh_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------

trlist=c("Water","Sanitation","Handwashing")

# Poisson regression for RRs
al_mh_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

hw_mh_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

tt_mh_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

sth_mh_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

# Linear regression for RDs
al_mh_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

hw_mh_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

tt_mh_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

sth_mh_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

rownames(al_mh_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_mh_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

# Poisson regression for RRs
al_mh_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

hw_mh_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

tt_mh_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

sth_mh_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

# Linear regression for RDs
al_mh_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$almh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

hw_mh_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hwmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

tt_mh_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$ttmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

sth_mh_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sthmh,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

rownames(al_mh_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_mh_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_mh_rr_h1_unadj_j,hw_mh_rr_h1_unadj_j,tt_mh_rr_h1_unadj_j,sth_mh_rr_h1_unadj_j,
     al_mh_rd_h1_unadj_j,hw_mh_rd_h1_unadj_j,tt_mh_rd_h1_unadj_j,sth_mh_rd_h1_unadj_j,

     al_mh_rr_h2_unadj_j,hw_mh_rr_h2_unadj_j,tt_mh_rr_h2_unadj_j,sth_mh_rr_h2_unadj_j,
     al_mh_rd_h2_unadj_j,hw_mh_rd_h2_unadj_j,tt_mh_rd_h2_unadj_j,sth_mh_rd_h2_unadj_j,

     al_mh_rr_h3_unadj_j,hw_mh_rr_h3_unadj_j,tt_mh_rr_h3_unadj_j,sth_mh_rr_h3_unadj_j,
     al_mh_rd_h3_unadj_j,hw_mh_rd_h3_unadj_j,tt_mh_rd_h3_unadj_j,sth_mh_rd_h3_unadj_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_mh_pr_unadj.RData")

