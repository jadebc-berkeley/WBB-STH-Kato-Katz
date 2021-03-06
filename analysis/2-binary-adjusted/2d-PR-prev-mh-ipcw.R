##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH IPCW analysis
# Moderate heavy infection

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
##############################################
rm(list=ls())
source(here::here("0-config.R"))

#----------------------------------------------
# load and pre-process analysis dataset 
#----------------------------------------------
data = read.csv(sth_data_path,stringsAsFactors=TRUE)

d=preprocess.sth(data)
d=preprocess.adj.sth(d)

d$block=as.factor(d$block)

# roof and landphone excluded due to low prevalence

W=c("hfiacat","index","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW=d[,c("block","tr","clusterid","sthmh","almh","hwmh","ttmh","hasoutcome",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$almh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$sthmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_mh_rr_h1_ipcw_j=format.tmle(est.al.h1,family="binomial")$rr
al_mh_rd_h1_ipcw_j=format.tmle(est.al.h1,family="binomial")$rd

hw_mh_rr_h1_ipcw_j=format.tmle(est.hw.h1,family="binomial")$rr
hw_mh_rd_h1_ipcw_j=format.tmle(est.hw.h1,family="binomial")$rd

tt_mh_rr_h1_ipcw_j=format.tmle(est.tt.h1,family="binomial")$rr
tt_mh_rd_h1_ipcw_j=format.tmle(est.tt.h1,family="binomial")$rd

sth_mh_rr_h1_ipcw_j=format.tmle(est.sth.h1,family="binomial")$rr
sth_mh_rd_h1_ipcw_j=format.tmle(est.sth.h1,family="binomial")$rd

rownames(al_mh_rr_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rr_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rr_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rr_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_mh_rd_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rd_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rd_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rd_h1_ipcw_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

est.al.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$almh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$sthmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_mh_rr_h2_ipcw_j=format.tmle(est.al.h2,family="binomial")$rr
al_mh_rd_h2_ipcw_j=format.tmle(est.al.h2,family="binomial")$rd

hw_mh_rr_h2_ipcw_j=format.tmle(est.hw.h2,family="binomial")$rr
hw_mh_rd_h2_ipcw_j=format.tmle(est.hw.h2,family="binomial")$rd

tt_mh_rr_h2_ipcw_j=format.tmle(est.tt.h2,family="binomial")$rr
tt_mh_rd_h2_ipcw_j=format.tmle(est.tt.h2,family="binomial")$rd

sth_mh_rr_h2_ipcw_j=format.tmle(est.sth.h2,family="binomial")$rr
sth_mh_rd_h2_ipcw_j=format.tmle(est.sth.h2,family="binomial")$rd

rownames(al_mh_rr_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rr_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rr_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rr_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_mh_rd_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rd_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rd_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rd_h2_ipcw_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

est.al.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$almh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$sthmh,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],Delta=dW$hasoutcome,
   family="binomial",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_mh_rr_h3_ipcw_j=format.tmle(est.al.h3,family="binomial")$rr
al_mh_rd_h3_ipcw_j=format.tmle(est.al.h3,family="binomial")$rd

hw_mh_rr_h3_ipcw_j=format.tmle(est.hw.h3,family="binomial")$rr
hw_mh_rd_h3_ipcw_j=format.tmle(est.hw.h3,family="binomial")$rd

tt_mh_rr_h3_ipcw_j=format.tmle(est.tt.h3,family="binomial")$rr
tt_mh_rd_h3_ipcw_j=format.tmle(est.tt.h3,family="binomial")$rd

sth_mh_rr_h3_ipcw_j=format.tmle(est.sth.h3,family="binomial")$rr
sth_mh_rd_h3_ipcw_j=format.tmle(est.sth.h3,family="binomial")$rd

rownames(al_mh_rr_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rr_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rr_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rr_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_mh_rd_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rd_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rd_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rd_h3_ipcw_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_mh_rr_h1_ipcw_j,hw_mh_rr_h1_ipcw_j,tt_mh_rr_h1_ipcw_j,sth_mh_rr_h1_ipcw_j,
     al_mh_rd_h1_ipcw_j,hw_mh_rd_h1_ipcw_j,tt_mh_rd_h1_ipcw_j,sth_mh_rd_h1_ipcw_j,

     al_mh_rr_h2_ipcw_j,hw_mh_rr_h2_ipcw_j,tt_mh_rr_h2_ipcw_j,sth_mh_rr_h2_ipcw_j,
     al_mh_rd_h2_ipcw_j,hw_mh_rd_h2_ipcw_j,tt_mh_rd_h2_ipcw_j,sth_mh_rd_h2_ipcw_j,

     al_mh_rr_h3_ipcw_j,hw_mh_rr_h3_ipcw_j,tt_mh_rr_h3_ipcw_j,sth_mh_rr_h3_ipcw_j,
     al_mh_rd_h3_ipcw_j,hw_mh_rd_h3_ipcw_j,tt_mh_rd_h3_ipcw_j,sth_mh_rd_h3_ipcw_j,
     
     file=paste0(save_data_path, "sth_pr_mh_ipcw.RData"))


