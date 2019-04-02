##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Binary STH outcomes

# Effect modification by percentage of latrine 
# mud floor > 0%

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

#----------------------------------------------
# create separate datasets for households with 
# percentage of latrine mud floor > 0%
#----------------------------------------------
d1=d[d$dirtfloor_lat==1,]
d0=d[d$dirtfloor_lat==0,]

# roof and landphone excluded due to low prevalence

W=c("counter","birthord","month","hfiacat","aged","sex","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW1=d1[,c("block","tr","clusterid","sth","al","hw","tt",W)]
dW0=d0[,c("block","tr","clusterid","sth","al","hw","tt",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# Latrine floor made of dirt
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.lmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$al,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.lmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hw,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.lmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$tt,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.lmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$sth,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_unadj_lmud1_j=format.tmle(est.al.h1.lmud1,family="binomial")$rr
al_rd_h1_unadj_lmud1_j=format.tmle(est.al.h1.lmud1,family="binomial")$rd

hw_rr_h1_unadj_lmud1_j=format.tmle(est.hw.h1.lmud1,family="binomial")$rr
hw_rd_h1_unadj_lmud1_j=format.tmle(est.hw.h1.lmud1,family="binomial")$rd

tt_rr_h1_unadj_lmud1_j=format.tmle(est.tt.h1.lmud1,family="binomial")$rr
tt_rd_h1_unadj_lmud1_j=format.tmle(est.tt.h1.lmud1,family="binomial")$rd

sth_rr_h1_unadj_lmud1_j=format.tmle(est.sth.h1.lmud1,family="binomial")$rr
sth_rd_h1_unadj_lmud1_j=format.tmle(est.sth.h1.lmud1,family="binomial")$rd

rownames(al_rr_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_lmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# Latrine floor not made of dirt
#----------------------------------------------
est.al.h1.lmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$al,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.lmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hw,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.lmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$tt,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.lmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$sth,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_unadj_lmud0_j=format.tmle(est.al.h1.lmud0,family="binomial")$rr
al_rd_h1_unadj_lmud0_j=format.tmle(est.al.h1.lmud0,family="binomial")$rd

hw_rr_h1_unadj_lmud0_j=format.tmle(est.hw.h1.lmud0,family="binomial")$rr
hw_rd_h1_unadj_lmud0_j=format.tmle(est.hw.h1.lmud0,family="binomial")$rd

tt_rr_h1_unadj_lmud0_j=format.tmle(est.tt.h1.lmud0,family="binomial")$rr
tt_rd_h1_unadj_lmud0_j=format.tmle(est.tt.h1.lmud0,family="binomial")$rd

sth_rr_h1_unadj_lmud0_j=format.tmle(est.sth.h1.lmud0,family="binomial")$rr
sth_rd_h1_unadj_lmud0_j=format.tmle(est.sth.h1.lmud0,family="binomial")$rd

rownames(al_rr_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_lmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_unadj_lmud1_j,hw_rr_h1_unadj_lmud1_j,tt_rr_h1_unadj_lmud1_j,sth_rr_h1_unadj_lmud1_j,
     al_rd_h1_unadj_lmud1_j,hw_rd_h1_unadj_lmud1_j,tt_rd_h1_unadj_lmud1_j,sth_rd_h1_unadj_lmud1_j,
     
     al_rr_h1_unadj_lmud0_j,hw_rr_h1_unadj_lmud0_j,tt_rr_h1_unadj_lmud0_j,sth_rr_h1_unadj_lmud0_j,
     al_rd_h1_unadj_lmud0_j,hw_rd_h1_unadj_lmud0_j,tt_rd_h1_unadj_lmud0_j,sth_rd_h1_unadj_lmud0_j,

     file=paste0(save_data_path, "sth_pr_unadj_dirtfloor_lat.RData"))


