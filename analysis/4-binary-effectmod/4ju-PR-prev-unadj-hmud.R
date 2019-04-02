##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Binary STH outcomes

# Effect modification by percentage of household 
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
# and without dirt floors
#----------------------------------------------
d1=d[d$dirtfloor_hh==1,]
d0=d[d$dirtfloor_hh==0,]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# Household floor made of dirt
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.hmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d1$al,tr=d1$tr,
   pair=d1$block, id=d1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d1$hw,tr=d1$tr,
   pair=d1$block, id=d1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d1$tt,tr=d1$tr,
   pair=d1$block, id=d1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.hmud1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d1$sth,tr=d1$tr,
   pair=d1$block, id=d1$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_unadj_hmud1_j=format.tmle(est.al.h1.hmud1,family="binomial")$rr
al_rd_h1_unadj_hmud1_j=format.tmle(est.al.h1.hmud1,family="binomial")$rd

hw_rr_h1_unadj_hmud1_j=format.tmle(est.hw.h1.hmud1,family="binomial")$rr
hw_rd_h1_unadj_hmud1_j=format.tmle(est.hw.h1.hmud1,family="binomial")$rd

tt_rr_h1_unadj_hmud1_j=format.tmle(est.tt.h1.hmud1,family="binomial")$rr
tt_rd_h1_unadj_hmud1_j=format.tmle(est.tt.h1.hmud1,family="binomial")$rd

sth_rr_h1_unadj_hmud1_j=format.tmle(est.sth.h1.hmud1,family="binomial")$rr
sth_rd_h1_unadj_hmud1_j=format.tmle(est.sth.h1.hmud1,family="binomial")$rd

rownames(al_rr_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# Household floor not made of dirt
#----------------------------------------------
est.al.h1.hmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d0$al,tr=d0$tr,
   pair=d0$block, id=d0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d0$hw,tr=d0$tr,
   pair=d0$block, id=d0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d0$tt,tr=d0$tr,
   pair=d0$block, id=d0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.hmud0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=d0$sth,tr=d0$tr,
   pair=d0$block, id=d0$block,
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_unadj_hmud0_j=format.tmle(est.al.h1.hmud0,family="binomial")$rr
al_rd_h1_unadj_hmud0_j=format.tmle(est.al.h1.hmud0,family="binomial")$rd

hw_rr_h1_unadj_hmud0_j=format.tmle(est.hw.h1.hmud0,family="binomial")$rr
hw_rd_h1_unadj_hmud0_j=format.tmle(est.hw.h1.hmud0,family="binomial")$rd

tt_rr_h1_unadj_hmud0_j=format.tmle(est.tt.h1.hmud0,family="binomial")$rr
tt_rd_h1_unadj_hmud0_j=format.tmle(est.tt.h1.hmud0,family="binomial")$rd

sth_rr_h1_unadj_hmud0_j=format.tmle(est.sth.h1.hmud0,family="binomial")$rr
sth_rd_h1_unadj_hmud0_j=format.tmle(est.sth.h1.hmud0,family="binomial")$rd

rownames(al_rr_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_unadj_hmud1_j,hw_rr_h1_unadj_hmud1_j,tt_rr_h1_unadj_hmud1_j,sth_rr_h1_unadj_hmud1_j,
     al_rd_h1_unadj_hmud1_j,hw_rd_h1_unadj_hmud1_j,tt_rd_h1_unadj_hmud1_j,sth_rd_h1_unadj_hmud1_j,

     al_rr_h1_unadj_hmud0_j,hw_rr_h1_unadj_hmud0_j,tt_rr_h1_unadj_hmud0_j,sth_rr_h1_unadj_hmud0_j,
     al_rd_h1_unadj_hmud0_j,hw_rd_h1_unadj_hmud0_j,tt_rd_h1_unadj_hmud0_j,sth_rd_h1_unadj_hmud0_j,
     
     file=paste0(save_data_path, "sth_pr_unadj_dirtfloor_hh.RData"))


