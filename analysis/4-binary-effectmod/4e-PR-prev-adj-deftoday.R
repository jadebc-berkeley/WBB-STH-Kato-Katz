##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Binary STH outcomes

# Effect modification by whether the stool was
# collected the same day as Kato-Katz

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
# create separate datasets for samples with 
# same day vs prior day defecation 
#----------------------------------------------
d1=d[d$defday=="Today" & d$defday!="",]
d0=d[d$defday=="Yesterday" & d$defday!="",]

# roof and landphone excluded due to low prevalence

# the following variables were dropped from the covariate list 
# because they had many levels and one of the effect modification
# strata has <20% prevalence: counter, birthorder
# using binary versions of the following due to sparse data: 
# food security, number of individuals in compound, number of <18 individuals
# age days, month
W=c("wet","sac","sex","hfiacatbin","momagebin","momheightbin","momedu",
    "Nlt18bin","Ncompbin","watminbin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW1=d1[,c("block","tr","clusterid","sth","al","hw","tt",W)]
dW0=d0[,c("block","tr","clusterid","sth","al","hw","tt",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# Defecated today
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.def1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$al,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.def1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hw,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.def1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$tt,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.def1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$sth,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_def1_j=format.tmle(est.al.h1.def1,family="binomial")$rr
al_rd_h1_def1_j=format.tmle(est.al.h1.def1,family="binomial")$rd

hw_rr_h1_def1_j=format.tmle(est.hw.h1.def1,family="binomial")$rr
hw_rd_h1_def1_j=format.tmle(est.hw.h1.def1,family="binomial")$rd

tt_rr_h1_def1_j=format.tmle(est.tt.h1.def1,family="binomial")$rr
tt_rd_h1_def1_j=format.tmle(est.tt.h1.def1,family="binomial")$rd

sth_rr_h1_def1_j=format.tmle(est.sth.h1.def1,family="binomial")$rr
sth_rd_h1_def1_j=format.tmle(est.sth.h1.def1,family="binomial")$rd

rownames(al_rr_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_def1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# Defecated yesterday 
#----------------------------------------------
est.al.h1.def0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$al,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.def0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hw,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.def0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$tt,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.def0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$sth,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_def0_j=format.tmle(est.al.h1.def0,family="binomial")$rr
al_rd_h1_def0_j=format.tmle(est.al.h1.def0,family="binomial")$rd

hw_rr_h1_def0_j=format.tmle(est.hw.h1.def0,family="binomial")$rr
hw_rd_h1_def0_j=format.tmle(est.hw.h1.def0,family="binomial")$rd

tt_rr_h1_def0_j=format.tmle(est.tt.h1.def0,family="binomial")$rr
tt_rd_h1_def0_j=format.tmle(est.tt.h1.def0,family="binomial")$rd

sth_rr_h1_def0_j=format.tmle(est.sth.h1.def0,family="binomial")$rr
sth_rd_h1_def0_j=format.tmle(est.sth.h1.def0,family="binomial")$rd

rownames(al_rr_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_def0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_def1_j,hw_rr_h1_def1_j,tt_rr_h1_def1_j,sth_rr_h1_def1_j,
     al_rd_h1_def1_j,hw_rd_h1_def1_j,tt_rd_h1_def1_j,sth_rd_h1_def1_j,

     al_rr_h1_def0_j,hw_rr_h1_def0_j,tt_rr_h1_def0_j,sth_rr_h1_def0_j,
     al_rd_h1_def0_j,hw_rd_h1_def0_j,tt_rd_h1_def0_j,sth_rd_h1_def0_j,

     file=paste0(save_data_path, "sth_pr_adj_defday.RData"))

