##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Binary STH outcomes

# Effect modification by geophagia yes/no

# by Jade
##############################################
library(devtools)
library(washb)

rm(list=ls())
data=read.csv("~/Box Sync/WASHB Parasites/Analysis datasets/Jade/sth.csv",stringsAsFactors=TRUE)
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)
d=preprocess.adj.sth(d)

d1=d[d$geophagia==1,]
d0=d[d$geophagia==0,]

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
dW1=d1[,c("block","tr","clusterid","sth","al","hw","tt",W)]
dW0=d0[,c("block","tr","clusterid","sth","al","hw","tt",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
# Child ate dirt
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.geo1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$al,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hw,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$tt,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.geo1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$sth,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_geo1_j=format.tmle(est.al.h1.geo1,family="binomial")$rr
al_rd_h1_geo1_j=format.tmle(est.al.h1.geo1,family="binomial")$rd

hw_rr_h1_geo1_j=format.tmle(est.hw.h1.geo1,family="binomial")$rr
hw_rd_h1_geo1_j=format.tmle(est.hw.h1.geo1,family="binomial")$rd

tt_rr_h1_geo1_j=format.tmle(est.tt.h1.geo1,family="binomial")$rr
tt_rd_h1_geo1_j=format.tmle(est.tt.h1.geo1,family="binomial")$rd

sth_rr_h1_geo1_j=format.tmle(est.sth.h1.geo1,family="binomial")$rr
sth_rd_h1_geo1_j=format.tmle(est.sth.h1.geo1,family="binomial")$rd

rownames(al_rr_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

# Child didn't eat dirt
est.al.h1.geo0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$al,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hw,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$tt,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1.geo0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$sth,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W],
   family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_geo0_j=format.tmle(est.al.h1.geo0,family="binomial")$rr
al_rd_h1_geo0_j=format.tmle(est.al.h1.geo0,family="binomial")$rd

hw_rr_h1_geo0_j=format.tmle(est.hw.h1.geo0,family="binomial")$rr
hw_rd_h1_geo0_j=format.tmle(est.hw.h1.geo0,family="binomial")$rd

tt_rr_h1_geo0_j=format.tmle(est.tt.h1.geo0,family="binomial")$rr
tt_rd_h1_geo0_j=format.tmle(est.tt.h1.geo0,family="binomial")$rd

sth_rr_h1_geo0_j=format.tmle(est.sth.h1.geo0,family="binomial")$rr
sth_rd_h1_geo0_j=format.tmle(est.sth.h1.geo0,family="binomial")$rd

rownames(al_rr_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_geo1_j,hw_rr_h1_geo1_j,tt_rr_h1_geo1_j,sth_rr_h1_geo1_j,
     al_rd_h1_geo1_j,hw_rd_h1_geo1_j,tt_rd_h1_geo1_j,sth_rd_h1_geo1_j,
     
     al_rr_h1_geo0_j,hw_rr_h1_geo0_j,tt_rr_h1_geo0_j,sth_rr_h1_geo0_j,
     al_rd_h1_geo0_j,hw_rd_h1_geo0_j,tt_rd_h1_geo0_j,sth_rd_h1_geo0_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_geophagia.RData")


