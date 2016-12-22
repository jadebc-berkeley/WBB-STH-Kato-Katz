##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

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

d1=d[d$geophagia=="yes",]
d0=d[d$geophagia=="no",]

# roof and landphone excluded due to low prevalence

# the following variables were dropped from the covariate list 
# because they had many levels and one of the effect modification
# strata has <20% prevalence: 
# counter, month, birthorder, food security, number of individuals 
# in compound, number of <18 individuals
W=c("aged","sex","momage","momheight","momedu","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

geo1=d1[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]
geo0=d0[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
# child ate soil
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.geo1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$alepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$hwepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$ttepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_geo1_j=format.epg.tmle(est.al.h1.geo1.ari)
hw_fecr_ari_h1_geo1_j=format.epg.tmle(est.hw.h1.geo1.ari)
tt_fecr_ari_h1_geo1_j=format.epg.tmle(est.tt.h1.geo1.ari)

rownames(al_fecr_ari_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.geo1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$logalepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$loghwepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo1$logttepg,tr=geo1$tr,
   pair=geo1$block, id=geo1$block,W=geo1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_geo1_j=format.epg.tmle(est.al.h1.geo1.geo)
hw_fecr_geo_h1_geo1_j=format.epg.tmle(est.hw.h1.geo1.geo)
tt_fecr_geo_h1_geo1_j=format.epg.tmle(est.tt.h1.geo1.geo)

rownames(al_fecr_geo_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_geo1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

# Child didn't eat soil
est.al.h1.geo0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$alepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$hwepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$ttepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_geo0_j=format.epg.tmle(est.al.h1.geo0.ari)
hw_fecr_ari_h1_geo0_j=format.epg.tmle(est.hw.h1.geo0.ari)
tt_fecr_ari_h1_geo0_j=format.epg.tmle(est.tt.h1.geo0.ari)

rownames(al_fecr_ari_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.geo0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$logalepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$loghwepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=geo0$logttepg,tr=geo0$tr,
   pair=geo0$block, id=geo0$block,W=geo0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_geo0_j=format.epg.tmle(est.al.h1.geo0.geo)
hw_fecr_geo_h1_geo0_j=format.epg.tmle(est.hw.h1.geo0.geo)
tt_fecr_geo_h1_geo0_j=format.epg.tmle(est.tt.h1.geo0.geo)

rownames(al_fecr_geo_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_geo0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_fecr_ari_h1_ari1_j,hw_fecr_ari_h1_ari1_j,tt_fecr_ari_h1_ari1_j,
     al_fecr_geo_h1_geo1_j,hw_fecr_geo_h1_geo1_j,tt_fecr_geo_h1_geo1_j,

     al_fecr_geo_h1_geo0_j,hw_fecr_geo_h1_geo0_j,tt_fecr_geo_h1_geo0_j,
     al_fecr_ari_h1_geo0_j,hw_fecr_ari_h1_geo0_j,tt_fecr_ari_h1_geo0_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_geophagia.RData")


