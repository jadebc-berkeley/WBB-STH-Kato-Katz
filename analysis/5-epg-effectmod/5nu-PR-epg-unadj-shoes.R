##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

# Effect modification by wearing shoes at the 
# time of data collection 

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
# create separate datasets for those who wore
# shoes vs those who did not
#----------------------------------------------
d1=d[d$shoes==1,]
d0=d[d$shoes==0,]

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

shoe1=d1[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]
shoe0=d0[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# child was wearing shoes
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.shoe1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$alepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.shoe1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$hwepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.shoe1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$ttepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_unadj_shoe1_j=format.epg.tmle(est.al.h1.shoe1.ari)
hw_fecr_ari_h1_unadj_shoe1_j=format.epg.tmle(est.hw.h1.shoe1.ari)
tt_fecr_ari_h1_unadj_shoe1_j=format.epg.tmle(est.tt.h1.shoe1.ari)

rownames(al_fecr_ari_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.shoe1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$logalepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.shoe1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$loghwepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.shoe1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe1$logttepg,tr=shoe1$tr,
   pair=shoe1$block, id=shoe1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_unadj_shoe1_j=format.epg.tmle(est.al.h1.shoe1.geo)
hw_fecr_geo_h1_unadj_shoe1_j=format.epg.tmle(est.hw.h1.shoe1.geo)
tt_fecr_geo_h1_unadj_shoe1_j=format.epg.tmle(est.tt.h1.shoe1.geo)

rownames(al_fecr_geo_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_unadj_shoe1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# Child wasn't wearing shoes
#----------------------------------------------
est.al.h1.shoe0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$alepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.shoe0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$hwepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.shoe0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$ttepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_unadj_shoe0_j=format.epg.tmle(est.al.h1.shoe0.ari)
hw_fecr_ari_h1_unadj_shoe0_j=format.epg.tmle(est.hw.h1.shoe0.ari)
tt_fecr_ari_h1_unadj_shoe0_j=format.epg.tmle(est.tt.h1.shoe0.ari)

rownames(al_fecr_ari_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.shoe0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$logalepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.shoe0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$loghwepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.shoe0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=shoe0$logttepg,tr=shoe0$tr,
   pair=shoe0$block, id=shoe0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_unadj_shoe0_j=format.epg.tmle(est.al.h1.shoe0.geo)
hw_fecr_geo_h1_unadj_shoe0_j=format.epg.tmle(est.hw.h1.shoe0.geo)
tt_fecr_geo_h1_unadj_shoe0_j=format.epg.tmle(est.tt.h1.shoe0.geo)

rownames(al_fecr_geo_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_unadj_shoe0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_fecr_ari_h1_unadj_shoe1_j,hw_fecr_ari_h1_unadj_shoe1_j,tt_fecr_ari_h1_unadj_shoe1_j,
     al_fecr_geo_h1_unadj_shoe1_j,hw_fecr_geo_h1_unadj_shoe1_j,tt_fecr_geo_h1_unadj_shoe1_j,

     al_fecr_ari_h1_unadj_shoe0_j,hw_fecr_ari_h1_unadj_shoe0_j,tt_fecr_ari_h1_unadj_shoe0_j,
     al_fecr_geo_h1_unadj_shoe0_j,hw_fecr_geo_h1_unadj_shoe0_j,tt_fecr_geo_h1_unadj_shoe0_j,
     
     file=paste0(save_data_path, "sth_pr_epg_adj_shoes.RData"))


