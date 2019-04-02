##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

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

# roof and landphone excluded due to low prevalence

# the following variables were dropped from the covariate list 
# because they had many levels and one of the effect modification
# strata has <20% prevalence: 
# counter, birthorder
# using binary versions of the following due to sparse data: 
# food security, number of individuals in compound, number of <18 individuals
# age days, month
# mobile phone dropped because of sparse data in one level 
W=c("wet","sac","sex","hfiacatbin","momagebin","momheightbin","momedu",
    "Nlt18bin","Ncompbin","watminbin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach")

dW1=d1[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]
dW0=d0[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# household floor made of dirt
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.hmud1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_hmud1_j=format.epg.tmle(est.al.h1.hmud1.ari)
hw_fecr_ari_h1_hmud1_j=format.epg.tmle(est.hw.h1.hmud1.ari)
tt_fecr_ari_h1_hmud1_j=format.epg.tmle(est.tt.h1.hmud1.ari)

rownames(al_fecr_ari_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.hmud1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$logalepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$loghwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$logttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_hmud1_j=format.epg.tmle(est.al.h1.hmud1.geo)
hw_fecr_geo_h1_hmud1_j=format.epg.tmle(est.hw.h1.hmud1.geo)
tt_fecr_geo_h1_hmud1_j=format.epg.tmle(est.tt.h1.hmud1.geo)

rownames(al_fecr_geo_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_hmud1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# household floor not made of dirt
#----------------------------------------------
est.al.h1.hmud0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$alepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$ttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_hmud0_j=format.epg.tmle(est.al.h1.hmud0.ari)
hw_fecr_ari_h1_hmud0_j=format.epg.tmle(est.hw.h1.hmud0.ari)
tt_fecr_ari_h1_hmud0_j=format.epg.tmle(est.tt.h1.hmud0.ari)

rownames(al_fecr_ari_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.hmud0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$logalepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.hmud0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$loghwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.hmud0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$logttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_hmud0_j=format.epg.tmle(est.al.h1.hmud0.geo)
hw_fecr_geo_h1_hmud0_j=format.epg.tmle(est.hw.h1.hmud0.geo)
tt_fecr_geo_h1_hmud0_j=format.epg.tmle(est.tt.h1.hmud0.geo)

rownames(al_fecr_geo_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_hmud0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_fecr_ari_h1_hmud1_j,hw_fecr_ari_h1_hmud1_j,tt_fecr_ari_h1_hmud1_j,
     al_fecr_geo_h1_hmud1_j,hw_fecr_geo_h1_hmud1_j,tt_fecr_geo_h1_hmud1_j,

     al_fecr_ari_h1_hmud0_j,hw_fecr_ari_h1_hmud0_j,tt_fecr_ari_h1_hmud0_j,
     al_fecr_geo_h1_hmud0_j,hw_fecr_geo_h1_hmud0_j,tt_fecr_geo_h1_hmud0_j,
     
     file=paste0(save_data_path, "sth_pr_epg_adj_dirtfloor_hh.RData"))


