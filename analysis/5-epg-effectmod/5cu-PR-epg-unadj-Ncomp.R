##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

# Effect modification by number of compound 
# members <10 or >10 

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
# create separate datasets number of compound 
# members <10 or >10 
#----------------------------------------------
d1=d[d$ncompover10==1,]
d0=d[d$ncompover10==0,]

# roof and landphone excluded due to low prevalence

W=c("counter","birthord","month","hfiacat","aged","sex","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW1=d1[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]
dW0=d0[,c("block","tr","clusterid","alepg","hwepg","ttepg","logalepg","loghwepg","logttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
#----------------------------------------------
# N compound members >10
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.ind1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.ind1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.ind1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_unadj_ind1_j=format.epg.tmle(est.al.h1.ind1.ari)
hw_fecr_ari_h1_unadj_ind1_j=format.epg.tmle(est.hw.h1.ind1.ari)
tt_fecr_ari_h1_unadj_ind1_j=format.epg.tmle(est.tt.h1.ind1.ari)

rownames(al_fecr_ari_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.ind1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$logalepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.ind1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$loghwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.ind1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$logttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_unadj_ind1_j=format.epg.tmle(est.al.h1.ind1.geo)
hw_fecr_geo_h1_unadj_ind1_j=format.epg.tmle(est.hw.h1.ind1.geo)
tt_fecr_geo_h1_unadj_ind1_j=format.epg.tmle(est.tt.h1.ind1.geo)

rownames(al_fecr_geo_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_unadj_ind1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# N compound members < 10
#----------------------------------------------
est.al.h1.ind0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$alepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.ind0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.ind0.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$ttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_unadj_ind0_j=format.epg.tmle(est.al.h1.ind0.ari)
hw_fecr_ari_h1_unadj_ind0_j=format.epg.tmle(est.hw.h1.ind0.ari)
tt_fecr_ari_h1_unadj_ind0_j=format.epg.tmle(est.tt.h1.ind0.ari)

rownames(al_fecr_ari_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.ind0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$logalepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.ind0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$loghwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.ind0.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$logttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block, FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_unadj_ind0_j=format.epg.tmle(est.al.h1.ind0.geo)
hw_fecr_geo_h1_unadj_ind0_j=format.epg.tmle(est.hw.h1.ind0.geo)
tt_fecr_geo_h1_unadj_ind0_j=format.epg.tmle(est.tt.h1.ind0.geo)

rownames(al_fecr_geo_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_unadj_ind0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_fecr_ari_h1_unadj_ind1_j,hw_fecr_ari_h1_unadj_ind1_j,tt_fecr_ari_h1_unadj_ind1_j,
     al_fecr_geo_h1_unadj_ind1_j,hw_fecr_geo_h1_unadj_ind1_j,tt_fecr_geo_h1_unadj_ind1_j,

     al_fecr_ari_h1_unadj_ind0_j,hw_fecr_ari_h1_unadj_ind0_j,tt_fecr_ari_h1_unadj_ind0_j,
     al_fecr_geo_h1_unadj_ind0_j,hw_fecr_geo_h1_unadj_ind0_j,tt_fecr_geo_h1_unadj_ind0_j,
     
     file=paste0(save_data_path, "sth_pr_epg_unadj_Ncomp.RData"))

