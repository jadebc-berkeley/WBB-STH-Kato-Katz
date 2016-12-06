##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

# by Jade
##############################################
library(devtools)
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv",stringsAsFactors=TRUE)
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)
d=preprocess.adj.sth(d)

# roof and landphone excluded due to low prevalence

W=c("counter","birthord","month","hfiacat","aged","sex","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW=d[,c("block","tr","clusterid","alepg","hwepg","ttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

# arithmetic means --------------------------
est.al.h1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h1_adj_j=format.epg.tmle(est.al.h1.ari)
hw_fecr_ari_h1_adj_j=format.epg.tmle(est.hw.h1.ari)
tt_fecr_ari_h1_adj_j=format.epg.tmle(est.tt.h1.ari)

rownames(al_fecr_ari_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_ari_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_ari_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

# geometric means --------------------------
est.al.h1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h1_adj_j=format.epg.tmle(est.al.h1.geo)
hw_fecr_geo_h1_adj_j=format.epg.tmle(est.hw.h1.geo)
tt_fecr_geo_h1_adj_j=format.epg.tmle(est.tt.h1.geo)

rownames(al_fecr_geo_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_fecr_geo_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_fecr_geo_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

# arithmetic means --------------------------
est.al.h2.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h2_adj_j=format.epg.tmle(est.al.h2.ari)
hw_fecr_ari_h2_adj_j=format.epg.tmle(est.hw.h2.ari)
tt_fecr_ari_h2_adj_j=format.epg.tmle(est.tt.h2.ari)

rownames(al_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

# geometric means --------------------------
est.al.h2.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h2_adj_j=format.epg.tmle(est.al.h2.geo)
hw_fecr_ari_h2_adj_j=format.epg.tmle(est.hw.h2.geo)
tt_fecr_ari_h2_adj_j=format.epg.tmle(est.tt.h2.geo)

rownames(al_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_fecr_ari_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

# arithmetic means --------------------------
est.al.h3.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_ari_h3_adj_j=format.epg.tmle(est.al.h3.ari)
hw_fecr_ari_h3_adj_j=format.epg.tmle(est.hw.h3.ari)
tt_fecr_ari_h3_adj_j=format.epg.tmle(est.tt.h3.ari)

rownames(al_fecr_ari_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_fecr_ari_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_fecr_ari_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

# geometric means --------------------------
est.al.h3.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$alepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$hwepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$ttepg,tr=dW$tr,
   pair=dW$block, id=dW$block,W=dW[,W],FECR="geometric",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_geo_h3_adj_j=format.epg.tmle(est.al.h3.geo)
hw_fecr_geo_h3_adj_j=format.epg.tmle(est.hw.h3.geo)
tt_fecr_geo_h3_adj_j=format.epg.tmle(est.tt.h3.geo)

rownames(al_fecr_geo_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_fecr_geo_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_fecr_geo_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_fecr_ari_h1_adj_j,hw_fecr_ari_h1_adj_j,tt_fecr_ari_h1_adj_j,
     al_fecr_ari_h2_adj_j,hw_fecr_ari_h2_adj_j,tt_fecr_ari_h2_adj_j,
     al_fecr_ari_h3_adj_j,hw_fecr_ari_h3_adj_j,tt_fecr_ari_h3_adj_j,

     al_fecr_geo_h1_adj_j,hw_fecr_geo_h1_adj_j,tt_fecr_geo_h1_adj_j,
     al_fecr_geo_h2_adj_j,hw_fecr_geo_h2_adj_j,tt_fecr_geo_h2_adj_j,
     al_fecr_geo_h3_adj_j,hw_fecr_geo_h3_adj_j,tt_fecr_geo_h3_adj_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj.RData")


