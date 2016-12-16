##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Eggs per gram

# Effect modification by index child

# by Jade
##############################################
library(devtools)
library(washb)

rm(list=ls())
data=read.csv("~/Box Sync/WASHB Parasites/Analysis datasets/Jade/sth.csv",stringsAsFactors=TRUE)
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)
d=preprocess.adj.sth(d)

d1=d[d$index==1,]
d0=d[d$index==0,]

# roof and landphone excluded due to low prevalence

W=c("counter","birthord","month","hfiacat","aged","sex","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","walls","floor",
    "elec","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW1=d1[,c("block","tr","clusterid","alepg","hwepg","ttepg",W)]
dW0=d0[,c("block","tr","clusterid","alepg","hwepg","ttepg",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
# Index child
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1.i1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.i1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.i1.ari=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_h1_ari_i1_j=format.epg.tmle(est.al.h1.i1.ari)
hw_fecr_h1_ari_i1_j=format.epg.tmle(est.hw.h1.i1.ari)
tt_fecr_h1_ari_i1_j=format.epg.tmle(est.tt.h1.i1.ari)

rownames(al_rr_h1_ari_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_ari_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_ari_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

est.al.h1.i1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.i1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.i1.geo=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="geometric",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_fecr_h1_geo_i1_j=format.epg.tmle(est.al.h1.i1.geo)
hw_fecr_h1_geo_i1_j=format.epg.tmle(est.hw.h1.i1.geo)
tt_fecr_h1_geo_i1_j=format.epg.tmle(est.tt.h1.i1.geo)

rownames(al_rd_h1_geo_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_geo_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_geo_i1_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")  

# Not index child
est.al.h1.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$alepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$ttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_i0_j=format.tmle(est.al.h1.i0,family="gaussian")$rr
al_rd_h1_i0_j=format.tmle(est.al.h1.i0,family="gaussian")$rd

hw_rr_h1_i0_j=format.tmle(est.hw.h1.i0,family="gaussian")$rr
hw_rd_h1_i0_j=format.tmle(est.hw.h1.i0,family="gaussian")$rd

tt_rr_h1_i0_j=format.tmle(est.tt.h1.i0,family="gaussian")$rr
tt_rd_h1_i0_j=format.tmle(est.tt.h1.i0,family="gaussian")$rd

rownames(al_rr_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_i0_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

# index child
est.al.h2.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h2_i1_j=format.tmle(est.al.h2.i1,family="gaussian")$rr
al_rd_h2_i1_j=format.tmle(est.al.h2.i1,family="gaussian")$rd

hw_rr_h2_i1_j=format.tmle(est.hw.h2.i1,family="gaussian")$rr
hw_rd_h2_i1_j=format.tmle(est.hw.h2.i1,family="gaussian")$rd

tt_rr_h2_i1_j=format.tmle(est.tt.h2.i1,family="gaussian")$rr
tt_rd_h2_i1_j=format.tmle(est.tt.h2.i1,family="gaussian")$rd

rownames(al_rr_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rr_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rr_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_rd_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rd_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rd_h2_i1_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")


# not index child
est.al.h2.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$alepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$ttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h2_i0_j=format.tmle(est.al.h2.i0,family="gaussian")$rr
al_rd_h2_i0_j=format.tmle(est.al.h2.i0,family="gaussian")$rd

hw_rr_h2_i0_j=format.tmle(est.hw.h2.i0,family="gaussian")$rr
hw_rd_h2_i0_j=format.tmle(est.hw.h2.i0,family="gaussian")$rd

tt_rr_h2_i0_j=format.tmle(est.tt.h2.i0,family="gaussian")$rr
tt_rd_h2_i0_j=format.tmle(est.tt.h2.i0,family="gaussian")$rd

rownames(al_rr_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rr_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rr_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_rd_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rd_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rd_h2_i0_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

est.al.h3.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$alepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$hwepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3.i1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW1$ttepg,tr=dW1$tr,
   pair=dW1$block, id=dW1$block,W=dW1[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h3_i1_j=format.tmle(est.al.h3.i1,family="gaussian")$rr
al_rd_h3_i1_j=format.tmle(est.al.h3.i1,family="gaussian")$rd

hw_rr_h3_i1_j=format.tmle(est.hw.h3.i1,family="gaussian")$rr
hw_rd_h3_i1_j=format.tmle(est.hw.h3.i1,family="gaussian")$rd

tt_rr_h3_i1_j=format.tmle(est.tt.h3.i1,family="gaussian")$rr
tt_rd_h3_i1_j=format.tmle(est.tt.h3.i1,family="gaussian")$rd

rownames(al_rr_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rr_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rr_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_rd_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rd_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rd_h3_i1_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

# not index child
est.al.h3.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$alepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$hwepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3.i0=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW0$ttepg,tr=dW0$tr,
   pair=dW0$block, id=dW0$block,W=dW0[,W], FECR="arithmetic",
   family="gaussian",contrast=c(x,"Nutrition + WSH"),Q.SL.library=SL.library,
   g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h3_i0_j=format.tmle(est.al.h3.i0,family="gaussian")$rr
al_rd_h3_i0_j=format.tmle(est.al.h3.i0,family="gaussian")$rd

hw_rr_h3_i0_j=format.tmle(est.hw.h3.i0,family="gaussian")$rr
hw_rd_h3_i0_j=format.tmle(est.hw.h3.i0,family="gaussian")$rd

tt_rr_h3_i0_j=format.tmle(est.tt.h3.i0,family="gaussian")$rr
tt_rd_h3_i0_j=format.tmle(est.tt.h3.i0,family="gaussian")$rd

rownames(al_rr_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rr_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rr_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_rd_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rd_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rd_h3_i0_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_i1_j,hw_rr_h1_i1_j,tt_rr_h1_i1_j,
     al_rd_h1_i1_j,hw_rd_h1_i1_j,tt_rd_h1_i1_j,

     al_rr_h2_i1_j,hw_rr_h2_i1_j,tt_rr_h2_i1_j,
     al_rd_h2_i1_j,hw_rd_h2_i1_j,tt_rd_h2_i1_j,

     al_rr_h3_i1_j,hw_rr_h3_i1_j,tt_rr_h3_i1_j,
     al_rd_h3_i1_j,hw_rd_h3_i1_j,tt_rd_h3_i1_j,

     al_rr_h1_i0_j,hw_rr_h1_i0_j,tt_rr_h1_i0_j,
     al_rd_h1_i0_j,hw_rd_h1_i0_j,tt_rd_h1_i0_j,

     al_rr_h2_i0_j,hw_rr_h2_i0_j,tt_rr_h2_i0_j,
     al_rd_h2_i0_j,hw_rd_h2_i0_j,tt_rd_h2_i0_j,

     al_rr_h3_i0_j,hw_rr_h3_i0_j,tt_rr_h3_i0_j,
     al_rd_h3_i0_j,hw_rd_h3_i0_j,tt_rd_h3_i0_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_index.RData")


