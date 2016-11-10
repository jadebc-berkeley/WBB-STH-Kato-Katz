##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis
# Moderate heavy infection

# by Jade
##############################################
library(devtools)
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv")
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)

d$block=as.factor(d$block)

W=c("month","hfias","aged","sex","momage","momheight","momedu",
    "Nlt18","Ncomp","watmin","roof","walls","floor",
    "elec","asset_wardrobe","n_asset_table","n_asset_chair","asset_khat","asset_chouki",
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

dW=d[,c("block","tr","clusterid","sthmh","almh","hwmh","ttmh",W)]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

# Poisson regression for RRs
glm.bin.al.h1=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.hw.h1=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h1=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h1=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h1=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h1=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h1=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h1=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

al_mh_rr_h1_adj_j=glm.bin.al.h1[[1]]$TR
hw_mh_rr_h1_adj_j=glm.bin.hw.h1[[1]]$TR
tt_mh_rr_h1_adj_j=glm.bin.tt.h1[[1]]$TR
sth_mh_rr_h1_adj_j=glm.bin.sth.h1[[1]]$TR
for(i in 2:6){
  al_mh_rr_h1_adj_j=rbind(al_mh_rr_h1_adj_j,glm.bin.al.h1[[i]]$TR)
  hw_mh_rr_h1_adj_j=rbind(hw_mh_rr_h1_adj_j,glm.bin.hw.h1[[i]]$TR)
  tt_mh_rr_h1_adj_j=rbind(tt_mh_rr_h1_adj_j,glm.bin.tt.h1[[i]]$TR)
  sth_mh_rr_h1_adj_j=rbind(sth_mh_rr_h1_adj_j,glm.bin.sth.h1[[i]]$TR)
}

al_mh_rd_h1_adj_j=glm.gau.al.h1[[1]]$TR
hw_mh_rd_h1_adj_j=glm.gau.hw.h1[[1]]$TR
tt_mh_rd_h1_adj_j=glm.gau.tt.h1[[1]]$TR
sth_mh_rd_h1_adj_j=glm.gau.sth.h1[[1]]$TR
for(i in 2:6){
  al_mh_rd_h1_adj_j=rbind(al_mh_rd_h1_adj_j,glm.gau.al.h1[[i]]$TR)
  hw_mh_rd_h1_adj_j=rbind(hw_mh_rd_h1_adj_j,glm.gau.hw.h1[[i]]$TR)
  tt_mh_rd_h1_adj_j=rbind(tt_mh_rd_h1_adj_j,glm.gau.tt.h1[[i]]$TR)
  sth_mh_rd_h1_adj_j=rbind(sth_mh_rd_h1_adj_j,glm.gau.sth.h1[[i]]$TR)
}

rownames(al_mh_rr_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rr_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rr_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rr_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_mh_rd_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_mh_rd_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_mh_rd_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_mh_rd_h1_adj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

# Poisson regression for RRs
glm.bin.al.h2=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.hw.h2=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h2=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h2=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h2=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h2=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h2=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h2=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

al_mh_rr_h2_adj_j=glm.bin.al.h2[[1]]$TR
hw_mh_rr_h2_adj_j=glm.bin.hw.h2[[1]]$TR
tt_mh_rr_h2_adj_j=glm.bin.tt.h2[[1]]$TR
sth_mh_rr_h2_adj_j=glm.bin.sth.h2[[1]]$TR
for(i in 2:3){
  al_mh_rr_h2_adj_j=rbind(al_mh_rr_h2_adj_j,glm.bin.al.h2[[i]]$TR)
  hw_mh_rr_h2_adj_j=rbind(hw_mh_rr_h2_adj_j,glm.bin.hw.h2[[i]]$TR)
  tt_mh_rr_h2_adj_j=rbind(tt_mh_rr_h2_adj_j,glm.bin.tt.h2[[i]]$TR)
  sth_mh_rr_h2_adj_j=rbind(sth_mh_rr_h2_adj_j,glm.bin.sth.h2[[i]]$TR)
}

al_mh_rd_h2_adj_j=glm.gau.al.h2[[1]]$TR
hw_mh_rd_h2_adj_j=glm.gau.hw.h2[[1]]$TR
tt_mh_rd_h2_adj_j=glm.gau.tt.h2[[1]]$TR
sth_mh_rd_h2_adj_j=glm.gau.sth.h2[[1]]$TR
for(i in 2:3){
  al_mh_rd_h2_adj_j=rbind(al_mh_rd_h2_adj_j,glm.gau.al.h2[[i]]$TR)
  hw_mh_rd_h2_adj_j=rbind(hw_mh_rd_h2_adj_j,glm.gau.hw.h2[[i]]$TR)
  tt_mh_rd_h2_adj_j=rbind(tt_mh_rd_h2_adj_j,glm.gau.tt.h2[[i]]$TR)
  sth_mh_rd_h2_adj_j=rbind(sth_mh_rd_h2_adj_j,glm.gau.sth.h2[[i]]$TR)
}

rownames(al_mh_rr_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rr_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rr_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rr_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_mh_rd_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_mh_rd_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_mh_rd_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_mh_rd_h2_adj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

# Poisson regression for RRs
glm.bin.al.h3=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.hw.h3=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h3=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h3=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h3=lapply(trlist ,function(x) washb_glm(Y=dW$almh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h3=lapply(trlist ,function(x) washb_glm(Y=dW$hwmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h3=lapply(trlist ,function(x) washb_glm(Y=dW$ttmh,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h3=lapply(trlist ,function(x) washb_glm(Y=dW$sth,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))


al_mh_rr_h3_adj_j=rbind(glm.bin.al.h3[[1]]$TR,glm.bin.al.h3[[2]]$TR)
hw_mh_rr_h3_adj_j=rbind(glm.bin.hw.h3[[1]]$TR,glm.bin.hw.h3[[2]]$TR)
tt_mh_rr_h3_adj_j=rbind(glm.bin.tt.h3[[1]]$TR,glm.bin.tt.h3[[2]]$TR)
sth_mh_rr_h3_adj_j=rbind(glm.bin.sth.h3[[1]]$TR,glm.bin.sth.h3[[2]]$TR)

al_mh_rd_h3_adj_j=rbind(glm.gau.al.h3[[1]]$TR,glm.gau.al.h3[[2]]$TR)
hw_mh_rd_h3_adj_j=rbind(glm.gau.hw.h3[[1]]$TR,glm.gau.hw.h3[[2]]$TR)
tt_mh_rd_h3_adj_j=rbind(glm.gau.tt.h3[[1]]$TR,glm.gau.tt.h3[[2]]$TR)
sth_mh_rd_h3_adj_j=rbind(glm.gau.sth.h3[[1]]$TR,glm.gau.sth.h3[[2]]$TR)

rownames(al_mh_rr_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rr_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rr_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rr_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_mh_rd_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_mh_rd_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_mh_rd_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_mh_rd_h3_adj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_mh_rr_h1_adj_j,hw_mh_rr_h1_adj_j,tt_mh_rr_h1_adj_j,sth_mh_rr_h1_adj_j,
     al_mh_rd_h1_adj_j,hw_mh_rd_h1_adj_j,tt_mh_rd_h1_adj_j,sth_mh_rd_h1_adj_j,

     al_mh_rr_h2_adj_j,hw_mh_rr_h2_adj_j,tt_mh_rr_h2_adj_j,sth_mh_rr_h2_adj_j,
     al_mh_rd_h2_adj_j,hw_mh_rd_h2_adj_j,tt_mh_rd_h2_adj_j,sth_mh_rd_h2_adj_j,

     al_mh_rr_h3_adj_j,hw_mh_rr_h3_adj_j,tt_mh_rr_h3_adj_j,sth_mh_rr_h3_adj_j,
     al_mh_rd_h3_adj_j,hw_mh_rd_h3_adj_j,tt_mh_rd_h3_adj_j,sth_mh_rd_h3_adj_j,
     
     file="~/Dropbox/WASHB Parasites/Results/Jade/sth_pr_mh_adj.RData")


