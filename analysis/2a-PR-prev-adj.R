##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH adjusted analysis

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
    "asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile",
    "month","mother_age","motherht","Ncomp","u18")

dW=d[,c("sth","al","hw","tt","block","tr","clusterid",W)]

######################################################
# H1: Adjusted prevalence ratios; each arm vs. control
# fit with Poisson if log binomial fails to converge
######################################################
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

glm.bin.h1=lapply(trlist ,function(x) washb_glm(Y=dW$diarr7,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,W=dW[,W],contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.gau.h1=lapply(trlist ,function(x) washb_glm(Y=dW$diarr7,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,W=dW[,W],contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

diar_h1_pr_adj_j=glm.bin.h1[[1]]$TR
for(i in 2:7){
  diar_h1_pr_adj_j=rbind(diar_h1_pr_adj_j,glm.bin.h1[[i]]$TR)
}

diar_h1_rd_adj_j=glm.gau.h1[[1]]$TR
for(i in 2:7){
  diar_h1_rd_adj_j=rbind(diar_h1_rd_adj_j,glm.gau.h1[[i]]$TR)
}

rownames(diar_h1_pr_adj_j)=c("Passive Control vs. C", "Water vs. C",
                             "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
                             "Nutrition vs. C", "Nutrition + WSH vs. C")
rownames(diar_h1_rd_adj_j)=c("Passive Control vs. C", "Water vs. C",
                             "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
                             "Nutrition vs. C", "Nutrition + WSH vs. C")

######################################################
# H2: Adjusted prevalence ratios;
# Combined vs single
######################################################
trlist=c("Water","Sanitation","Handwashing")

glm.bin.h2=lapply(trlist ,function(x) washb_glm(Y=dW$diarr7,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,W=dW[,W],contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.gau.h2=lapply(trlist ,function(x) washb_glm(Y=dW$diarr7,tr=dW$tr,pair=dW$block,
     id=dW$clusterid,W=dW[,W],contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

diar_h2_pr_adj_j=glm.bin.h2[[1]]$TR
for(i in 2:3){
  diar_h2_pr_adj_j=rbind(diar_h2_pr_adj_j,glm.bin.h2[[i]]$TR)
}

diar_h2_rd_adj_j=glm.gau.h2[[1]]$TR
for(i in 2:3){
  diar_h2_rd_adj_j=rbind(diar_h2_rd_adj_j,glm.gau.h2[[i]]$TR)
}

rownames(diar_h2_pr_adj_j)=c("WSH vs. Water","WSH vs. Sanitation","WSH vs. Handwashing")
rownames(diar_h2_rd_adj_j)=c("WSH vs. Water","WSH vs. Sanitation","WSH vs. Handwashing")

######################################################
# Save results
######################################################
diar_h1_pr_adj_j
diar_h2_pr_adj_j
diar_h1_rd_adj_j
diar_h2_rd_adj_j

save(diar_h1_pr_adj_j,diar_h2_pr_adj_j, diar_h1_rd_adj_j, diar_h2_rd_adj_j,
     file="~/Dropbox/WBK-primary-analysis/Results/jade/diarr-PR-adj.RData")


