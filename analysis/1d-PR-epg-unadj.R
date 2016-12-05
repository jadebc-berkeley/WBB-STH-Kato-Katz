##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH unadjusted analysis

# by Jade
##############################################
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv")
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)
  
# subset to columns needed for unadjusted PR
df = d[,c("block","clusterid","tr","alepg","ttepg","hwepg")]
df$block=as.factor(df$block)

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

# Poisson regression for RRs
glm.bin.al.h1=lapply(trlist ,function(x) washb_glm(Y=df$alepg,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family=gaussian, pval=0.2, print=TRUE))


epg.al.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$alepg,tr=df$tr,
     pair=df$block, id=df$block,
     family="gaussian",contrast=c("Control",x),Q.SL.library=SL.library,
     g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

### START HERE

glm.bin.hw.h1=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h1=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h1=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h1=lapply(trlist ,function(x) washb_glm(Y=df$al,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h1=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h1=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h1=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c("Control",x),
     family="gaussian", pval=0.2, print=TRUE))

al_rr_h1_unadj_j=glm.bin.al.h1[[1]]$TR
hw_rr_h1_unadj_j=glm.bin.hw.h1[[1]]$TR
tt_rr_h1_unadj_j=glm.bin.tt.h1[[1]]$TR
sth_rr_h1_unadj_j=glm.bin.sth.h1[[1]]$TR
for(i in 2:6){
  al_rr_h1_unadj_j=rbind(al_rr_h1_unadj_j,glm.bin.al.h1[[i]]$TR)
  hw_rr_h1_unadj_j=rbind(hw_rr_h1_unadj_j,glm.bin.hw.h1[[i]]$TR)
  tt_rr_h1_unadj_j=rbind(tt_rr_h1_unadj_j,glm.bin.tt.h1[[i]]$TR)
  sth_rr_h1_unadj_j=rbind(sth_rr_h1_unadj_j,glm.bin.sth.h1[[i]]$TR)
}

al_rd_h1_unadj_j=glm.gau.al.h1[[1]]$TR
hw_rd_h1_unadj_j=glm.gau.hw.h1[[1]]$TR
tt_rd_h1_unadj_j=glm.gau.tt.h1[[1]]$TR
sth_rd_h1_unadj_j=glm.gau.sth.h1[[1]]$TR
for(i in 2:6){
  al_rd_h1_unadj_j=rbind(al_rd_h1_unadj_j,glm.gau.al.h1[[i]]$TR)
  hw_rd_h1_unadj_j=rbind(hw_rd_h1_unadj_j,glm.gau.hw.h1[[i]]$TR)
  tt_rd_h1_unadj_j=rbind(tt_rd_h1_unadj_j,glm.gau.tt.h1[[i]]$TR)
  sth_rd_h1_unadj_j=rbind(sth_rd_h1_unadj_j,glm.gau.sth.h1[[i]]$TR)
}

rownames(al_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------

trlist=c("Water","Sanitation","Handwashing")

# Poisson regression for RRs
glm.bin.al.h2=lapply(trlist ,function(x) washb_glm(Y=df$al,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.hw.h2=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h2=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h2=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h2=lapply(trlist ,function(x) washb_glm(Y=df$al,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h2=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h2=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h2=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"WSH"),
     family="gaussian", pval=0.2, print=TRUE))

al_rr_h2_unadj_j=glm.bin.al.h2[[1]]$TR
hw_rr_h2_unadj_j=glm.bin.hw.h2[[1]]$TR
tt_rr_h2_unadj_j=glm.bin.tt.h2[[1]]$TR
sth_rr_h2_unadj_j=glm.bin.sth.h2[[1]]$TR
for(i in 2:3){
  al_rr_h2_unadj_j=rbind(al_rr_h2_unadj_j,glm.bin.al.h2[[i]]$TR)
  hw_rr_h2_unadj_j=rbind(hw_rr_h2_unadj_j,glm.bin.hw.h2[[i]]$TR)
  tt_rr_h2_unadj_j=rbind(tt_rr_h2_unadj_j,glm.bin.tt.h2[[i]]$TR)
  sth_rr_h2_unadj_j=rbind(sth_rr_h2_unadj_j,glm.bin.sth.h2[[i]]$TR)
}

al_rd_h2_unadj_j=glm.gau.al.h2[[1]]$TR
hw_rd_h2_unadj_j=glm.gau.hw.h2[[1]]$TR
tt_rd_h2_unadj_j=glm.gau.tt.h2[[1]]$TR
sth_rd_h2_unadj_j=glm.gau.sth.h2[[1]]$TR
for(i in 2:3){
  al_rd_h2_unadj_j=rbind(al_rd_h2_unadj_j,glm.gau.al.h2[[i]]$TR)
  hw_rd_h2_unadj_j=rbind(hw_rd_h2_unadj_j,glm.gau.hw.h2[[i]]$TR)
  tt_rd_h2_unadj_j=rbind(tt_rd_h2_unadj_j,glm.gau.tt.h2[[i]]$TR)
  sth_rd_h2_unadj_j=rbind(sth_rd_h2_unadj_j,glm.gau.sth.h2[[i]]$TR)
}

rownames(al_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_rr_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_rd_h2_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

# Poisson regression for RRs
glm.bin.al.h3=lapply(trlist ,function(x) washb_glm(Y=df$al,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.hw.h3=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.tt.h3=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

glm.bin.sth.h3=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family=poisson(link='log'), pval=0.2, print=TRUE))

# Linear regression for RDs
glm.gau.al.h3=lapply(trlist ,function(x) washb_glm(Y=df$al,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.hw.h3=lapply(trlist ,function(x) washb_glm(Y=df$hw,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.tt.h3=lapply(trlist ,function(x) washb_glm(Y=df$tt,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))

glm.gau.sth.h3=lapply(trlist ,function(x) washb_glm(Y=df$sth,tr=df$tr,pair=df$block,
     id=df$clusterid,contrast=c(x,"Nutrition + WSH"),
     family="gaussian", pval=0.2, print=TRUE))


al_rr_h3_unadj_j=rbind(glm.bin.al.h3[[1]]$TR,glm.bin.al.h3[[2]]$TR)
hw_rr_h3_unadj_j=rbind(glm.bin.hw.h3[[1]]$TR,glm.bin.hw.h3[[2]]$TR)
tt_rr_h3_unadj_j=rbind(glm.bin.tt.h3[[1]]$TR,glm.bin.tt.h3[[2]]$TR)
sth_rr_h3_unadj_j=rbind(glm.bin.sth.h3[[1]]$TR,glm.bin.sth.h3[[2]]$TR)

al_rd_h3_unadj_j=rbind(glm.gau.al.h3[[1]]$TR,glm.gau.al.h3[[2]]$TR)
hw_rd_h3_unadj_j=rbind(glm.gau.hw.h3[[1]]$TR,glm.gau.hw.h3[[2]]$TR)
tt_rd_h3_unadj_j=rbind(glm.gau.tt.h3[[1]]$TR,glm.gau.tt.h3[[2]]$TR)
sth_rd_h3_unadj_j=rbind(glm.gau.sth.h3[[1]]$TR,glm.gau.sth.h3[[2]]$TR)

rownames(al_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_rr_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_rd_h3_unadj_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_unadj_j,hw_rr_h1_unadj_j,tt_rr_h1_unadj_j,sth_rr_h1_unadj_j,
     al_rd_h1_unadj_j,hw_rd_h1_unadj_j,tt_rd_h1_unadj_j,sth_rd_h1_unadj_j,

     al_rr_h2_unadj_j,hw_rr_h2_unadj_j,tt_rr_h2_unadj_j,sth_rr_h2_unadj_j,
     al_rd_h2_unadj_j,hw_rd_h2_unadj_j,tt_rd_h2_unadj_j,sth_rd_h2_unadj_j,

     al_rr_h3_unadj_j,hw_rr_h3_unadj_j,tt_rr_h3_unadj_j,sth_rr_h3_unadj_j,
     al_rd_h3_unadj_j,hw_rd_h3_unadj_j,tt_rd_h3_unadj_j,sth_rd_h3_unadj_j,
     
     file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_unadj.RData")

