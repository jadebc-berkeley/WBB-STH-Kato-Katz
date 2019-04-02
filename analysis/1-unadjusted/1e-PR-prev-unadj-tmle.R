##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH unadjusted analysis with TMLE

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
##############################################
rm(list=ls())
source(here::here("0-config.R"))

#----------------------------------------------
# load and pre-process analysis dataset 
#----------------------------------------------
data = read.csv(sth_data_path)

d=preprocess.sth(data)

# subset to columns needed for unadjusted PR
df = d[,c("block","clusterid","tr","al","tt","hw","sth")]
df$block=as.factor(df$block)

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.al.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$al,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c("Control",x),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$hw,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c("Control",x),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$tt,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c("Control",x),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$sth,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c("Control",x),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h1_unadj_tmle_j=format.tmle(est.al.h1,family="binomial")$rr
al_rd_h1_unadj_tmle_j=format.tmle(est.al.h1,family="binomial")$rd

hw_rr_h1_unadj_tmle_j=format.tmle(est.hw.h1,family="binomial")$rr
hw_rd_h1_unadj_tmle_j=format.tmle(est.hw.h1,family="binomial")$rd

tt_rr_h1_unadj_tmle_j=format.tmle(est.tt.h1,family="binomial")$rr
tt_rd_h1_unadj_tmle_j=format.tmle(est.tt.h1,family="binomial")$rd

sth_rr_h1_unadj_tmle_j=format.tmle(est.sth.h1,family="binomial")$rr
sth_rd_h1_unadj_tmle_j=format.tmle(est.sth.h1,family="binomial")$rd

rownames(al_rr_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rr_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rr_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rr_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(al_rd_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                               "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(hw_rd_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(tt_rd_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
rownames(sth_rd_h1_unadj_tmle_j)=c("Water vs C","Sanitation vs C","Handwashing vs C",
                             "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, P-value
#----------------------------------------------

trlist=c("Water","Sanitation","Handwashing")

est.al.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$al,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$hw,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$tt,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$sth,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h2_unadj_tmle_j=format.tmle(est.al.h2,family="binomial")$rr
al_rd_h2_unadj_tmle_j=format.tmle(est.al.h2,family="binomial")$rd

hw_rr_h2_unadj_tmle_j=format.tmle(est.hw.h2,family="binomial")$rr
hw_rd_h2_unadj_tmle_j=format.tmle(est.hw.h2,family="binomial")$rd

tt_rr_h2_unadj_tmle_j=format.tmle(est.tt.h2,family="binomial")$rr
tt_rd_h2_unadj_tmle_j=format.tmle(est.tt.h2,family="binomial")$rd

sth_rr_h2_unadj_tmle_j=format.tmle(est.sth.h2,family="binomial")$rr
sth_rd_h2_unadj_tmle_j=format.tmle(est.sth.h2,family="binomial")$rd

rownames(al_rr_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rr_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rr_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_rr_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

rownames(al_rd_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(hw_rd_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(tt_rd_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(sth_rd_h2_unadj_tmle_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")

#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSH Nvs. 
# single arms.  PR, CI, P-value
#----------------------------------------------
trlist=c("WSH","Nutrition")

est.al.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$al,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"Nutrition + WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.hw.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$hw,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"Nutrition + WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.tt.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$tt,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"Nutrition + WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

est.sth.h3=apply(matrix(trlist), 1,function(x) washb_tmle(Y=df$sth,
    tr=df$tr, pair=df$block, id=df$block,family="binomial",contrast=c(x,"Nutrition + WSH"),
    Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=12345, print=TRUE))

al_rr_h3_unadj_tmle_j=format.tmle(est.al.h3,family="binomial")$rr
al_rd_h3_unadj_tmle_j=format.tmle(est.al.h3,family="binomial")$rd

hw_rr_h3_unadj_tmle_j=format.tmle(est.hw.h3,family="binomial")$rr
hw_rd_h3_unadj_tmle_j=format.tmle(est.hw.h3,family="binomial")$rd

tt_rr_h3_unadj_tmle_j=format.tmle(est.tt.h3,family="binomial")$rr
tt_rd_h3_unadj_tmle_j=format.tmle(est.tt.h3,family="binomial")$rd

sth_rr_h3_unadj_tmle_j=format.tmle(est.sth.h3,family="binomial")$rr
sth_rd_h3_unadj_tmle_j=format.tmle(est.sth.h3,family="binomial")$rd

rownames(al_rr_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rr_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rr_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_rr_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

rownames(al_rd_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(hw_rd_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(tt_rd_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")
rownames(sth_rd_h3_unadj_tmle_j)=c("Nutrition + WSH vs WSH","Nutrition + WSH vs Nutrition")

#----------------------------------------------
# save objects
#----------------------------------------------

save(al_rr_h1_unadj_tmle_j,hw_rr_h1_unadj_tmle_j,tt_rr_h1_unadj_tmle_j,sth_rr_h1_unadj_tmle_j,
     al_rd_h1_unadj_tmle_j,hw_rd_h1_unadj_tmle_j,tt_rd_h1_unadj_tmle_j,sth_rd_h1_unadj_tmle_j,

     al_rr_h2_unadj_tmle_j,hw_rr_h2_unadj_tmle_j,tt_rr_h2_unadj_tmle_j,sth_rr_h2_unadj_tmle_j,
     al_rd_h2_unadj_tmle_j,hw_rd_h2_unadj_tmle_j,tt_rd_h2_unadj_tmle_j,sth_rd_h2_unadj_tmle_j,

     al_rr_h3_unadj_tmle_j,hw_rr_h3_unadj_tmle_j,tt_rr_h3_unadj_tmle_j,sth_rr_h3_unadj_tmle_j,
     al_rd_h3_unadj_tmle_j,hw_rd_h3_unadj_tmle_j,tt_rd_h3_unadj_tmle_j,sth_rd_h3_unadj_tmle_j,
     
     file=paste0(save_data_path, "sth_pr_unadj_tmle.RData"))

