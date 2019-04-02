##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH unadjusted analysis

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

# Poisson regression for RRs
al_rr_h1_unadj_j=t(apply(matrix(trlist), 1,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

hw_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

tt_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

sth_rr_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RR")))

# Linear regression for RDs
al_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

hw_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

tt_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))

sth_rd_h1_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c("Control",x),measure="RD")))


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
al_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

hw_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

tt_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

sth_rr_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RR")))

# Linear regression for RDs
al_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

hw_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

tt_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

sth_rd_h2_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c(x,"WSH"),measure="RD")))

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
al_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

hw_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

tt_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

sth_rr_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RR")))

# Linear regression for RDs
al_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$al,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

hw_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$hw,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

tt_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$tt,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

sth_rd_h3_unadj_j=t(apply(matrix(trlist),1 ,function(x) washb_mh(Y=df$sth,tr=df$tr,strat=df$block,
     contrast=c(x,"Nutrition + WSH"),measure="RD")))

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
     
     file=paste0(save_data_path, "sth_pr_unadj.RData"))

