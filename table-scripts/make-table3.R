##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Generate Manuscript table 3

# by Jade
##############################################
rm(list=ls())

source(here::here("0-config.R"))

#---------------------------------------------
# Primary analysis
#---------------------------------------------
load(paste0(save_data_path,"sth_prev.RData"))
load(paste0(save_data_path,"sth_prev_em.RData"))
load(paste0(save_data_path,"sth_pr_adj_index.RData"))
load(paste0(save_data_path,"sth_pr_epg_adj_index.RData"))
load(paste0(save_data_path,"sth_prev_sub_unadj_index.RData"))
load(paste0(save_data_path,"sth_prev_sub_unadj_ind.RData"))
load(paste0(save_data_path,"sth_prev_unadj_tmle.RData"))
load(paste0(save_data_path,"sth_em_ihh.RData"))
load(paste0(save_data_path,"sth_prev_sub_unadj_ihh.RData"))

# Change column name ihh objects
colnames(al_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(al_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")


# Index child and index household table
index.table=make.index.table.csv(al_prev,hw_prev,tt_prev,sth_prev,
                             al_em_prev_i1_j,hw_em_prev_i1_j,tt_em_prev_i1_j,sth_em_prev_i1_j, 
                             al_em_prev_ihh1_a,hw_em_prev_ihh1_a,tt_em_prev_ihh1_a,sth_em_prev_ihh1_a, 
                             al_em_prev_ihh0_a,hw_em_prev_ihh0_a,tt_em_prev_ihh0_a,sth_em_prev_ihh0_a, 
                             al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                             al_rr_h1_unadj_i1_a,hw_rr_h1_unadj_i1_a,tt_rr_h1_unadj_i1_a,sth_rr_h1_unadj_i1_a, 
                             al_rr_h1_unadj_ihh1_a,hw_rr_h1_unadj_ihh1_a,tt_rr_h1_unadj_ihh1_a,sth_rr_h1_unadj_ihh1_a, 
                             al_rr_h1_unadj_ihh0_a,hw_rr_h1_unadj_ihh0_a,tt_rr_h1_unadj_ihh0_a,sth_rr_h1_unadj_ihh0_a,decimals=2)

write.csv(index.table, file = paste0(table_path, "table3.csv"), row.names=FALSE)
