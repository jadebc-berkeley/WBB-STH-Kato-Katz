##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Generate STH supplementary tables

# by Jade
##############################################
rm(list=ls())

source(here::here("0-config.R"))

#---------------------------------------------
# Primary analysis
#---------------------------------------------
load(paste0(save_data_path,"sth_prev.RData"))
load(paste0(save_data_path,"sth_pr_unadj_tmle.RData"))
load(paste0(save_data_path,"sth_pr_unadj.RData"))
load(paste0(save_data_path,"sth_pr_adj.RData"))
load(paste0(save_data_path,"sth_mh_pr_unadj.RData"))
load(paste0(save_data_path,"sth_pr_mh_adj.RData"))
load(paste0(save_data_path,"sth_pr_ipcw.RData"))
load(paste0(save_data_path,"sth_pr_mh_ipcw.RData"))
load(paste0(save_data_path,"sth_pr_epg_unadj.RData"))
load(paste0(save_data_path,"sth_pr_epg_adj.RData"))
load(paste0(save_data_path,"sth_pr_epg_ipcw.RData"))

#--------------------------------------------------------------
# Table S2: Infection prevalence, all interventions vs. control
#--------------------------------------------------------------
bin.h1.table=make.h1.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                                  al_rr_h1_adj_j,hw_rr_h1_adj_j,tt_rr_h1_adj_j,sth_rr_h1_adj_j,
                                  al_rr_h1_ipcw_j,hw_rr_h1_ipcw_j,tt_rr_h1_ipcw_j,sth_rr_h1_ipcw_j,
                                  al_rd_h1_unadj_tmle_a,hw_rd_h1_unadj_tmle_a,tt_rd_h1_unadj_tmle_a,sth_rd_h1_unadj_tmle_a,
                                  al_rd_h1_adj_j,hw_rd_h1_adj_j,tt_rd_h1_adj_j,sth_rd_h1_adj_j,
                                  al_rd_h1_ipcw_j,hw_rd_h1_ipcw_j,tt_rd_h1_ipcw_j,sth_rd_h1_ipcw_j,decimals=2)

write.csv(bin.h1.table, file = paste0(table_path, "table-s2.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S3: Infection prevalence, combined vs. individual WSH interventions
#--------------------------------------------------------------
bin.h2.table=make.h2.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h2_unadj_tmle_a,hw_rr_h2_unadj_tmle_a,tt_rr_h2_unadj_tmle_a,sth_rr_h2_unadj_tmle_a,
                                  al_rr_h2_adj_j,hw_rr_h2_adj_j,tt_rr_h2_adj_j,sth_rr_h2_adj_j,
                                  al_rr_h2_ipcw_j,hw_rr_h2_ipcw_j,tt_rr_h2_ipcw_j,sth_rr_h2_ipcw_j,
                                  al_rd_h2_unadj_tmle_a,hw_rd_h2_unadj_tmle_a,tt_rd_h2_unadj_tmle_a,sth_rd_h2_unadj_tmle_a,
                                  al_rd_h2_adj_j,hw_rd_h2_adj_j,tt_rd_h2_adj_j,sth_rd_h2_adj_j,
                                  al_rd_h2_ipcw_j,hw_rd_h2_ipcw_j,tt_rd_h2_ipcw_j,sth_rd_h2_ipcw_j,decimals=2)

write.csv(bin.h2.table, file = paste0(table_path, "table-s3.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S4: Infection prevalence, combined nutrition plus WSH vs. WSH and nutrition interventions
#--------------------------------------------------------------
bin.h3.table=make.h3.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h3_unadj_tmle_a,hw_rr_h3_unadj_tmle_a,tt_rr_h3_unadj_tmle_a,sth_rr_h3_unadj_tmle_a,
                                  al_rr_h3_adj_j,hw_rr_h3_adj_j,tt_rr_h3_adj_j,sth_rr_h3_adj_j,
                                  al_rr_h3_ipcw_j,hw_rr_h3_ipcw_j,tt_rr_h3_ipcw_j,sth_rr_h3_ipcw_j,
                                  al_rd_h3_unadj_tmle_a,hw_rd_h3_unadj_tmle_a,tt_rd_h3_unadj_tmle_a,sth_rd_h3_unadj_tmle_a,
                                  al_rd_h3_adj_j,hw_rd_h3_adj_j,tt_rd_h3_adj_j,sth_rd_h3_adj_j,
                                  al_rd_h3_ipcw_j,hw_rd_h3_ipcw_j,tt_rd_h3_ipcw_j,sth_rd_h3_ipcw_j,decimals=2)

write.csv(bin.h3.table, file = paste0(table_path, "table-s4.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S5: Moderate/heavy infection prevalence, all interventions vs. control
#--------------------------------------------------------------
bin.mh.h1.table=make.h1.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h1_unadj_tmle_a,hw_mh_rr_h1_unadj_tmle_a,tt_mh_rr_h1_unadj_tmle_a,sth_mh_rr_h1_unadj_tmle_a,
                                     al_mh_rr_h1_adj_j,hw_mh_rr_h1_adj_j,tt_mh_rr_h1_adj_j,sth_mh_rr_h1_adj_j,
                                     al_mh_rr_h1_ipcw_j,hw_mh_rr_h1_ipcw_j,tt_mh_rr_h1_ipcw_j,sth_mh_rr_h1_ipcw_j,
                                     al_mh_rd_h1_unadj_tmle_a,hw_mh_rd_h1_unadj_tmle_a,tt_mh_rd_h1_unadj_tmle_a,sth_mh_rd_h1_unadj_tmle_a,
                                     al_mh_rd_h1_adj_j,hw_mh_rd_h1_adj_j,tt_mh_rd_h1_adj_j,sth_mh_rd_h1_adj_j,
                                     al_mh_rd_h1_ipcw_j,hw_mh_rd_h1_ipcw_j,tt_mh_rd_h1_ipcw_j,sth_mh_rd_h1_ipcw_j,decimals=2)

write.csv(bin.mh.h1.table, file = paste0(table_path, "table-s5.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S6: Moderate/heavy infection prevalence, combined vs. individual WSH interventions
#--------------------------------------------------------------
bin.mh.h2.table=make.h2.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h2_unadj_tmle_a,hw_mh_rr_h2_unadj_tmle_a,tt_mh_rr_h2_unadj_tmle_a,sth_mh_rr_h2_unadj_tmle_a,
                                     al_mh_rr_h2_adj_j,hw_mh_rr_h2_adj_j,tt_mh_rr_h2_adj_j,sth_mh_rr_h2_adj_j,
                                     al_mh_rr_h2_ipcw_j,hw_mh_rr_h2_ipcw_j,tt_mh_rr_h2_ipcw_j,sth_mh_rr_h2_ipcw_j,
                                     al_mh_rd_h2_unadj_tmle_a,hw_mh_rd_h2_unadj_tmle_a,tt_mh_rd_h2_unadj_tmle_a,sth_mh_rd_h2_unadj_tmle_a,
                                     al_mh_rd_h2_adj_j,hw_mh_rd_h2_adj_j,tt_mh_rd_h2_adj_j,sth_mh_rd_h2_adj_j,
                                     al_mh_rd_h2_ipcw_j,hw_mh_rd_h2_ipcw_j,tt_mh_rd_h2_ipcw_j,sth_mh_rd_h2_ipcw_j,decimals=2)

write.csv(bin.mh.h2.table, file = paste0(table_path, "table-s6.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S7: Moderate/heavy infection prevalence, combined nutrition plus WSH 
# vs. WSH and nutrition interventions
#--------------------------------------------------------------
bin.mh.h3.table=make.h3.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h3_unadj_tmle_a,hw_mh_rr_h3_unadj_tmle_a,tt_mh_rr_h3_unadj_tmle_a,sth_mh_rr_h3_unadj_tmle_a,
                                     al_mh_rr_h3_adj_j,hw_mh_rr_h3_adj_j,tt_mh_rr_h3_adj_j,sth_mh_rr_h3_adj_j,
                                     al_mh_rr_h3_ipcw_j,hw_mh_rr_h3_ipcw_j,tt_mh_rr_h3_ipcw_j,sth_mh_rr_h3_ipcw_j,
                                     al_mh_rd_h3_unadj_tmle_a,hw_mh_rd_h3_unadj_tmle_a,tt_mh_rd_h3_unadj_tmle_a,sth_mh_rd_h3_unadj_tmle_a,
                                     al_mh_rd_h3_adj_j,hw_mh_rd_h3_adj_j,tt_mh_rd_h3_adj_j,sth_mh_rd_h3_adj_j,
                                     al_mh_rd_h3_ipcw_j,hw_mh_rd_h3_ipcw_j,tt_mh_rd_h3_ipcw_j,sth_mh_rd_h3_ipcw_j,decimals=2)

write.csv(bin.mh.h3.table, file = paste0(table_path, "table-s7.csv"), row.names=FALSE)


#--------------------------------------------------------------
# Table S8:  Fecal egg count reduction, all interventions vs.  control
#--------------------------------------------------------------
epg.h1.table=make.h1.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h1_unadj_j,hw_fecr_ari_h1_unadj_j,tt_fecr_ari_h1_unadj_j,
                                   al_fecr_ari_h1_adj_j,hw_fecr_ari_h1_adj_j,tt_fecr_ari_h1_adj_j,
                                   al_fecr_ari_h1_ipcw_j,hw_fecr_ari_h1_ipcw_j,tt_fecr_ari_h1_ipcw_j,
                                   al_fecr_geo_h1_unadj_j,hw_fecr_geo_h1_unadj_j,tt_fecr_geo_h1_unadj_j,
                                   al_fecr_geo_h1_adj_j,hw_fecr_geo_h1_adj_j,tt_fecr_geo_h1_adj_j,
                                   al_fecr_geo_h1_ipcw_j,hw_fecr_geo_h1_ipcw_j,tt_fecr_geo_h1_ipcw_j,decimals=2)

write.csv(epg.h1.table, file = paste0(table_path, "table-s8.csv"), row.names=FALSE)

#--------------------------------------------------------------
# Table S9:  Fecal egg count reduction, combined vs.  individual WSH interventions
#--------------------------------------------------------------
epg.h2.table=make.h2.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h2_unadj_j,hw_fecr_ari_h2_unadj_j,tt_fecr_ari_h2_unadj_j,
                                   al_fecr_ari_h2_adj_j,hw_fecr_ari_h2_adj_j,tt_fecr_ari_h2_adj_j,
                                   al_fecr_ari_h2_ipcw_j,hw_fecr_ari_h2_ipcw_j,tt_fecr_ari_h2_ipcw_j,
                                   al_fecr_geo_h2_unadj_j,hw_fecr_geo_h2_unadj_j,tt_fecr_geo_h2_unadj_j,
                                   al_fecr_geo_h2_adj_j,hw_fecr_geo_h2_adj_j,tt_fecr_geo_h2_adj_j,
                                   al_fecr_geo_h2_ipcw_j,hw_fecr_geo_h2_ipcw_j,tt_fecr_geo_h2_ipcw_j,decimals=2)

write.csv(epg.h2.table, file = paste0(table_path, "table-s9.csv"), row.names=FALSE)


#--------------------------------------------------------------
# Table S10:  Fecal egg count reduction, combined nutrition plus WSH vs.  WSH and nutrition interventions
#--------------------------------------------------------------
epg.h3.table=make.h3.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h3_unadj_j,hw_fecr_ari_h3_unadj_j,tt_fecr_ari_h3_unadj_j,
                                   al_fecr_ari_h3_adj_j,hw_fecr_ari_h3_adj_j,tt_fecr_ari_h3_adj_j,
                                   al_fecr_ari_h3_ipcw_j,hw_fecr_ari_h3_ipcw_j,tt_fecr_ari_h3_ipcw_j,
                                   al_fecr_geo_h3_unadj_j,hw_fecr_geo_h3_unadj_j,tt_fecr_geo_h3_unadj_j,
                                   al_fecr_geo_h3_adj_j,hw_fecr_geo_h3_adj_j,tt_fecr_geo_h3_adj_j,
                                   al_fecr_geo_h3_ipcw_j,hw_fecr_geo_h3_ipcw_j,tt_fecr_geo_h3_ipcw_j,decimals=2)

write.csv(epg.h3.table, file = paste0(table_path, "table-s10.csv"), row.names=FALSE)
