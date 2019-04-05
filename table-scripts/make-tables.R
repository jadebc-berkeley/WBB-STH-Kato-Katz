##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Generate STH tables

# by Jade
##############################################
rm(list=ls())

source("~/Box Sync/WASHB Parasites/Scripts/Tables/0-base-table-functions.R")

#---------------------------------------------
# Primary analysis
#---------------------------------------------
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_prev.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_unadj_tmle.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_mh_pr_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_mh_adj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_ipcw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_mh_ipcw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_ipcw.RData")

load("~/Box Sync/WASHB Parasites/Results/Jade/sth_prev_em.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_psac.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_wealth.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_defday.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_Ncomp.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_hh.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_lat.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_latrine.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_scoop.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_noopendef.RData")

load("~/Box Sync/WASHB Parasites/Results/Jade/sth_mean_em.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_sac.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_wealth.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_defday.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_Ncomp.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_hh.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_lat.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_latrine.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_scoop.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_noopendef.RData")

load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_psac.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_poor.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_dw.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_def.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_ind.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_hmud.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_lmud.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_lat.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_fec.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_odf.RData")

load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_em_ihh.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_sub_unadj_ihh.RData")

# Change column name ihh objects
colnames(al_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(al_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_em_prev_ihh0_a)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_em_prev_ihh1_a)=c("N","Prev","SD","Robust SE","lb","ub")

colnames(al_em_geo_ihh0_a)=c("N","geomean","lb","ub")
colnames(al_em_geo_ihh1_a)=c("N","geomean","lb","ub")
colnames(hw_em_geo_ihh0_a)=c("N","geomean","lb","ub")
colnames(hw_em_geo_ihh1_a)=c("N","geomean","lb","ub")
colnames(tt_em_geo_ihh0_a)=c("N","geomean","lb","ub")
colnames(tt_em_geo_ihh1_a)=c("N","geomean","lb","ub")

# Prevalence ratio and difference
bin.h1.table=make.h1.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                                  al_rr_h1_adj_j,hw_rr_h1_adj_j,tt_rr_h1_adj_j,sth_rr_h1_adj_j,
                                  al_rr_h1_ipcw_j,hw_rr_h1_ipcw_j,tt_rr_h1_ipcw_j,sth_rr_h1_ipcw_j,
                                  al_rd_h1_unadj_tmle_a,hw_rd_h1_unadj_tmle_a,tt_rd_h1_unadj_tmle_a,sth_rd_h1_unadj_tmle_a,
                                  al_rd_h1_adj_j,hw_rd_h1_adj_j,tt_rd_h1_adj_j,sth_rd_h1_adj_j,
                                  al_rd_h1_ipcw_j,hw_rd_h1_ipcw_j,tt_rd_h1_ipcw_j,sth_rd_h1_ipcw_j,decimals=2)

bin.h2.table=make.h2.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h2_unadj_tmle_a,hw_rr_h2_unadj_tmle_a,tt_rr_h2_unadj_tmle_a,sth_rr_h2_unadj_tmle_a,
                                  al_rr_h2_adj_j,hw_rr_h2_adj_j,tt_rr_h2_adj_j,sth_rr_h2_adj_j,
                                  al_rr_h2_ipcw_j,hw_rr_h2_ipcw_j,tt_rr_h2_ipcw_j,sth_rr_h2_ipcw_j,
                                  al_rd_h2_unadj_tmle_a,hw_rd_h2_unadj_tmle_a,tt_rd_h2_unadj_tmle_a,sth_rd_h2_unadj_tmle_a,
                                  al_rd_h2_adj_j,hw_rd_h2_adj_j,tt_rd_h2_adj_j,sth_rd_h2_adj_j,
                                  al_rd_h2_ipcw_j,hw_rd_h2_ipcw_j,tt_rd_h2_ipcw_j,sth_rd_h2_ipcw_j,decimals=2)

bin.h3.table=make.h3.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
                                  al_rr_h3_unadj_tmle_a,hw_rr_h3_unadj_tmle_a,tt_rr_h3_unadj_tmle_a,sth_rr_h3_unadj_tmle_a,
                                  al_rr_h3_adj_j,hw_rr_h3_adj_j,tt_rr_h3_adj_j,sth_rr_h3_adj_j,
                                  al_rr_h3_ipcw_j,hw_rr_h3_ipcw_j,tt_rr_h3_ipcw_j,sth_rr_h3_ipcw_j,
                                  al_rd_h3_unadj_tmle_a,hw_rd_h3_unadj_tmle_a,tt_rd_h3_unadj_tmle_a,sth_rd_h3_unadj_tmle_a,
                                  al_rd_h3_adj_j,hw_rd_h3_adj_j,tt_rd_h3_adj_j,sth_rd_h3_adj_j,
                                  al_rd_h3_ipcw_j,hw_rd_h3_ipcw_j,tt_rd_h3_ipcw_j,sth_rd_h3_ipcw_j,decimals=2)

# Moderate/heavy prevalence ratio and difference
bin.mh.h1.table=make.h1.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h1_unadj_tmle_a,hw_mh_rr_h1_unadj_tmle_a,tt_mh_rr_h1_unadj_tmle_a,sth_mh_rr_h1_unadj_tmle_a,
                                     al_mh_rr_h1_adj_j,hw_mh_rr_h1_adj_j,tt_mh_rr_h1_adj_j,sth_mh_rr_h1_adj_j,
                                     al_mh_rr_h1_ipcw_j,hw_mh_rr_h1_ipcw_j,tt_mh_rr_h1_ipcw_j,sth_mh_rr_h1_ipcw_j,
                                     al_mh_rd_h1_unadj_tmle_a,hw_mh_rd_h1_unadj_tmle_a,tt_mh_rd_h1_unadj_tmle_a,sth_mh_rd_h1_unadj_tmle_a,
                                     al_mh_rd_h1_adj_j,hw_mh_rd_h1_adj_j,tt_mh_rd_h1_adj_j,sth_mh_rd_h1_adj_j,
                                     al_mh_rd_h1_ipcw_j,hw_mh_rd_h1_ipcw_j,tt_mh_rd_h1_ipcw_j,sth_mh_rd_h1_ipcw_j,decimals=2)

bin.mh.h2.table=make.h2.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h2_unadj_tmle_a,hw_mh_rr_h2_unadj_tmle_a,tt_mh_rr_h2_unadj_tmle_a,sth_mh_rr_h2_unadj_tmle_a,
                                     al_mh_rr_h2_adj_j,hw_mh_rr_h2_adj_j,tt_mh_rr_h2_adj_j,sth_mh_rr_h2_adj_j,
                                     al_mh_rr_h2_ipcw_j,hw_mh_rr_h2_ipcw_j,tt_mh_rr_h2_ipcw_j,sth_mh_rr_h2_ipcw_j,
                                     al_mh_rd_h2_unadj_tmle_a,hw_mh_rd_h2_unadj_tmle_a,tt_mh_rd_h2_unadj_tmle_a,sth_mh_rd_h2_unadj_tmle_a,
                                     al_mh_rd_h2_adj_j,hw_mh_rd_h2_adj_j,tt_mh_rd_h2_adj_j,sth_mh_rd_h2_adj_j,
                                     al_mh_rd_h2_ipcw_j,hw_mh_rd_h2_ipcw_j,tt_mh_rd_h2_ipcw_j,sth_mh_rd_h2_ipcw_j,decimals=2)

bin.mh.h3.table=make.h3.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
                                     al_mh_rr_h3_unadj_tmle_a,hw_mh_rr_h3_unadj_tmle_a,tt_mh_rr_h3_unadj_tmle_a,sth_mh_rr_h3_unadj_tmle_a,
                                     al_mh_rr_h3_adj_j,hw_mh_rr_h3_adj_j,tt_mh_rr_h3_adj_j,sth_mh_rr_h3_adj_j,
                                     al_mh_rr_h3_ipcw_j,hw_mh_rr_h3_ipcw_j,tt_mh_rr_h3_ipcw_j,sth_mh_rr_h3_ipcw_j,
                                     al_mh_rd_h3_unadj_tmle_a,hw_mh_rd_h3_unadj_tmle_a,tt_mh_rd_h3_unadj_tmle_a,sth_mh_rd_h3_unadj_tmle_a,
                                     al_mh_rd_h3_adj_j,hw_mh_rd_h3_adj_j,tt_mh_rd_h3_adj_j,sth_mh_rd_h3_adj_j,
                                     al_mh_rd_h3_ipcw_j,hw_mh_rd_h3_ipcw_j,tt_mh_rd_h3_ipcw_j,sth_mh_rd_h3_ipcw_j,decimals=2)

# Arithmetic and geometric FECR
epg.h1.table=make.h1.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h1_unadj_j,hw_fecr_ari_h1_unadj_j,tt_fecr_ari_h1_unadj_j,
                                   al_fecr_ari_h1_adj_j,hw_fecr_ari_h1_adj_j,tt_fecr_ari_h1_adj_j,
                                   al_fecr_ari_h1_ipcw_j,hw_fecr_ari_h1_ipcw_j,tt_fecr_ari_h1_ipcw_j,
                                   al_fecr_geo_h1_unadj_j,hw_fecr_geo_h1_unadj_j,tt_fecr_geo_h1_unadj_j,
                                   al_fecr_geo_h1_adj_j,hw_fecr_geo_h1_adj_j,tt_fecr_geo_h1_adj_j,
                                   al_fecr_geo_h1_ipcw_j,hw_fecr_geo_h1_ipcw_j,tt_fecr_geo_h1_ipcw_j,decimals=2)

epg.h2.table=make.h2.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h2_unadj_j,hw_fecr_ari_h2_unadj_j,tt_fecr_ari_h2_unadj_j,
                                   al_fecr_ari_h2_adj_j,hw_fecr_ari_h2_adj_j,tt_fecr_ari_h2_adj_j,
                                   al_fecr_ari_h2_ipcw_j,hw_fecr_ari_h2_ipcw_j,tt_fecr_ari_h2_ipcw_j,
                                   al_fecr_geo_h2_unadj_j,hw_fecr_geo_h2_unadj_j,tt_fecr_geo_h2_unadj_j,
                                   al_fecr_geo_h2_adj_j,hw_fecr_geo_h2_adj_j,tt_fecr_geo_h2_adj_j,
                                   al_fecr_geo_h2_ipcw_j,hw_fecr_geo_h2_ipcw_j,tt_fecr_geo_h2_ipcw_j,decimals=2)

epg.h3.table=make.h3.epg.table(al_int_mn,hw_int_mn,tt_int_mn,al_int_gmn,hw_int_gmn,tt_int_gmn,
                                   al_fecr_ari_h3_unadj_j,hw_fecr_ari_h3_unadj_j,tt_fecr_ari_h3_unadj_j,
                                   al_fecr_ari_h3_adj_j,hw_fecr_ari_h3_adj_j,tt_fecr_ari_h3_adj_j,
                                   al_fecr_ari_h3_ipcw_j,hw_fecr_ari_h3_ipcw_j,tt_fecr_ari_h3_ipcw_j,
                                   al_fecr_geo_h3_unadj_j,hw_fecr_geo_h3_unadj_j,tt_fecr_geo_h3_unadj_j,
                                   al_fecr_geo_h3_adj_j,hw_fecr_geo_h3_adj_j,tt_fecr_geo_h3_adj_j,
                                   al_fecr_geo_h3_ipcw_j,hw_fecr_geo_h3_ipcw_j,tt_fecr_geo_h3_ipcw_j,decimals=2)

# Binary EM N and prevalence
al.em.bin.table=make.em.bin.table(al_prev,al_rr_h1_unadj_tmle_a,
                                  al_em_prev_i0_j, al_em_prev_i1_j, al_rr_h1_unadj_i0_a, al_rr_h1_unadj_i1_a,
                                  al_em_prev_ihh0_a, al_em_prev_ihh1_a, al_rr_h1_unadj_ihh0_a, al_rr_h1_unadj_ihh1_a,
                                  al_em_prev_psac0_j, al_em_prev_psac1_j, al_rr_h1_unadj_psac0_a, al_rr_h1_unadj_psac1_a,
                                  al_em_prev_poor0_j, al_em_prev_poor1_j, al_rr_h1_unadj_poor0_a, al_rr_h1_unadj_poor1_a,
                                  al_em_prev_dw0_j, al_em_prev_dw1_j, al_rr_h1_unadj_dw0_a, al_rr_h1_unadj_dw1_a,
                                  al_em_prev_def0_j, al_em_prev_def1_j, al_rr_h1_unadj_def0_a, al_rr_h1_unadj_def1_a,
                                  al_em_prev_ind0_j, al_em_prev_ind1_j, al_rr_h1_unadj_ind0_a, al_rr_h1_unadj_ind1_a,
                                  al_em_prev_hmud0_j, al_em_prev_hmud1_j, al_rr_h1_unadj_hmud0_a, al_rr_h1_unadj_hmud1_a,
                                  al_em_prev_lmud0_j, al_em_prev_lmud1_j, al_rr_h1_unadj_lmud0_a, al_rr_h1_unadj_lmud1_a,
                                  al_em_prev_lat0_j, al_em_prev_lat1_j, al_rr_h1_unadj_lat0_a, al_rr_h1_unadj_lat1_a,
                                  al_em_prev_fec0_j, al_em_prev_fec1_j, al_rr_h1_unadj_fec0_a, al_rr_h1_unadj_fec1_a,
                                  al_em_prev_odf0_j, al_em_prev_odf1_j, al_rr_h1_unadj_odf0_a, al_rr_h1_unadj_odf1_a)


hw.em.bin.table=make.em.bin.table(hw_prev,hw_rr_h1_unadj_tmle_a,
                                  hw_em_prev_i0_j, hw_em_prev_i1_j, hw_rr_h1_unadj_i0_a, hw_rr_h1_unadj_i1_a,
                                  hw_em_prev_ihh0_a, hw_em_prev_ihh1_a, hw_rr_h1_unadj_ihh0_a, hw_rr_h1_unadj_ihh1_a,
                                  hw_em_prev_psac0_j, hw_em_prev_psac1_j, hw_rr_h1_unadj_psac0_a, hw_rr_h1_unadj_psac1_a,
                                  hw_em_prev_poor0_j, hw_em_prev_poor1_j, hw_rr_h1_unadj_poor0_a, hw_rr_h1_unadj_poor1_a,
                                  hw_em_prev_dw0_j, hw_em_prev_dw1_j, hw_rr_h1_unadj_dw0_a, hw_rr_h1_unadj_dw1_a,
                                  hw_em_prev_def0_j, hw_em_prev_def1_j, hw_rr_h1_unadj_def0_a, hw_rr_h1_unadj_def1_a,
                                  hw_em_prev_ind0_j, hw_em_prev_ind1_j, hw_rr_h1_unadj_ind0_a, hw_rr_h1_unadj_ind1_a,
                                  hw_em_prev_hmud0_j, hw_em_prev_hmud1_j, hw_rr_h1_unadj_hmud0_a, hw_rr_h1_unadj_hmud1_a,
                                  hw_em_prev_lmud0_j, hw_em_prev_lmud1_j, hw_rr_h1_unadj_lmud0_a, hw_rr_h1_unadj_lmud1_a,
                                  hw_em_prev_lat0_j, hw_em_prev_lat1_j, hw_rr_h1_unadj_lat0_a, hw_rr_h1_unadj_lat1_a,
                                  hw_em_prev_fec0_j, hw_em_prev_fec1_j, hw_rr_h1_unadj_fec0_a, hw_rr_h1_unadj_fec1_a,
                                  hw_em_prev_odf0_j, hw_em_prev_odf1_j, hw_rr_h1_unadj_odf0_a, hw_rr_h1_unadj_odf1_a)


tt.em.bin.table=make.em.bin.table(tt_prev,tt_rr_h1_unadj_tmle_a,
                                  tt_em_prev_i0_j, tt_em_prev_i1_j, tt_rr_h1_unadj_i0_a, tt_rr_h1_unadj_i1_a,
                                  tt_em_prev_ihh0_a, tt_em_prev_ihh1_a, tt_rr_h1_unadj_ihh0_a, tt_rr_h1_unadj_ihh1_a,
                                  tt_em_prev_psac0_j, tt_em_prev_psac1_j, tt_rr_h1_unadj_psac0_a, tt_rr_h1_unadj_psac1_a,
                                  tt_em_prev_poor0_j, tt_em_prev_poor1_j, tt_rr_h1_unadj_poor0_a, tt_rr_h1_unadj_poor1_a,
                                  tt_em_prev_dw0_j, tt_em_prev_dw1_j, tt_rr_h1_unadj_dw0_a, tt_rr_h1_unadj_dw1_a,
                                  tt_em_prev_def0_j, tt_em_prev_def1_j, tt_rr_h1_unadj_def0_a, tt_rr_h1_unadj_def1_a,
                                  tt_em_prev_ind0_j, tt_em_prev_ind1_j, tt_rr_h1_unadj_ind0_a, tt_rr_h1_unadj_ind1_a,
                                  tt_em_prev_hmud0_j, tt_em_prev_hmud1_j, tt_rr_h1_unadj_hmud0_a, tt_rr_h1_unadj_hmud1_a,
                                  tt_em_prev_lmud0_j, tt_em_prev_lmud1_j, tt_rr_h1_unadj_lmud0_a, tt_rr_h1_unadj_lmud1_a,
                                  tt_em_prev_lat0_j, tt_em_prev_lat1_j, tt_rr_h1_unadj_lat0_a, tt_rr_h1_unadj_lat1_a,
                                  tt_em_prev_fec0_j, tt_em_prev_fec1_j, tt_rr_h1_unadj_fec0_a, tt_rr_h1_unadj_fec1_a,
                                  tt_em_prev_odf0_j, tt_em_prev_odf1_j, tt_rr_h1_unadj_odf0_a, tt_rr_h1_unadj_odf1_a)


sth.em.bin.table=make.em.bin.table(sth_prev,sth_rr_h1_unadj_tmle_a,
                                  sth_em_prev_i0_j, sth_em_prev_i1_j, sth_rr_h1_unadj_i0_a, sth_rr_h1_unadj_i1_a,
                                  sth_em_prev_ihh0_a, sth_em_prev_ihh1_a, sth_rr_h1_unadj_ihh0_a, sth_rr_h1_unadj_ihh1_a,
                                  sth_em_prev_psac0_j, sth_em_prev_psac1_j, sth_rr_h1_unadj_psac0_a, sth_rr_h1_unadj_psac1_a,
                                  sth_em_prev_poor0_j, sth_em_prev_poor1_j, sth_rr_h1_unadj_poor0_a, sth_rr_h1_unadj_poor1_a,
                                  sth_em_prev_dw0_j, sth_em_prev_dw1_j, sth_rr_h1_unadj_dw0_a, sth_rr_h1_unadj_dw1_a,
                                  sth_em_prev_def0_j, sth_em_prev_def1_j, sth_rr_h1_unadj_def0_a, sth_rr_h1_unadj_def1_a,
                                  sth_em_prev_ind0_j, sth_em_prev_ind1_j, sth_rr_h1_unadj_ind0_a, sth_rr_h1_unadj_ind1_a,
                                  sth_em_prev_hmud0_j, sth_em_prev_hmud1_j, sth_rr_h1_unadj_hmud0_a, sth_rr_h1_unadj_hmud1_a,
                                  sth_em_prev_lmud0_j, sth_em_prev_lmud1_j, sth_rr_h1_unadj_lmud0_a, sth_rr_h1_unadj_lmud1_a,
                                  sth_em_prev_lat0_j, sth_em_prev_lat1_j, sth_rr_h1_unadj_lat0_a, sth_rr_h1_unadj_lat1_a,
                                  sth_em_prev_fec0_j, sth_em_prev_fec1_j, sth_rr_h1_unadj_fec0_a, sth_rr_h1_unadj_fec1_a,
                                  sth_em_prev_odf0_j, sth_em_prev_odf1_j, sth_rr_h1_unadj_odf0_a, sth_rr_h1_unadj_odf1_a)


# Binary EM prevalence ratio
al.sub.bin.table=make.sub.bin.table(al_prev,al_rr_h1_unadj_tmle_a,
                                  al_em_prev_i0_j, al_em_prev_i1_j, al_rr_h1_unadj_i0_a, al_rr_h1_unadj_i1_a,
                                  al_em_prev_ihh0_a, al_em_prev_ihh1_a, al_rr_h1_unadj_ihh0_a, al_rr_h1_unadj_ihh1_a,
                                  al_em_prev_psac0_j, al_em_prev_psac1_j, al_rr_h1_unadj_psac0_a, al_rr_h1_unadj_psac1_a,
                                  al_em_prev_poor0_j, al_em_prev_poor1_j, al_rr_h1_unadj_poor0_a, al_rr_h1_unadj_poor1_a,
                                  al_em_prev_dw0_j, al_em_prev_dw1_j, al_rr_h1_unadj_dw0_a, al_rr_h1_unadj_dw1_a,
                                  al_em_prev_def0_j, al_em_prev_def1_j, al_rr_h1_unadj_def0_a, al_rr_h1_unadj_def1_a,
                                  al_em_prev_ind0_j, al_em_prev_ind1_j, al_rr_h1_unadj_ind0_a, al_rr_h1_unadj_ind1_a,
                                  al_em_prev_hmud0_j, al_em_prev_hmud1_j, al_rr_h1_unadj_hmud0_a, al_rr_h1_unadj_hmud1_a,
                                  al_em_prev_lmud0_j, al_em_prev_lmud1_j, al_rr_h1_unadj_lmud0_a, al_rr_h1_unadj_lmud1_a,
                                  al_em_prev_lat0_j, al_em_prev_lat1_j, al_rr_h1_unadj_lat0_a, al_rr_h1_unadj_lat1_a,
                                  al_em_prev_fec0_j, al_em_prev_fec1_j, al_rr_h1_unadj_fec0_a, al_rr_h1_unadj_fec1_a,
                                  al_em_prev_odf0_j, al_em_prev_odf1_j, al_rr_h1_unadj_odf0_a, al_rr_h1_unadj_odf1_a)


hw.sub.bin.table=make.sub.bin.table(hw_prev,hw_rr_h1_unadj_tmle_a,
                                  hw_em_prev_i0_j, hw_em_prev_i1_j, hw_rr_h1_unadj_i0_a, hw_rr_h1_unadj_i1_a,
                                  hw_em_prev_ihh0_a, hw_em_prev_ihh1_a, hw_rr_h1_unadj_ihh0_a, hw_rr_h1_unadj_ihh1_a,
                                  hw_em_prev_psac0_j, hw_em_prev_psac1_j, hw_rr_h1_unadj_psac0_a, hw_rr_h1_unadj_psac1_a,
                                  hw_em_prev_poor0_j, hw_em_prev_poor1_j, hw_rr_h1_unadj_poor0_a, hw_rr_h1_unadj_poor1_a,
                                  hw_em_prev_dw0_j, hw_em_prev_dw1_j, hw_rr_h1_unadj_dw0_a, hw_rr_h1_unadj_dw1_a,
                                  hw_em_prev_def0_j, hw_em_prev_def1_j, hw_rr_h1_unadj_def0_a, hw_rr_h1_unadj_def1_a,
                                  hw_em_prev_ind0_j, hw_em_prev_ind1_j, hw_rr_h1_unadj_ind0_a, hw_rr_h1_unadj_ind1_a,
                                  hw_em_prev_hmud0_j, hw_em_prev_hmud1_j, hw_rr_h1_unadj_hmud0_a, hw_rr_h1_unadj_hmud1_a,
                                  hw_em_prev_lmud0_j, hw_em_prev_lmud1_j, hw_rr_h1_unadj_lmud0_a, hw_rr_h1_unadj_lmud1_a,
                                  hw_em_prev_lat0_j, hw_em_prev_lat1_j, hw_rr_h1_unadj_lat0_a, hw_rr_h1_unadj_lat1_a,
                                  hw_em_prev_fec0_j, hw_em_prev_fec1_j, hw_rr_h1_unadj_fec0_a, hw_rr_h1_unadj_fec1_a,
                                  hw_em_prev_odf0_j, hw_em_prev_odf1_j, hw_rr_h1_unadj_odf0_a, hw_rr_h1_unadj_odf1_a)


tt.sub.bin.table=make.sub.bin.table(tt_prev,tt_rr_h1_unadj_tmle_a,
                                  tt_em_prev_i0_j, tt_em_prev_i1_j, tt_rr_h1_unadj_i0_a, tt_rr_h1_unadj_i1_a,
                                  tt_em_prev_ihh0_a, tt_em_prev_ihh1_a, tt_rr_h1_unadj_ihh0_a, tt_rr_h1_unadj_ihh1_a,
                                  tt_em_prev_psac0_j, tt_em_prev_psac1_j, tt_rr_h1_unadj_psac0_a, tt_rr_h1_unadj_psac1_a,
                                  tt_em_prev_poor0_j, tt_em_prev_poor1_j, tt_rr_h1_unadj_poor0_a, tt_rr_h1_unadj_poor1_a,
                                  tt_em_prev_dw0_j, tt_em_prev_dw1_j, tt_rr_h1_unadj_dw0_a, tt_rr_h1_unadj_dw1_a,
                                  tt_em_prev_def0_j, tt_em_prev_def1_j, tt_rr_h1_unadj_def0_a, tt_rr_h1_unadj_def1_a,
                                  tt_em_prev_ind0_j, tt_em_prev_ind1_j, tt_rr_h1_unadj_ind0_a, tt_rr_h1_unadj_ind1_a,
                                  tt_em_prev_hmud0_j, tt_em_prev_hmud1_j, tt_rr_h1_unadj_hmud0_a, tt_rr_h1_unadj_hmud1_a,
                                  tt_em_prev_lmud0_j, tt_em_prev_lmud1_j, tt_rr_h1_unadj_lmud0_a, tt_rr_h1_unadj_lmud1_a,
                                  tt_em_prev_lat0_j, tt_em_prev_lat1_j, tt_rr_h1_unadj_lat0_a, tt_rr_h1_unadj_lat1_a,
                                  tt_em_prev_fec0_j, tt_em_prev_fec1_j, tt_rr_h1_unadj_fec0_a, tt_rr_h1_unadj_fec1_a,
                                  tt_em_prev_odf0_j, tt_em_prev_odf1_j, tt_rr_h1_unadj_odf0_a, tt_rr_h1_unadj_odf1_a)


sth.sub.bin.table=make.sub.bin.table(sth_prev,sth_rr_h1_unadj_tmle_a,
                                  sth_em_prev_i0_j, sth_em_prev_i1_j, sth_rr_h1_unadj_i0_a, sth_rr_h1_unadj_i1_a,
                                  sth_em_prev_ihh0_a, sth_em_prev_ihh1_a, sth_rr_h1_unadj_ihh0_a, sth_rr_h1_unadj_ihh1_a,
                                  sth_em_prev_psac0_j, sth_em_prev_psac1_j, sth_rr_h1_unadj_psac0_a, sth_rr_h1_unadj_psac1_a,
                                  sth_em_prev_poor0_j, sth_em_prev_poor1_j, sth_rr_h1_unadj_poor0_a, sth_rr_h1_unadj_poor1_a,
                                  sth_em_prev_dw0_j, sth_em_prev_dw1_j, sth_rr_h1_unadj_dw0_a, sth_rr_h1_unadj_dw1_a,
                                  sth_em_prev_def0_j, sth_em_prev_def1_j, sth_rr_h1_unadj_def0_a, sth_rr_h1_unadj_def1_a,
                                  sth_em_prev_ind0_j, sth_em_prev_ind1_j, sth_rr_h1_unadj_ind0_a, sth_rr_h1_unadj_ind1_a,
                                  sth_em_prev_hmud0_j, sth_em_prev_hmud1_j, sth_rr_h1_unadj_hmud0_a, sth_rr_h1_unadj_hmud1_a,
                                  sth_em_prev_lmud0_j, sth_em_prev_lmud1_j, sth_rr_h1_unadj_lmud0_a, sth_rr_h1_unadj_lmud1_a,
                                  sth_em_prev_lat0_j, sth_em_prev_lat1_j, sth_rr_h1_unadj_lat0_a, sth_rr_h1_unadj_lat1_a,
                                  sth_em_prev_fec0_j, sth_em_prev_fec1_j, sth_rr_h1_unadj_fec0_a, sth_rr_h1_unadj_fec1_a,
                                  sth_em_prev_odf0_j, sth_em_prev_odf1_j, sth_rr_h1_unadj_odf0_a, sth_rr_h1_unadj_odf1_a)


# EPG EM N and geomean
al.em.epg.table=make.em.epg.table(al_int_mn,al_int_gmn,al_fecr_geo_h1_unadj_j,
                                  al_em_geo_i0_j,al_em_geo_i1_j,al_fecr_geo_h1_unadj_i0_a,al_fecr_geo_h1_unadj_i1_a,
                                  al_em_geo_ihh0_a,al_em_geo_ihh1_a,al_fecr_geo_h1_unadj_ihh0_a,al_fecr_geo_h1_unadj_ihh1_a,
                                  al_em_geo_psac0_j, al_em_geo_psac1_j, al_fecr_geo_h1_unadj_psac0_a, al_fecr_geo_h1_unadj_psac1_a,
                                  al_em_geo_poor0_j, al_em_geo_poor1_j, al_fecr_geo_h1_unadj_poor0_a, al_fecr_geo_h1_unadj_poor1_a,
                                  al_em_geo_dw0_j, al_em_geo_dw1_j, al_fecr_geo_h1_unadj_dw0_a, al_fecr_geo_h1_unadj_dw1_a,
                                  al_em_geo_def0_j, al_em_geo_def1_j, al_fecr_geo_h1_unadj_def0_a, al_fecr_geo_h1_unadj_def1_a,
                                  al_em_geo_ind0_j, al_em_geo_ind1_j, al_fecr_geo_h1_unadj_ind0_a, al_fecr_geo_h1_unadj_ind1_a,
                                  al_em_geo_hmud0_j, al_em_geo_hmud1_j, al_fecr_geo_h1_unadj_hmud0_a, al_fecr_geo_h1_unadj_hmud1_a,
                                  al_em_geo_lmud0_j, al_em_geo_lmud1_j, al_fecr_geo_h1_unadj_lmud0_a, al_fecr_geo_h1_unadj_lmud1_a,
                                  al_em_geo_lat0_j, al_em_geo_lat1_j, al_fecr_geo_h1_unadj_lat0_a, al_fecr_geo_h1_unadj_lat1_a,
                                  al_em_geo_fec0_j, al_em_geo_fec1_j, al_fecr_geo_h1_unadj_fec0_a, al_fecr_geo_h1_unadj_fec1_a,
                                  al_em_geo_odf0_j, al_em_geo_odf1_j, al_fecr_geo_h1_unadj_odf0_a, al_fecr_geo_h1_unadj_odf1_a)


hw.em.epg.table=make.em.epg.table(hw_int_mn,hw_int_gmn,hw_fecr_geo_h1_unadj_j,
                                  hw_em_geo_i0_j,hw_em_geo_i1_j,hw_fecr_geo_h1_unadj_i0_a,hw_fecr_geo_h1_unadj_i1_a,
                                  hw_em_geo_ihh0_a,hw_em_geo_ihh1_a,hw_fecr_geo_h1_unadj_ihh0_a,hw_fecr_geo_h1_unadj_ihh1_a,
                                  hw_em_geo_psac0_j, hw_em_geo_psac1_j, hw_fecr_geo_h1_unadj_psac0_a, hw_fecr_geo_h1_unadj_psac1_a,
                                  hw_em_geo_poor0_j, hw_em_geo_poor1_j, hw_fecr_geo_h1_unadj_poor0_a, hw_fecr_geo_h1_unadj_poor1_a,
                                  hw_em_geo_dw0_j, hw_em_geo_dw1_j, hw_fecr_geo_h1_unadj_dw0_a, hw_fecr_geo_h1_unadj_dw1_a,
                                  hw_em_geo_def0_j, hw_em_geo_def1_j, hw_fecr_geo_h1_unadj_def0_a, hw_fecr_geo_h1_unadj_def1_a,
                                  hw_em_geo_ind0_j, hw_em_geo_ind1_j, hw_fecr_geo_h1_unadj_ind0_a, hw_fecr_geo_h1_unadj_ind1_a,
                                  hw_em_geo_hmud0_j, hw_em_geo_hmud1_j, hw_fecr_geo_h1_unadj_hmud0_a, hw_fecr_geo_h1_unadj_hmud1_a,
                                  hw_em_geo_lmud0_j, hw_em_geo_lmud1_j, hw_fecr_geo_h1_unadj_lmud0_a, hw_fecr_geo_h1_unadj_lmud1_a,
                                  hw_em_geo_lat0_j, hw_em_geo_lat1_j, hw_fecr_geo_h1_unadj_lat0_a, hw_fecr_geo_h1_unadj_lat1_a,
                                  hw_em_geo_fec0_j, hw_em_geo_fec1_j, hw_fecr_geo_h1_unadj_fec0_a, hw_fecr_geo_h1_unadj_fec1_a,
                                  hw_em_geo_odf0_j, hw_em_geo_odf1_j, hw_fecr_geo_h1_unadj_odf0_a, hw_fecr_geo_h1_unadj_odf1_a)


tt.em.epg.table=make.em.epg.table(tt_int_mn,tt_int_gmn,tt_fecr_geo_h1_unadj_j,
                                  tt_em_geo_i0_j,tt_em_geo_i1_j,tt_fecr_geo_h1_unadj_i0_a,tt_fecr_geo_h1_unadj_i1_a,
                                  tt_em_geo_ihh0_a,tt_em_geo_ihh1_a,tt_fecr_geo_h1_unadj_ihh0_a,tt_fecr_geo_h1_unadj_ihh1_a,
                                  tt_em_geo_psac0_j, tt_em_geo_psac1_j, tt_fecr_geo_h1_unadj_psac0_a, tt_fecr_geo_h1_unadj_psac1_a,
                                  tt_em_geo_poor0_j, tt_em_geo_poor1_j, tt_fecr_geo_h1_unadj_poor0_a, tt_fecr_geo_h1_unadj_poor1_a,
                                  tt_em_geo_dw0_j, tt_em_geo_dw1_j, tt_fecr_geo_h1_unadj_dw0_a, tt_fecr_geo_h1_unadj_dw1_a,
                                  tt_em_geo_def0_j, tt_em_geo_def1_j, tt_fecr_geo_h1_unadj_def0_a, tt_fecr_geo_h1_unadj_def1_a,
                                  tt_em_geo_ind0_j, tt_em_geo_ind1_j, tt_fecr_geo_h1_unadj_ind0_a, tt_fecr_geo_h1_unadj_ind1_a,
                                  tt_em_geo_hmud0_j, tt_em_geo_hmud1_j, tt_fecr_geo_h1_unadj_hmud0_a, tt_fecr_geo_h1_unadj_hmud1_a,
                                  tt_em_geo_lmud0_j, tt_em_geo_lmud1_j, tt_fecr_geo_h1_unadj_lmud0_a, tt_fecr_geo_h1_unadj_lmud1_a,
                                  tt_em_geo_lat0_j, tt_em_geo_lat1_j, tt_fecr_geo_h1_unadj_lat0_a, tt_fecr_geo_h1_unadj_lat1_a,
                                  tt_em_geo_fec0_j, tt_em_geo_fec1_j, tt_fecr_geo_h1_unadj_fec0_a, tt_fecr_geo_h1_unadj_fec1_a,
                                  tt_em_geo_odf0_j, tt_em_geo_odf1_j, tt_fecr_geo_h1_unadj_odf0_a, tt_fecr_geo_h1_unadj_odf1_a)


# EPG EM FECR
al.sub.epg.table=make.sub.epg.table(al_int_mn,al_int_gmn,al_fecr_geo_h1_unadj_j,
                                  al_em_geo_i0_j,al_em_geo_i1_j,al_fecr_geo_h1_unadj_i0_a,al_fecr_geo_h1_unadj_i1_a,
                                  al_em_geo_ihh0_a,al_em_geo_ihh1_a,al_fecr_geo_h1_unadj_ihh0_a,al_fecr_geo_h1_unadj_ihh1_a,
                                  al_em_geo_psac0_j, al_em_geo_psac1_j, al_fecr_geo_h1_unadj_psac0_a, al_fecr_geo_h1_unadj_psac1_a,
                                  al_em_geo_poor0_j, al_em_geo_poor1_j, al_fecr_geo_h1_unadj_poor0_a, al_fecr_geo_h1_unadj_poor1_a,
                                  al_em_geo_dw0_j, al_em_geo_dw1_j, al_fecr_geo_h1_unadj_dw0_a, al_fecr_geo_h1_unadj_dw1_a,
                                  al_em_geo_def0_j, al_em_geo_def1_j, al_fecr_geo_h1_unadj_def0_a, al_fecr_geo_h1_unadj_def1_a,
                                  al_em_geo_ind0_j, al_em_geo_ind1_j, al_fecr_geo_h1_unadj_ind0_a, al_fecr_geo_h1_unadj_ind1_a,
                                  al_em_geo_hmud0_j, al_em_geo_hmud1_j, al_fecr_geo_h1_unadj_hmud0_a, al_fecr_geo_h1_unadj_hmud1_a,
                                  al_em_geo_lmud0_j, al_em_geo_lmud1_j, al_fecr_geo_h1_unadj_lmud0_a, al_fecr_geo_h1_unadj_lmud1_a,
                                  al_em_geo_lat0_j, al_em_geo_lat1_j, al_fecr_geo_h1_unadj_lat0_a, al_fecr_geo_h1_unadj_lat1_a,
                                  al_em_geo_fec0_j, al_em_geo_fec1_j, al_fecr_geo_h1_unadj_fec0_a, al_fecr_geo_h1_unadj_fec1_a,
                                  al_em_geo_odf0_j, al_em_geo_odf1_j, al_fecr_geo_h1_unadj_odf0_a, al_fecr_geo_h1_unadj_odf1_a)


hw.sub.epg.table=make.sub.epg.table(hw_int_mn,hw_int_gmn,hw_fecr_geo_h1_unadj_j,
                                  hw_em_geo_i0_j,hw_em_geo_i1_j,hw_fecr_geo_h1_unadj_i0_a,hw_fecr_geo_h1_unadj_i1_a,
                                  hw_em_geo_ihh0_a,hw_em_geo_ihh1_a,hw_fecr_geo_h1_unadj_ihh0_a,hw_fecr_geo_h1_unadj_ihh1_a,
                                  hw_em_geo_psac0_j, hw_em_geo_psac1_j, hw_fecr_geo_h1_unadj_psac0_a, hw_fecr_geo_h1_unadj_psac1_a,
                                  hw_em_geo_poor0_j, hw_em_geo_poor1_j, hw_fecr_geo_h1_unadj_poor0_a, hw_fecr_geo_h1_unadj_poor1_a,
                                  hw_em_geo_dw0_j, hw_em_geo_dw1_j, hw_fecr_geo_h1_unadj_dw0_a, hw_fecr_geo_h1_unadj_dw1_a,
                                  hw_em_geo_def0_j, hw_em_geo_def1_j, hw_fecr_geo_h1_unadj_def0_a, hw_fecr_geo_h1_unadj_def1_a,
                                  hw_em_geo_ind0_j, hw_em_geo_ind1_j, hw_fecr_geo_h1_unadj_ind0_a, hw_fecr_geo_h1_unadj_ind1_a,
                                  hw_em_geo_hmud0_j, hw_em_geo_hmud1_j, hw_fecr_geo_h1_unadj_hmud0_a, hw_fecr_geo_h1_unadj_hmud1_a,
                                  hw_em_geo_lmud0_j, hw_em_geo_lmud1_j, hw_fecr_geo_h1_unadj_lmud0_a, hw_fecr_geo_h1_unadj_lmud1_a,
                                  hw_em_geo_lat0_j, hw_em_geo_lat1_j, hw_fecr_geo_h1_unadj_lat0_a, hw_fecr_geo_h1_unadj_lat1_a,
                                  hw_em_geo_fec0_j, hw_em_geo_fec1_j, hw_fecr_geo_h1_unadj_fec0_a, hw_fecr_geo_h1_unadj_fec1_a,
                                  hw_em_geo_odf0_j, hw_em_geo_odf1_j, hw_fecr_geo_h1_unadj_odf0_a, hw_fecr_geo_h1_unadj_odf1_a)


tt.sub.epg.table=make.sub.epg.table(tt_int_mn,tt_int_gmn,tt_fecr_geo_h1_unadj_j,
                                  tt_em_geo_i0_j,tt_em_geo_i1_j,tt_fecr_geo_h1_unadj_i0_a,tt_fecr_geo_h1_unadj_i1_a,
                                  tt_em_geo_ihh0_a,tt_em_geo_ihh1_a,tt_fecr_geo_h1_unadj_ihh0_a,tt_fecr_geo_h1_unadj_ihh1_a,
                                  tt_em_geo_psac0_j, tt_em_geo_psac1_j, tt_fecr_geo_h1_unadj_psac0_a, tt_fecr_geo_h1_unadj_psac1_a,
                                  tt_em_geo_poor0_j, tt_em_geo_poor1_j, tt_fecr_geo_h1_unadj_poor0_a, tt_fecr_geo_h1_unadj_poor1_a,
                                  tt_em_geo_dw0_j, tt_em_geo_dw1_j, tt_fecr_geo_h1_unadj_dw0_a, tt_fecr_geo_h1_unadj_dw1_a,
                                  tt_em_geo_def0_j, tt_em_geo_def1_j, tt_fecr_geo_h1_unadj_def0_a, tt_fecr_geo_h1_unadj_def1_a,
                                  tt_em_geo_ind0_j, tt_em_geo_ind1_j, tt_fecr_geo_h1_unadj_ind0_a, tt_fecr_geo_h1_unadj_ind1_a,
                                  tt_em_geo_hmud0_j, tt_em_geo_hmud1_j, tt_fecr_geo_h1_unadj_hmud0_a, tt_fecr_geo_h1_unadj_hmud1_a,
                                  tt_em_geo_lmud0_j, tt_em_geo_lmud1_j, tt_fecr_geo_h1_unadj_lmud0_a, tt_fecr_geo_h1_unadj_lmud1_a,
                                  tt_em_geo_lat0_j, tt_em_geo_lat1_j, tt_fecr_geo_h1_unadj_lat0_a, tt_fecr_geo_h1_unadj_lat1_a,
                                  tt_em_geo_fec0_j, tt_em_geo_fec1_j, tt_fecr_geo_h1_unadj_fec0_a, tt_fecr_geo_h1_unadj_fec1_a,
                                  tt_em_geo_odf0_j, tt_em_geo_odf1_j, tt_fecr_geo_h1_unadj_odf0_a, tt_fecr_geo_h1_unadj_odf1_a)

# Index child and index household table
index.table=make.index.table(al_prev,hw_prev,tt_prev,sth_prev,
                             al_em_prev_i1_j,hw_em_prev_i1_j,tt_em_prev_i1_j,sth_em_prev_i1_j, 
                             al_em_prev_ihh1_a,hw_em_prev_ihh1_a,tt_em_prev_ihh1_a,sth_em_prev_ihh1_a, 
                             al_em_prev_ihh0_a,hw_em_prev_ihh0_a,tt_em_prev_ihh0_a,sth_em_prev_ihh0_a, 
                             al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                             al_rr_h1_unadj_i1_a,hw_rr_h1_unadj_i1_a,tt_rr_h1_unadj_i1_a,sth_rr_h1_unadj_i1_a, 
                             al_rr_h1_unadj_ihh1_a,hw_rr_h1_unadj_ihh1_a,tt_rr_h1_unadj_ihh1_a,sth_rr_h1_unadj_ihh1_a, 
                             al_rr_h1_unadj_ihh0_a,hw_rr_h1_unadj_ihh0_a,tt_rr_h1_unadj_ihh0_a,sth_rr_h1_unadj_ihh0_a,decimals=2)

# Index child table
i.sub.table=make.sub.table(al_prev,hw_prev,tt_prev,sth_prev,
                            al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                            al_em_prev_i1_j,hw_em_prev_i1_j,tt_em_prev_i1_j,sth_em_prev_i1_j, 
                            al_em_prev_i0_j,hw_em_prev_i0_j,tt_em_prev_i0_j,sth_em_prev_i0_j, 
                            al_rr_h1_unadj_i1_a,hw_rr_h1_unadj_i1_a,tt_rr_h1_unadj_i1_a,sth_rr_h1_unadj_i1_a, 
                            al_rr_h1_unadj_i0_a,hw_rr_h1_unadj_i0_a,tt_rr_h1_unadj_i0_a,sth_rr_h1_unadj_i0_a,decimals=2)

# Index household table
ihh.sub.table=make.sub.table(al_em_prev_i0_j,hw_em_prev_i0_j,tt_em_prev_i0_j,sth_em_prev_i0_j,
                           al_rr_h1_unadj_i0_a,hw_rr_h1_unadj_i0_a,tt_rr_h1_unadj_i0_a,sth_rr_h1_unadj_i0_a,
                           al_em_prev_ihh1_a,hw_em_prev_ihh1_a,tt_em_prev_ihh1_a,sth_em_prev_ihh1_a, 
                           al_em_prev_ihh0_a,hw_em_prev_ihh0_a,tt_em_prev_ihh0_a,sth_em_prev_ihh0_a, 
                           al_rr_h1_unadj_ihh1_a,hw_rr_h1_unadj_ihh1_a,tt_rr_h1_unadj_ihh1_a,sth_rr_h1_unadj_ihh1_a, 
                           al_rr_h1_unadj_ihh0_a,hw_rr_h1_unadj_ihh0_a,tt_rr_h1_unadj_ihh0_a,sth_rr_h1_unadj_ihh0_a,decimals=2)

# PSAC table
psac.sub.table=make.sub.table(al_prev,hw_prev,tt_prev,sth_prev,
                           al_rr_h1_unadj_tmle_a,hw_rr_h1_unadj_tmle_a,tt_rr_h1_unadj_tmle_a,sth_rr_h1_unadj_tmle_a,
                           al_em_prev_psac1_j,hw_em_prev_psac1_j,tt_em_prev_psac1_j,sth_em_prev_psac1_j, 
                           al_em_prev_psac0_j,hw_em_prev_psac0_j,tt_em_prev_psac0_j,sth_em_prev_psac0_j, 
                           al_rr_h1_unadj_psac1_a,hw_rr_h1_unadj_psac1_a,tt_rr_h1_unadj_psac1_a,sth_rr_h1_unadj_psac1_a, 
                           al_rr_h1_unadj_psac0_a,hw_rr_h1_unadj_psac0_a,tt_rr_h1_unadj_psac0_a,sth_rr_h1_unadj_psac0_a,decimals=2)

# Prevalence ratio 
#bin.h1.rr.table=make.h1.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rr_h1_unadj_j,hw_rr_h1_unadj_j,tt_rr_h1_unadj_j,sth_rr_h1_unadj_j,
#                                  al_rr_h1_adj_j,hw_rr_h1_adj_j,tt_rr_h1_adj_j,sth_rr_h1_adj_j,
#                                  al_rr_h1_ipcw_j,hw_rr_h1_ipcw_j,tt_rr_h1_ipcw_j,sth_rr_h1_ipcw_j,decimals=2)

#bin.h2.rr.table=make.h2.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rr_h2_unadj_j,hw_rr_h2_unadj_j,tt_rr_h2_unadj_j,sth_rr_h2_unadj_j,
#                                  al_rr_h2_adj_j,hw_rr_h2_adj_j,tt_rr_h2_adj_j,sth_rr_h2_adj_j,
#                                  al_rr_h2_ipcw_j,hw_rr_h2_ipcw_j,tt_rr_h2_ipcw_j,sth_rr_h2_ipcw_j,decimals=2)

#bin.h3.rr.table=make.h3.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rr_h3_unadj_j,hw_rr_h3_unadj_j,tt_rr_h3_unadj_j,sth_rr_h3_unadj_j,
#                                  al_rr_h3_adj_j,hw_rr_h3_adj_j,tt_rr_h3_adj_j,sth_rr_h3_adj_j,
#                                  al_rr_h3_ipcw_j,hw_rr_h3_ipcw_j,tt_rr_h3_ipcw_j,sth_rr_h3_ipcw_j,decimals=2)

# Prevalence difference 
#bin.h1.rd.table=make.h1.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rd_h1_unadj_j,hw_rd_h1_unadj_j,tt_rd_h1_unadj_j,sth_rd_h1_unadj_j,
#                                  al_rd_h1_adj_j,hw_rd_h1_adj_j,tt_rd_h1_adj_j,sth_rd_h1_adj_j,
#                                  al_rd_h1_ipcw_j,hw_rd_h1_ipcw_j,tt_rd_h1_ipcw_j,sth_rd_h1_ipcw_j,decimals=2)

#bin.h2.rd.table=make.h2.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rd_h2_unadj_j,hw_rd_h2_unadj_j,tt_rd_h2_unadj_j,sth_rd_h2_unadj_j,
#                                  al_rd_h2_adj_j,hw_rd_h2_adj_j,tt_rd_h2_adj_j,sth_rd_h2_adj_j,
#                                  al_rd_h2_ipcw_j,hw_rd_h2_ipcw_j,tt_rd_h2_ipcw_j,sth_rd_h2_ipcw_j,decimals=2)

#bin.h3.rd.table=make.h3.bin.table(al_prev,hw_prev,tt_prev,sth_prev,
#                                  al_rd_h3_unadj_j,hw_rd_h3_unadj_j,tt_rd_h3_unadj_j,sth_rd_h3_unadj_j,
#                                  al_rd_h3_adj_j,hw_rd_h3_adj_j,tt_rd_h3_adj_j,sth_rd_h3_adj_j,
#                                  al_rd_h3_ipcw_j,hw_rd_h3_ipcw_j,tt_rd_h3_ipcw_j,sth_rd_h3_ipcw_j,decimals=2)

# Moderate heavy prevalence ratio
#bin.mh.h1.rr.table=make.h1.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rr_h1_unadj_j,hw_mh_rr_h1_unadj_j,tt_mh_rr_h1_unadj_j,sth_mh_rr_h1_unadj_j,
#                                     al_mh_rr_h1_adj_j,hw_mh_rr_h1_adj_j,tt_mh_rr_h1_adj_j,sth_mh_rr_h1_adj_j,
#                                     al_mh_rr_h1_ipcw_j,hw_mh_rr_h1_ipcw_j,tt_mh_rr_h1_ipcw_j,sth_mh_rr_h1_ipcw_j,decimals=2)

#bin.mh.h2.rr.table=make.h2.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rr_h2_unadj_j,hw_mh_rr_h2_unadj_j,tt_mh_rr_h2_unadj_j,sth_mh_rr_h2_unadj_j,
#                                     al_mh_rr_h2_adj_j,hw_mh_rr_h2_adj_j,tt_mh_rr_h2_adj_j,sth_mh_rr_h2_adj_j,
#                                     al_mh_rr_h2_ipcw_j,hw_mh_rr_h2_ipcw_j,tt_mh_rr_h2_ipcw_j,sth_mh_rr_h2_ipcw_j,decimals=2)

#bin.mh.h3.rr.table=make.h3.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rr_h3_unadj_j,hw_mh_rr_h3_unadj_j,tt_mh_rr_h3_unadj_j,sth_mh_rr_h3_unadj_j,
#                                     al_mh_rr_h3_adj_j,hw_mh_rr_h3_adj_j,tt_mh_rr_h3_adj_j,sth_mh_rr_h3_adj_j,
#                                     al_mh_rr_h3_ipcw_j,hw_mh_rr_h3_ipcw_j,tt_mh_rr_h3_ipcw_j,sth_mh_rr_h3_ipcw_j,decimals=2)

# Moderate heavy prevalence difference
#bin.mh.h1.rd.table=make.h1.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rd_h1_unadj_j,hw_mh_rd_h1_unadj_j,tt_mh_rd_h1_unadj_j,sth_mh_rd_h1_unadj_j,
#                                     al_mh_rd_h1_adj_j,hw_mh_rd_h1_adj_j,tt_mh_rd_h1_adj_j,sth_mh_rd_h1_adj_j,
#                                     al_mh_rd_h1_ipcw_j,hw_mh_rd_h1_ipcw_j,tt_mh_rd_h1_ipcw_j,sth_mh_rd_h1_ipcw_j,decimals=2)

#bin.mh.h2.rd.table=make.h2.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rd_h2_unadj_j,hw_mh_rd_h2_unadj_j,tt_mh_rd_h2_unadj_j,sth_mh_rd_h2_unadj_j,
#                                     al_mh_rd_h2_adj_j,hw_mh_rd_h2_adj_j,tt_mh_rd_h2_adj_j,sth_mh_rd_h2_adj_j,
#                                     al_mh_rd_h2_ipcw_j,hw_mh_rd_h2_ipcw_j,tt_mh_rd_h2_ipcw_j,sth_mh_rd_h2_ipcw_j,decimals=2)

#bin.mh.h3.rd.table=make.h3.bin.table(al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
#                                     al_mh_rd_h3_unadj_j,hw_mh_rd_h3_unadj_j,tt_mh_rd_h3_unadj_j,sth_mh_rd_h3_unadj_j,
#                                     al_mh_rd_h3_adj_j,hw_mh_rd_h3_adj_j,tt_mh_rd_h3_adj_j,sth_mh_rd_h3_adj_j,
#                                     al_mh_rd_h3_ipcw_j,hw_mh_rd_h3_ipcw_j,tt_mh_rd_h3_ipcw_j,sth_mh_rd_h3_ipcw_j,decimals=2)

# Geometric FECR
#epg.h1.geo.table=make.h1.epg.table(al_int_gmn,hw_int_gmn,tt_int_gmn,
#                               al_fecr_geo_h1_unadj_j,hw_fecr_geo_h1_unadj_j,tt_fecr_geo_h1_unadj_j,
#                               al_fecr_geo_h1_adj_j,hw_fecr_geo_h1_adj_j,tt_fecr_geo_h1_adj_j,
#                               al_fecr_geo_h1_ipcw_j,hw_fecr_geo_h1_ipcw_j,tt_fecr_geo_h1_ipcw_j,decimals=2)

#epg.h2.geo.table=make.h2.epg.table(al_int_gmn,hw_int_gmn,tt_int_gmn,
#                               al_fecr_geo_h2_unadj_j,hw_fecr_geo_h2_unadj_j,tt_fecr_geo_h2_unadj_j,
#                               al_fecr_geo_h2_adj_j,hw_fecr_geo_h2_adj_j,tt_fecr_geo_h2_adj_j,
#                               al_fecr_geo_h2_ipcw_j,hw_fecr_geo_h2_ipcw_j,tt_fecr_geo_h2_ipcw_j,decimals=2)

#epg.h3.geo.table=make.h3.epg.table(al_int_gmn,hw_int_gmn,tt_int_gmn,
#                               al_fecr_geo_h3_unadj_j,hw_fecr_geo_h3_unadj_j,tt_fecr_geo_h3_unadj_j,
#                               al_fecr_geo_h3_adj_j,hw_fecr_geo_h3_adj_j,tt_fecr_geo_h3_adj_j,
#                               al_fecr_geo_h3_ipcw_j,hw_fecr_geo_h3_ipcw_j,tt_fecr_geo_h3_ipcw_j,decimals=2)

# Arithmetic FECR
#epg.h1.ari.table=make.h1.epg.table(al_int_mn,hw_int_mn,tt_int_mn,
#                              al_fecr_ari_h1_unadj_j,hw_fecr_ari_h1_unadj_j,tt_fecr_ari_h1_unadj_j,
#                              al_fecr_ari_h1_adj_j,hw_fecr_ari_h1_adj_j,tt_fecr_ari_h1_adj_j,
#                              al_fecr_ari_h1_ipcw_j,hw_fecr_ari_h1_ipcw_j,tt_fecr_ari_h1_ipcw_j,decimals=2)

#epg.h2.ari.table=make.h2.epg.table(al_int_mn,hw_int_mn,tt_int_mn,
#                              al_fecr_ari_h2_unadj_j,hw_fecr_ari_h2_unadj_j,tt_fecr_ari_h2_unadj_j,
#                              al_fecr_ari_h2_adj_j,hw_fecr_ari_h2_adj_j,tt_fecr_ari_h2_adj_j,
#                              al_fecr_ari_h2_ipcw_j,hw_fecr_ari_h2_ipcw_j,tt_fecr_ari_h2_ipcw_j,decimals=2)

#epg.h3.ari.table=make.h3.epg.table(al_int_mn,hw_int_mn,tt_int_mn,
#                              al_fecr_ari_h3_unadj_j,hw_fecr_ari_h3_unadj_j,tt_fecr_ari_h3_unadj_j,
#                              al_fecr_ari_h3_adj_j,hw_fecr_ari_h3_adj_j,tt_fecr_ari_h3_adj_j,
#                              al_fecr_ari_h3_ipcw_j,hw_fecr_ari_h3_ipcw_j,tt_fecr_ari_h3_ipcw_j,decimals=2)
