##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Generate plots

# by Jade
##############################################
rm(list=ls())
library(ggplot2)
library(grid)
library(gridExtra)

source("~/Box Sync/WASHB Parasites/Scripts/Figures/0-base-plot-functions.R")

#---------------------------------------------
# Primary analysis
#---------------------------------------------
# Descriptive
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_prev.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_prev_em.RData")

# Unadjusted
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_mh_pr_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_unadj.RData")
load("~/Box Sync/WASHB Parasites/Results/Ayse/sth_prev_unadj_tmle.RData")

# Adjusted
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_mh_adj.RData")

# IPCW
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_ipcw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_mh_ipcw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_ipcw.RData")

# Effect modification binary
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_psac.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_Ncomp.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_Nchild.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_defday.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_latrine.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_scoop.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_noopendef.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_wealth.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_hh.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_lat.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_geophagia.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_shoe.RData")

# Effect modification epg
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_mean_em.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_sac.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_Ncomp.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_Nchild.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_defday.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_latrine.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_scoop.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_noopendef.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_wealth.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_hh.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_lat.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dw.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_geophagia.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_shoes.RData")

# binary primary outcome unadj 
sth.bin.plot(psth_n_prev_j$N.al,psth_n_prev_j$N.hw,psth_n_prev_j$N.tt,psth_n_prev_j$N.sth,
             al_prev,hw_prev,tt_prev,sth_prev,
             al_rr_h1_unadj_j,al_rr_h2_unadj_j,al_rr_h3_unadj_j,
             hw_rr_h1_unadj_j,hw_rr_h2_unadj_j,hw_rr_h3_unadj_j,
             tt_rr_h1_unadj_j,tt_rr_h2_unadj_j,tt_rr_h3_unadj_j,
             sth_rr_h1_unadj_j,sth_rr_h2_unadj_j,sth_rr_h3_unadj_j,
             lab="primary")

# binary primary outcome unadj tmle
# Corresponds to manuscript Figure 3
sth.bin.plot(psth_n_prev_j$N.al,psth_n_prev_j$N.hw,psth_n_prev_j$N.tt,psth_n_prev_j$N.sth,
             al_prev,hw_prev,tt_prev,sth_prev,
             al_rr_h1_unadj_tmle_a,al_rr_h2_unadj_tmle_a,al_rr_h3_unadj_tmle_a,
             hw_rr_h1_unadj_tmle_a,hw_rr_h2_unadj_tmle_a,hw_rr_h3_unadj_tmle_a,
             tt_rr_h1_unadj_tmle_a,tt_rr_h2_unadj_tmle_a,tt_rr_h3_unadj_tmle_a,
             sth_rr_h1_unadj_tmle_a,sth_rr_h2_unadj_tmle_a,sth_rr_h3_unadj_tmle_a,
             lab="primary-tmle")

# epg primary outcome unadj
# Corresponds to manuscript Figure 4
sth.epg.plot(psth_n_int_j$N.int.al,psth_n_int_j$N.int.hw,psth_n_int_j$N.int.tt,
             al_int_gmn,hw_int_gmn,tt_int_gmn,
             al_fecr_geo_h1_unadj_j,al_fecr_geo_h2_unadj_j,al_fecr_geo_h3_unadj_j,
             hw_fecr_geo_h1_unadj_j,hw_fecr_geo_h2_unadj_j,hw_fecr_geo_h3_unadj_j,
             tt_fecr_geo_h1_unadj_j,tt_fecr_geo_h2_unadj_j,tt_fecr_geo_h3_unadj_j,
             lab="primary")

#---------------------------------------------
# Effect modification binary
#---------------------------------------------

# index child
sth.bin.em.plot(al_em_prev_i1_j,hw_em_prev_i1_j,tt_em_prev_i1_j,sth_em_prev_i1_j,
                al_em_prev_i0_j,hw_em_prev_i0_j,tt_em_prev_i0_j,sth_em_prev_i0_j,
                al_rr_h1_i1_j,hw_rr_h1_i1_j,tt_rr_h1_i1_j,sth_rr_h1_i1_j,
                al_rr_h1_i0_j,hw_rr_h1_i0_j,tt_rr_h1_i0_j,sth_rr_h1_i0_j,
               figlab="index",em1lab="Index child",em0lab="Not index child",
               em1labs="Index child",em0labs="Not index child",ylim=c(0,0.6))

# presac vs. sac
sth.bin.em.plot(al_em_prev_psac1_j,hw_em_prev_psac1_j,tt_em_prev_psac1_j,sth_em_prev_psac1_j,
                al_em_prev_psac0_j,hw_em_prev_psac0_j,tt_em_prev_psac0_j,sth_em_prev_psac0_j,
                al_rr_h1_psac1_j,hw_rr_h1_psac1_j,tt_rr_h1_psac1_j,sth_rr_h1_psac1_j,
                al_rr_h1_psac0_j,hw_rr_h1_psac0_j,tt_rr_h1_psac0_j,sth_rr_h1_psac0_j,
                figlab="psac",em1lab="Pre-school aged (PSAC, 2-4 yrs)",em0lab="School aged (SAC, 5-12 yrs)",
                em1labs="PSAC",em0labs="SAC",ylim=c(0,0.6))

# number of people in compound
sth.bin.em.plot(al_em_prev_ind1_j,hw_em_prev_ind1_j,tt_em_prev_ind1_j,sth_em_prev_ind1_j,
                al_em_prev_ind0_j,hw_em_prev_ind0_j,tt_em_prev_ind0_j,sth_em_prev_ind0_j,
                al_rr_h1_ind1_j,hw_rr_h1_ind1_j,tt_rr_h1_ind1_j,sth_rr_h1_ind1_j,
                al_rr_h1_ind0_j,hw_rr_h1_ind0_j,tt_rr_h1_ind0_j,sth_rr_h1_ind0_j,
               figlab="ind",em1lab=">=10 people in compound",em0lab="<10 people in compound",
               em1labs=">=10",em0labs="<10",ylim=c(0,0.6))

# number of school aged children in compound
sth.bin.em.plot(al_em_prev_ch1_j,hw_em_prev_ch1_j,tt_em_prev_ch1_j,sth_em_prev_ch1_j,
                al_em_prev_ch0_j,hw_em_prev_ch0_j,tt_em_prev_ch0_j,sth_em_prev_ch0_j,
                al_rr_h1_ch1_j,hw_rr_h1_ch1_j,tt_rr_h1_ch1_j,sth_rr_h1_ch1_j,
                al_rr_h1_ch0_j,hw_rr_h1_ch0_j,tt_rr_h1_ch0_j,sth_rr_h1_ch0_j,
               figlab="ch",em1lab=">=1 school aged child in compound",em0lab="No school aged child in compound",
               em1labs=">=1",em0labs="0",ylim=c(0,0.6))

# defecation day
sth.bin.em.plot(al_em_prev_def1_j,hw_em_prev_def1_j,tt_em_prev_def1_j,sth_em_prev_def1_j,
                al_em_prev_def0_j,hw_em_prev_def0_j,tt_em_prev_def0_j,sth_em_prev_def0_j,
                al_rr_h1_def1_j,hw_rr_h1_def1_j,tt_rr_h1_def1_j,sth_rr_h1_def1_j,
                al_rr_h1_def0_j,hw_rr_h1_def0_j,tt_rr_h1_def0_j,sth_rr_h1_def0_j,
               figlab="def",em1lab="Same day defecation",em0lab="Previous day defecation",
               em1labs="Same day",em0labs="Prev day",ylim=c(0,0.6))

# latrine status at baseline
sth.bin.em.plot(al_em_prev_lat1_j,hw_em_prev_lat1_j,tt_em_prev_lat1_j,sth_em_prev_lat1_j,
                al_em_prev_lat0_j,hw_em_prev_lat0_j,tt_em_prev_lat0_j,sth_em_prev_lat0_j,
                al_rr_h1_lat1_j,hw_rr_h1_lat1_j,tt_rr_h1_lat1_j,sth_rr_h1_lat1_j,
                al_rr_h1_lat0_j,hw_rr_h1_lat0_j,tt_rr_h1_lat0_j,sth_rr_h1_lat0_j,
               figlab="lat",em1lab="Latrine w/ water seal and hygienic drain",
               em0lab="No latrine w/ water seal and hygienic drain",
               em1labs="Good latrine",em0labs="Bad latrine",ylim=c(0,0.6))

# child feces management at baseline
sth.bin.em.plot(al_em_prev_fec1_j,hw_em_prev_fec1_j,tt_em_prev_fec1_j,sth_em_prev_fec1_j,
                al_em_prev_fec0_j,hw_em_prev_fec0_j,tt_em_prev_fec0_j,sth_em_prev_fec0_j,
                al_rr_h1_fec1_j,hw_rr_h1_fec1_j,tt_rr_h1_fec1_j,sth_rr_h1_fec1_j,
                al_rr_h1_fec0_j,hw_rr_h1_fec0_j,tt_rr_h1_fec0_j,sth_rr_h1_fec0_j,
               figlab="fec",em1lab="Scoop used for child feces",em0lab="No scoop used for child",
               em1labs="Scoop",em0labs="No scoop",ylim=c(0,0.6))

# open defecation at baseline
sth.bin.em.plot(al_em_prev_odf1_j,hw_em_prev_odf1_j,tt_em_prev_odf1_j,sth_em_prev_odf1_j,
                al_em_prev_odf0_j,hw_em_prev_odf0_j,tt_em_prev_odf0_j,sth_em_prev_odf0_j,
                al_rr_h1_odf1_j,hw_rr_h1_odf1_j,tt_rr_h1_odf1_j,sth_rr_h1_odf1_j,
                al_rr_h1_odf0_j,hw_rr_h1_odf0_j,tt_rr_h1_odf0_j,sth_rr_h1_odf0_j,
               figlab="odf",em1lab="Compound open defecation free",em0lab="Compound not open defecation free",
               em1labs="Open defecation free",em0labs="Not open defecation free",ylim=c(0,0.6))

# wealth
sth.bin.em.plot(al_em_prev_poor1_j,hw_em_prev_poor1_j,tt_em_prev_poor1_j,sth_em_prev_poor1_j,
                al_em_prev_poor0_j,hw_em_prev_poor0_j,tt_em_prev_poor0_j,sth_em_prev_poor0_j,
                al_rr_h1_poor1_j,hw_rr_h1_poor1_j,tt_rr_h1_poor1_j,sth_rr_h1_poor1_j,
                al_rr_h1_poor0_j,hw_rr_h1_poor0_j,tt_rr_h1_poor0_j,sth_rr_h1_poor0_j,
               figlab="poor",em1lab="Below median wealth",em0lab="Above median wealth",
               em1labs="Below median",em0labs="Above median",ylim=c(0,0.6))

# household floor
sth.bin.em.plot(al_em_prev_hmud1_j,hw_em_prev_hmud1_j,tt_em_prev_hmud1_j,sth_em_prev_hmud1_j,
                al_em_prev_hmud0_j,hw_em_prev_hmud0_j,tt_em_prev_hmud0_j,sth_em_prev_hmud0_j,
                al_rr_h1_hmud1_j,hw_rr_h1_hmud1_j,tt_rr_h1_hmud1_j,sth_rr_h1_hmud1_j,
                al_rr_h1_hmud0_j,hw_rr_h1_hmud0_j,tt_rr_h1_hmud0_j,sth_rr_h1_hmud0_j,
               figlab="hmud",em1lab="Some mud floor in household",em0lab="No mud floor in household",
               em1labs="Some mud",em0labs="No mud",ylim=c(0,0.6))

# latrine floor
sth.bin.em.plot(al_em_prev_lmud1_j,hw_em_prev_lmud1_j,tt_em_prev_lmud1_j,sth_em_prev_lmud1_j,
                al_em_prev_lmud0_j,hw_em_prev_lmud0_j,tt_em_prev_lmud0_j,sth_em_prev_lmud0_j,
                al_rr_h1_lmud1_j,hw_rr_h1_lmud1_j,tt_rr_h1_lmud1_j,sth_rr_h1_lmud1_j,
                al_rr_h1_lmud0_j,hw_rr_h1_lmud0_j,tt_rr_h1_lmud0_j,sth_rr_h1_lmud0_j,
               figlab="lmud",em1lab="Some mud floor in latrine",em0lab="No mud floor in latrine",
               em1labs="Some mud",em0labs="No mud",ylim=c(0,0.6))

# deworming
sth.bin.em.plot(al_em_prev_dw1_j,hw_em_prev_dw1_j,tt_em_prev_dw1_j,sth_em_prev_dw1_j,
                al_em_prev_dw0_j,hw_em_prev_dw0_j,tt_em_prev_dw0_j,sth_em_prev_dw0_j,
                al_rr_h1_dw1_j,hw_rr_h1_dw1_j,tt_rr_h1_dw1_j,sth_rr_h1_dw1_j,
                al_rr_h1_dw0_j,hw_rr_h1_dw0_j,tt_rr_h1_dw0_j,sth_rr_h1_dw0_j,
               figlab="dw",em1lab="Deworming in last 6 months",em0lab="No deworming in last 6 months",
               em1labs="Deworming",em0labs="No deworming",ylim=c(0,0.6))

# geophagia
sth.bin.em.plot(al_em_prev_geo1_j,hw_em_prev_geo1_j,tt_em_prev_geo1_j,sth_em_prev_geo1_j,
                al_em_prev_geo0_j,hw_em_prev_geo0_j,tt_em_prev_geo0_j,sth_em_prev_geo0_j,
                al_rr_h1_geo1_j,hw_rr_h1_geo1_j,tt_rr_h1_geo1_j,sth_rr_h1_geo1_j,
                al_rr_h1_geo0_j,hw_rr_h1_geo0_j,tt_rr_h1_geo0_j,sth_rr_h1_geo0_j,
               figlab="geo",em1lab="Geophagia in last 7 days",em0lab="No geophagia in last 7 days",
               em1labs="Geophagia",em0labs="No geophagia",ylim=c(0,0.6))

# shoes
sth.bin.em.plot(al_em_prev_shoe1_j,hw_em_prev_shoe1_j,tt_em_prev_shoe1_j,sth_em_prev_shoe1_j,
                al_em_prev_shoe0_j,hw_em_prev_shoe0_j,tt_em_prev_shoe0_j,sth_em_prev_shoe0_j,
                al_rr_h1_shoe1_j,hw_rr_h1_shoe1_j,tt_rr_h1_shoe1_j,sth_rr_h1_shoe1_j,
                al_rr_h1_shoe0_j,hw_rr_h1_shoe0_j,tt_rr_h1_shoe0_j,sth_rr_h1_shoe0_j,
               figlab="shoe",em1lab="Child wearing shoes",em0lab="Child not wearing shoes",
               em1labs="Shoes",em0labs="No shoes",ylim=c(0,0.6))

#---------------------------------------------
# Effect modification EPG
#---------------------------------------------
# index child
sth.epg.em.plot(al_em_geo_i1_j,hw_em_geo_i1_j,tt_em_geo_i1_j,
                al_em_geo_i0_j,hw_em_geo_i0_j,tt_em_geo_i0_j,
                al_fecr_geo_h1_i1_j,hw_fecr_geo_h1_i1_j,tt_fecr_geo_h1_i1_j,
                al_fecr_geo_h1_i0_j,hw_fecr_geo_h1_i0_j,tt_fecr_geo_h1_i0_j,
                figlab="index",em1lab="Index child",em0lab="Not index child",
                em1labs="Index child",em0labs="Not index child")

# presac vs. sac
sth.epg.em.plot(al_em_geo_psac1_j,hw_em_geo_psac1_j,tt_em_geo_psac1_j,
                al_em_geo_psac0_j,hw_em_geo_psac0_j,tt_em_geo_psac0_j,
                al_fecr_geo_h1_psac1_j,hw_fecr_geo_h1_psac1_j,tt_fecr_geo_h1_psac1_j,
                al_fecr_geo_h1_psac0_j,hw_fecr_geo_h1_psac0_j,tt_fecr_geo_h1_psac0_j,
                figlab="psac",em1lab="Pre-school aged (PSAC, 2-4 yrs)",em0lab="School aged (SAC, 5-12 yrs)",
                em1labs="PSAC",em0labs="SAC")

# number of people in compound
sth.epg.em.plot(al_em_geo_ind1_j,hw_em_geo_ind1_j,tt_em_geo_ind1_j,
                al_em_geo_ind0_j,hw_em_geo_ind0_j,tt_em_geo_ind0_j,
                al_fecr_geo_h1_ind1_j,hw_fecr_geo_h1_ind1_j,tt_fecr_geo_h1_ind1_j,
                al_fecr_geo_h1_ind0_j,hw_fecr_geo_h1_ind0_j,tt_fecr_geo_h1_ind0_j,
               figlab="ind",em1lab=">=10 people in compound",em0lab="<10 people in compound",
               em1labs=">=10",em0labs="<10")

# number of school aged children in compound
sth.epg.em.plot(al_em_geo_ch1_j,hw_em_geo_ch1_j,tt_em_geo_ch1_j,
                al_em_geo_ch0_j,hw_em_geo_ch0_j,tt_em_geo_ch0_j,
                al_fecr_geo_h1_ch1_j,hw_fecr_geo_h1_ch1_j,tt_fecr_geo_h1_ch1_j,
                al_fecr_geo_h1_ch0_j,hw_fecr_geo_h1_ch0_j,tt_fecr_geo_h1_ch0_j,
               figlab="ch",em1lab=">=1 school aged child in compound",em0lab="No school aged child in compound",
               em1labs=">=1",em0labs="0")

# defecation day
sth.epg.em.plot(al_em_geo_def1_j,hw_em_geo_def1_j,tt_em_geo_def1_j,
                al_em_geo_def0_j,hw_em_geo_def0_j,tt_em_geo_def0_j,
                al_fecr_geo_h1_def1_j,hw_fecr_geo_h1_def1_j,tt_fecr_geo_h1_def1_j,
                al_fecr_geo_h1_def0_j,hw_fecr_geo_h1_def0_j,tt_fecr_geo_h1_def0_j,
               figlab="def",em1lab="Same day defecation",em0lab="Previous day defecation",
               em1labs="Same day",em0labs="Prev day")

# latrine status at baseline
sth.epg.em.plot(al_em_geo_lat1_j,hw_em_geo_lat1_j,tt_em_geo_lat1_j,
                al_em_geo_lat0_j,hw_em_geo_lat0_j,tt_em_geo_lat0_j,
                al_fecr_geo_h1_lat1_j,hw_fecr_geo_h1_lat1_j,tt_fecr_geo_h1_lat1_j,
                al_fecr_geo_h1_lat0_j,hw_fecr_geo_h1_lat0_j,tt_fecr_geo_h1_lat0_j,
               figlab="lat",em1lab="Latrine w/ water seal and hygienic drain",em0lab="No latrine w/ water seal and hygienic drain",
               em1labs="Good latrine",em0labs="Bad latrine")

# child feces management at baseline
sth.epg.em.plot(al_em_geo_fec1_j,hw_em_geo_fec1_j,tt_em_geo_fec1_j,
                al_em_geo_fec0_j,hw_em_geo_fec0_j,tt_em_geo_fec0_j,
                al_fecr_geo_h1_fec1_j,hw_fecr_geo_h1_fec1_j,tt_fecr_geo_h1_fec1_j,
                al_fecr_geo_h1_fec0_j,hw_fecr_geo_h1_fec0_j,tt_fecr_geo_h1_fec0_j,
               figlab="fec",em1lab="Scoop used for child feces",em0lab="No scoop used for child feces",
               em1labs="Scoop",em0labs="No scoop")

# open defecation at baseline
sth.epg.em.plot(al_em_geo_odf1_j,hw_em_geo_odf1_j,tt_em_geo_odf1_j,
                al_em_geo_odf0_j,hw_em_geo_odf0_j,tt_em_geo_odf0_j,
                al_fecr_geo_h1_odf1_j,hw_fecr_geo_h1_odf1_j,tt_fecr_geo_h1_odf1_j,
                al_fecr_geo_h1_odf0_j,hw_fecr_geo_h1_odf0_j,tt_fecr_geo_h1_odf0_j,
               figlab="odf",em1lab="Compound open defecation free",em0lab="Compound not open defecation free",
               em1labs="Open defecation free",em0labs="Not open defecation free")

# wealth
sth.epg.em.plot(al_em_geo_poor1_j,hw_em_geo_poor1_j,tt_em_geo_poor1_j,
                al_em_geo_poor0_j,hw_em_geo_poor0_j,tt_em_geo_poor0_j,
                al_fecr_geo_h1_poor1_j,hw_fecr_geo_h1_poor1_j,tt_fecr_geo_h1_poor1_j,
                al_fecr_geo_h1_poor0_j,hw_fecr_geo_h1_poor0_j,tt_fecr_geo_h1_poor0_j,
               figlab="poor",em1lab="Below median wealth",em0lab="Above median wealth",
               em1labs="Below median",em0labs="Above median")

# household floor
sth.epg.em.plot(al_em_geo_hmud1_j,hw_em_geo_hmud1_j,tt_em_geo_hmud1_j,
                al_em_geo_hmud0_j,hw_em_geo_hmud0_j,tt_em_geo_hmud0_j,
                al_fecr_geo_h1_hmud1_j,hw_fecr_geo_h1_hmud1_j,tt_fecr_geo_h1_hmud1_j,
                al_fecr_geo_h1_hmud0_j,hw_fecr_geo_h1_hmud0_j,tt_fecr_geo_h1_hmud0_j,
               figlab="hmud",em1lab="Some mud floor in household",em0lab="No mud floor in household",
               em1labs="Some mud",em0labs="No mud")

# latrine floor
sth.epg.em.plot(al_em_geo_lmud1_j,hw_em_geo_lmud1_j,tt_em_geo_lmud1_j,
                al_em_geo_lmud0_j,hw_em_geo_lmud0_j,tt_em_geo_lmud0_j,
                al_fecr_geo_h1_lmud1_j,hw_fecr_geo_h1_lmud1_j,tt_fecr_geo_h1_lmud1_j,
                al_fecr_geo_h1_lmud0_j,hw_fecr_geo_h1_lmud0_j,tt_fecr_geo_h1_lmud0_j,
               figlab="lmud",em1lab="Some mud floor in latrine",em0lab="No mud floor in latrine",
               em1labs="Some mud",em0labs="No mud")

# deworming
sth.epg.em.plot(al_em_geo_dw1_j,hw_em_geo_dw1_j,tt_em_geo_dw1_j,
                al_em_geo_dw0_j,hw_em_geo_dw0_j,tt_em_geo_dw0_j,
                al_fecr_geo_h1_dw1_j,hw_fecr_geo_h1_dw1_j,tt_fecr_geo_h1_dw1_j,
                al_fecr_geo_h1_dw0_j,hw_fecr_geo_h1_dw0_j,tt_fecr_geo_h1_dw0_j,
               figlab="dw",em1lab="Deworming in last 6 months",em0lab="No deworming in last 6 months",
               em1labs="Deworming",em0labs="No deworming")

# geophagia
sth.epg.em.plot(al_em_geo_geo1_j,hw_em_geo_geo1_j,tt_em_geo_geo1_j,
                al_em_geo_geo0_j,hw_em_geo_geo0_j,tt_em_geo_geo0_j,
                al_fecr_geo_h1_geo1_j,hw_fecr_geo_h1_geo1_j,tt_fecr_geo_h1_geo1_j,
                al_fecr_geo_h1_geo0_j,hw_fecr_geo_h1_geo0_j,tt_fecr_geo_h1_geo0_j,
               figlab="geo",em1lab="Geophagia in last 7 days",em0lab="No geophagia in last 7 days",
               em1labs="Geophagia",em0labs="No geophagia")

# shoes
sth.epg.em.plot(al_em_geo_shoe1_j,hw_em_geo_shoe1_j,tt_em_geo_shoe1_j,
                al_em_geo_shoe0_j,hw_em_geo_shoe0_j,tt_em_geo_shoe0_j,
                al_fecr_geo_h1_shoe1_j,hw_fecr_geo_h1_shoe1_j,tt_fecr_geo_h1_shoe1_j,
                al_fecr_geo_h1_shoe0_j,hw_fecr_geo_h1_shoe0_j,tt_fecr_geo_h1_shoe0_j,
               figlab="shoe",em1lab="Child wearing shoes",em0lab="Child not wearing shoes",
               em1labs="Shoes",em0labs="No shoes")


# notes for Ayse

# figlab= the name to add to the pdf file name for that figure - preferably with no spaces
# em1lab is the title for the EM level 1 panel
# em0lab is the title for the EM level 0 panel