##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Plot of unadjusted primary outcome estimates 
# Corresponds to manuscript Figure 3

# by Jade

# Output: fig-prev-primary-tmle.pdf
##############################################
rm(list=ls())

source(here::here("0-config.R"))

#---------------------------------------------
# Load data
#---------------------------------------------
# Descriptive
load(paste0(save_data_path, "sth_prev.RData"))

# Unadjusted
load(paste0(save_data_path, "sth_pr_unadj.RData"))
load(paste0(save_data_path, "sth_mh_pr_unadj.RData"))
load(paste0(save_data_path, "sth_pr_epg_unadj.RData"))
load(paste0(save_data_path, "sth_pr_unadj_tmle.RData"))

#---------------------------------------------
# Create plot
#---------------------------------------------

sth.bin.plot(psth_n_prev_j$N.al,psth_n_prev_j$N.hw,psth_n_prev_j$N.tt,psth_n_prev_j$N.sth,
             al_prev,hw_prev,tt_prev,sth_prev,
             al_rr_h1_unadj_tmle_j,al_rr_h2_unadj_tmle_j,al_rr_h3_unadj_tmle_j,
             hw_rr_h1_unadj_tmle_j,hw_rr_h2_unadj_tmle_j,hw_rr_h3_unadj_tmle_j,
             tt_rr_h1_unadj_tmle_j,tt_rr_h2_unadj_tmle_j,tt_rr_h3_unadj_tmle_j,
             sth_rr_h1_unadj_tmle_j,sth_rr_h2_unadj_tmle_j,sth_rr_h3_unadj_tmle_j,
             lab="primary-tmle",
             fig_dir = figure_path)