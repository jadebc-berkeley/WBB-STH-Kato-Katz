##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Plot of unadjusted eggs per gram 
# primary outcome estimates 
# Corresponds to manuscript Figure 4

# by Jade

# Output: fig-epg-primary.pdf
##############################################
rm(list=ls())

source(here::here("0-config.R"))

#---------------------------------------------
# Load data
#---------------------------------------------
# Descriptive
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_prev.RData")

# Unadjusted fecal egg count ratios
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_unadj.RData")

#---------------------------------------------
# Create plot
#---------------------------------------------
sth.epg.plot(psth_n_int_j$N.int.al,psth_n_int_j$N.int.hw,psth_n_int_j$N.int.tt,
             al_int_gmn,hw_int_gmn,tt_int_gmn,
             al_fecr_geo_h1_unadj_j,al_fecr_geo_h2_unadj_j,al_fecr_geo_h3_unadj_j,
             hw_fecr_geo_h1_unadj_j,hw_fecr_geo_h2_unadj_j,hw_fecr_geo_h3_unadj_j,
             tt_fecr_geo_h1_unadj_j,tt_fecr_geo_h2_unadj_j,tt_fecr_geo_h3_unadj_j,
             lab="primary",
             fig_dir = figure_path)
