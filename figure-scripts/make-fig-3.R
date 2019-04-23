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

# Unadjusted prevalence ratios
load(paste0(save_data_path, "sth_pr_unadj_tmle.RData"))

#---------------------------------------------
# Create plot
#---------------------------------------------
alprev=sth.plot.prep(al_prev)
hwprev=sth.plot.prep(hw_prev)
ttprev=sth.plot.prep(tt_prev)
sthprev=sth.plot.prep(sth_prev)

alplot=makeplot(n = psth_n_prev_j$N.al,
                prev = alprev,
                prh1 = al_rr_h1_unadj_tmle_j,
                ytitle = "A. lumbricoides",
                ylim = ylim)

hwplot = makeplot(n = psth_n_prev_j$N.hw, 
                  prev = hwprev, 
                  prh1 = hw_rr_h1_unadj_tmle_j, 
                  ytitle = "Hookworm", 
                  ylim = ylim)

ttplot = makeplot(n = psth_n_prev_j$N.tt, 
                  prev = ttprev, 
                  prh1 = tt_rr_h1_unadj_tmle_j, 
                  ytitle = "T. trichiura", 
                  ylim = ylim)

pdf(paste0(figure_path,"fig-PR-primary-tmle.pdf"),width=7.2,height=8)
grid.arrange(alplot,hwplot,ttplot,ncol=1,nrow=3)
dev.off()

