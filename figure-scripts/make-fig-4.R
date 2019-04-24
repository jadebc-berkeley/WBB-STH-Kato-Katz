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
almn=sth.epg.plot.prep(al_int_gmn)
hwmn=sth.epg.plot.prep(hw_int_gmn)
ttmn=sth.epg.plot.prep(tt_int_gmn)

alplot=makeepgplot(n = psth_n_int_j$N.int.al,
                   mn = almn,
                   ytitle="Ascaris")

hwplot=makeepgplot(n = psth_n_int_j$N.int.hw,
                   mn = hwmn,
                   ytitle="Hookworm")

ttplot=makeepgplot(n = psth_n_int_j$N.int.tt,
                   mn = ttmn,
                   ytitle="Trichuris")

pdf(paste0(figure_path,"fig-epg-primary.pdf"),width=7.2,height=8)
grid.arrange(alplot,hwplot,ttplot,ncol=1,nrow=3)
dev.off()

