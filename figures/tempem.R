rm(list=ls())

source("~/Box Sync/WASHB Parasites/Scripts/Figures/0-base-plot-functions.R")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_index.RData")
load("~/Box Sync/WASHB Parasites/Results/Jade/sth_mean_em.RData")

sth.epg.em.plot(al_em_geo_i1_j,hw_em_geo_i1_j,tt_em_geo_i1_j,
                al_em_geo_i0_j,hw_em_geo_i0_j,tt_em_geo_i0_j,
                al_fecr_geo_h1_i1_j,hw_fecr_geo_h1_i1_j,tt_fecr_geo_h1_i1_j,
                al_fecr_geo_h1_i0_j,hw_fecr_geo_h1_i0_j,tt_fecr_geo_h1_i0_j,
                figlab="index",em1lab="Index child",em0lab="Not index child")

almn1=al_em_geo_i1_j
hwmn1=hw_em_geo_i1_j
ttmn1=tt_em_geo_i1_j
almn0=al_em_geo_i0_j
hwmn0=hw_em_geo_i0_j
ttmn0=tt_em_geo_i0_j

alprh11=al_fecr_geo_h1_i1_j
hwprh11=hw_fecr_geo_h1_i1_j
ttprh11=tt_fecr_geo_h1_i1_j
alprh10=al_fecr_geo_h1_i0_j
hwprh10=hw_fecr_geo_h1_i0_j
ttprh10=tt_fecr_geo_h1_i0_j

file="~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_index.RData")
