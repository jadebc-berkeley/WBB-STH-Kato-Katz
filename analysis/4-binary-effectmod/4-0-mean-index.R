##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH 
# n, N, prevalence, and 95% CI by arm 

# by Jade
##############################################
rm(list=ls())
library(washb)

data=read.csv("~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv")
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")

d=preprocess.sth(data)

# index child
al_em_ari_i1_j=em_arimean(d,"index")$out1.al
hw_em_ari_i1_j=em_arimean(d,"index")$out1.hw
tt_em_ari_i1_j=em_arimean(d,"index")$out1.tt

al_em_ari_i0_j=em_arimean(d,"index")$out0.al
hw_em_ari_i0_j=em_arimean(d,"index")$out0.hw
tt_em_ari_i0_j=em_arimean(d,"index")$out0.tt

al_em_geo_i1_j=em_geomean(d,"index")$out1.al
hw_em_geo_i1_j=em_geomean(d,"index")$out1.hw
tt_em_geo_i1_j=em_geomean(d,"index")$out1.tt

al_em_geo_i0_j=em_geomean(d,"index")$out0.al
hw_em_geo_i0_j=em_geomean(d,"index")$out0.hw
tt_em_geo_i0_j=em_geomean(d,"index")$out0.tt

# school age child
al_em_ari_psac1_j=em_arimean(d,"sac")$out0.al
hw_em_ari_psac1_j=em_arimean(d,"sac")$out0.hw
tt_em_ari_psac1_j=em_arimean(d,"sac")$out0.tt

al_em_ari_psac0_j=em_arimean(d,"sac")$out1.al
hw_em_ari_psac0_j=em_arimean(d,"sac")$out1.hw
tt_em_ari_psac0_j=em_arimean(d,"sac")$out1.tt

al_em_geo_psac1_j=em_geomean(d,"sac")$out0.al
hw_em_geo_psac1_j=em_geomean(d,"sac")$out0.hw
tt_em_geo_psac1_j=em_geomean(d,"sac")$out0.tt

al_em_geo_psac0_j=em_geomean(d,"sac")$out1.al
hw_em_geo_psac0_j=em_geomean(d,"sac")$out1.hw
tt_em_geo_psac0_j=em_geomean(d,"sac")$out1.tt

# N compound members
al_em_ari_ind1_j=em_arimean(d,"ncompover10")$out1.al
hw_em_ari_ind1_j=em_arimean(d,"ncompover10")$out1.hw
tt_em_ari_ind1_j=em_arimean(d,"ncompover10")$out1.tt

al_em_ari_ind0_j=em_arimean(d,"ncompover10")$out0.al
hw_em_ari_ind0_j=em_arimean(d,"ncompover10")$out0.hw
tt_em_ari_ind0_j=em_arimean(d,"ncompover10")$out0.tt

al_em_geo_ind1_j=em_geomean(d,"ncompover10")$out1.al
hw_em_geo_ind1_j=em_geomean(d,"ncompover10")$out1.hw
tt_em_geo_ind1_j=em_geomean(d,"ncompover10")$out1.tt

al_em_geo_ind0_j=em_geomean(d,"ncompover10")$out0.al
hw_em_geo_ind0_j=em_geomean(d,"ncompover10")$out0.hw
tt_em_geo_ind0_j=em_geomean(d,"ncompover10")$out0.tt

# N 5 to 14 years 
al_em_ari_ch1_j=em_arimean(d,"n5to14")$out1.al
hw_em_ari_ch1_j=em_arimean(d,"n5to14")$out1.hw
tt_em_ari_ch1_j=em_arimean(d,"n5to14")$out1.tt

al_em_ari_ch0_j=em_arimean(d,"n5to14")$out0.al
hw_em_ari_ch0_j=em_arimean(d,"n5to14")$out0.hw
tt_em_ari_ch0_j=em_arimean(d,"n5to14")$out0.tt

al_em_geo_ch1_j=em_geomean(d,"n5to14")$out1.al
hw_em_geo_ch1_j=em_geomean(d,"n5to14")$out1.hw
tt_em_geo_ch1_j=em_geomean(d,"n5to14")$out1.tt

al_em_geo_ch0_j=em_geomean(d,"n5to14")$out0.al
hw_em_geo_ch0_j=em_geomean(d,"n5to14")$out0.hw
tt_em_geo_ch0_j=em_geomean(d,"n5to14")$out0.tt

# defecated same day as KK
d$defday_rec=ifelse(d$defday=="Today",1,0)
d$defday_rec[d$defday==""]=NA
al_em_ari_def1_j=em_arimean(d,"defday_rec")$out1.al
hw_em_ari_def1_j=em_arimean(d,"defday_rec")$out1.hw
tt_em_ari_def1_j=em_arimean(d,"defday_rec")$out1.tt

al_em_ari_def0_j=em_arimean(d,"defday_rec")$out0.al
hw_em_ari_def0_j=em_arimean(d,"defday_rec")$out0.hw
tt_em_ari_def0_j=em_arimean(d,"defday_rec")$out0.tt

al_em_geo_def1_j=em_geomean(d,"defday_rec")$out1.al
hw_em_geo_def1_j=em_geomean(d,"defday_rec")$out1.hw
tt_em_geo_def1_j=em_geomean(d,"defday_rec")$out1.tt

al_em_geo_def0_j=em_geomean(d,"defday_rec")$out0.al
hw_em_geo_def0_j=em_geomean(d,"defday_rec")$out0.hw
tt_em_geo_def0_j=em_geomean(d,"defday_rec")$out0.tt

# latrine with seal and flush 
al_em_ari_lat1_j=em_arimean(d,"lat")$out1.al
hw_em_ari_lat1_j=em_arimean(d,"lat")$out1.hw
tt_em_ari_lat1_j=em_arimean(d,"lat")$out1.tt

al_em_ari_lat0_j=em_arimean(d,"lat")$out0.al
hw_em_ari_lat0_j=em_arimean(d,"lat")$out0.hw
tt_em_ari_lat0_j=em_arimean(d,"lat")$out0.tt

al_em_geo_lat1_j=em_geomean(d,"lat")$out1.al
hw_em_geo_lat1_j=em_geomean(d,"lat")$out1.hw
tt_em_geo_lat1_j=em_geomean(d,"lat")$out1.tt

al_em_geo_lat0_j=em_geomean(d,"lat")$out0.al
hw_em_geo_lat0_j=em_geomean(d,"lat")$out0.hw
tt_em_geo_lat0_j=em_geomean(d,"lat")$out0.tt

# dedicated scoop
al_em_ari_fec1_j=em_arimean(d,"scoop")$out1.al
hw_em_ari_fec1_j=em_arimean(d,"scoop")$out1.hw
tt_em_ari_fec1_j=em_arimean(d,"scoop")$out1.tt

al_em_ari_fec0_j=em_arimean(d,"scoop")$out0.al
hw_em_ari_fec0_j=em_arimean(d,"scoop")$out0.hw
tt_em_ari_fec0_j=em_arimean(d,"scoop")$out0.tt

al_em_geo_fec1_j=em_geomean(d,"scoop")$out1.al
hw_em_geo_fec1_j=em_geomean(d,"scoop")$out1.hw
tt_em_geo_fec1_j=em_geomean(d,"scoop")$out1.tt

al_em_geo_fec0_j=em_geomean(d,"scoop")$out0.al
hw_em_geo_fec0_j=em_geomean(d,"scoop")$out0.hw
tt_em_geo_fec0_j=em_geomean(d,"scoop")$out0.tt

# no open defecation
al_em_ari_odf1_j=em_arimean(d,"noopendef")$out1.al
hw_em_ari_odf1_j=em_arimean(d,"noopendef")$out1.hw
tt_em_ari_odf1_j=em_arimean(d,"noopendef")$out1.tt

al_em_ari_odf0_j=em_arimean(d,"noopendef")$out0.al
hw_em_ari_odf0_j=em_arimean(d,"noopendef")$out0.hw
tt_em_ari_odf0_j=em_arimean(d,"noopendef")$out0.tt

al_em_geo_odf1_j=em_geomean(d,"noopendef")$out1.al
hw_em_geo_odf1_j=em_geomean(d,"noopendef")$out1.hw
tt_em_geo_odf1_j=em_geomean(d,"noopendef")$out1.tt

al_em_geo_odf0_j=em_geomean(d,"noopendef")$out0.al
hw_em_geo_odf0_j=em_geomean(d,"noopendef")$out0.hw
tt_em_geo_odf0_j=em_geomean(d,"noopendef")$out0.tt

# wealth index
d$wealth_recode=ifelse(d$wealth==1,1,0)
al_em_ari_poor1_j=em_arimean(d,"wealth_recode")$out1.al
hw_em_ari_poor1_j=em_arimean(d,"wealth_recode")$out1.hw
tt_em_ari_poor1_j=em_arimean(d,"wealth_recode")$out1.tt

al_em_ari_poor0_j=em_arimean(d,"wealth_recode")$out0.al
hw_em_ari_poor0_j=em_arimean(d,"wealth_recode")$out0.hw
tt_em_ari_poor0_j=em_arimean(d,"wealth_recode")$out0.tt

al_em_geo_poor1_j=em_geomean(d,"wealth_recode")$out1.al
hw_em_geo_poor1_j=em_geomean(d,"wealth_recode")$out1.hw
tt_em_geo_poor1_j=em_geomean(d,"wealth_recode")$out1.tt

al_em_geo_poor0_j=em_geomean(d,"wealth_recode")$out0.al
hw_em_geo_poor0_j=em_geomean(d,"wealth_recode")$out0.hw
tt_em_geo_poor0_j=em_geomean(d,"wealth_recode")$out0.tt

# household dirt floor
al_em_ari_hmud1_j=em_arimean(d,"dirtfloor_hh")$out1.al
hw_em_ari_hmud1_j=em_arimean(d,"dirtfloor_hh")$out1.hw
tt_em_ari_hmud1_j=em_arimean(d,"dirtfloor_hh")$out1.tt

al_em_ari_hmud0_j=em_arimean(d,"dirtfloor_hh")$out0.al
hw_em_ari_hmud0_j=em_arimean(d,"dirtfloor_hh")$out0.hw
tt_em_ari_hmud0_j=em_arimean(d,"dirtfloor_hh")$out0.tt

al_em_geo_hmud1_j=em_geomean(d,"dirtfloor_hh")$out1.al
hw_em_geo_hmud1_j=em_geomean(d,"dirtfloor_hh")$out1.hw
tt_em_geo_hmud1_j=em_geomean(d,"dirtfloor_hh")$out1.tt

al_em_geo_hmud0_j=em_geomean(d,"dirtfloor_hh")$out0.al
hw_em_geo_hmud0_j=em_geomean(d,"dirtfloor_hh")$out0.hw
tt_em_geo_hmud0_j=em_geomean(d,"dirtfloor_hh")$out0.tt

# latrine dirt floor
al_em_ari_lmud1_j=em_arimean(d,"dirtfloor_lat")$out1.al
hw_em_ari_lmud1_j=em_arimean(d,"dirtfloor_lat")$out1.hw
tt_em_ari_lmud1_j=em_arimean(d,"dirtfloor_lat")$out1.tt

al_em_ari_lmud0_j=em_arimean(d,"dirtfloor_lat")$out0.al
hw_em_ari_lmud0_j=em_arimean(d,"dirtfloor_lat")$out0.hw
tt_em_ari_lmud0_j=em_arimean(d,"dirtfloor_lat")$out0.tt

al_em_geo_lmud1_j=em_geomean(d,"dirtfloor_lat")$out1.al
hw_em_geo_lmud1_j=em_geomean(d,"dirtfloor_lat")$out1.hw
tt_em_geo_lmud1_j=em_geomean(d,"dirtfloor_lat")$out1.tt

al_em_geo_lmud0_j=em_geomean(d,"dirtfloor_lat")$out0.al
hw_em_geo_lmud0_j=em_geomean(d,"dirtfloor_lat")$out0.hw
tt_em_geo_lmud0_j=em_geomean(d,"dirtfloor_lat")$out0.tt

# deworming
al_em_ari_dw1_j=em_arimean(d,"dw")$out1.al
hw_em_ari_dw1_j=em_arimean(d,"dw")$out1.hw
tt_em_ari_dw1_j=em_arimean(d,"dw")$out1.tt

al_em_ari_dw0_j=em_arimean(d,"dw")$out0.al
hw_em_ari_dw0_j=em_arimean(d,"dw")$out0.hw
tt_em_ari_dw0_j=em_arimean(d,"dw")$out0.tt

al_em_geo_dw1_j=em_geomean(d,"dw")$out1.al
hw_em_geo_dw1_j=em_geomean(d,"dw")$out1.hw
tt_em_geo_dw1_j=em_geomean(d,"dw")$out1.tt

al_em_geo_dw0_j=em_geomean(d,"dw")$out0.al
hw_em_geo_dw0_j=em_geomean(d,"dw")$out0.hw
tt_em_geo_dw0_j=em_geomean(d,"dw")$out0.tt

# geophagia
d$geoph=ifelse(d$geophagia=="yes",1,0)
d$geoph[d$geophagia==""]=NA
al_em_ari_geo1_j=em_arimean(d,"geoph")$out1.al
hw_em_ari_geo1_j=em_arimean(d,"geoph")$out1.hw
tt_em_ari_geo1_j=em_arimean(d,"geoph")$out1.tt

al_em_ari_geo0_j=em_arimean(d,"geoph")$out0.al
hw_em_ari_geo0_j=em_arimean(d,"geoph")$out0.hw
tt_em_ari_geo0_j=em_arimean(d,"geoph")$out0.tt

# shoes
al_em_ari_shoe1_j=em_arimean(d,"shoes")$out1.al
hw_em_ari_shoe1_j=em_arimean(d,"shoes")$out1.hw
tt_em_ari_shoe1_j=em_arimean(d,"shoes")$out1.tt

al_em_ari_shoe0_j=em_arimean(d,"shoes")$out0.al
hw_em_ari_shoe0_j=em_arimean(d,"shoes")$out0.hw
tt_em_ari_shoe0_j=em_arimean(d,"shoes")$out0.tt

al_em_geo_shoe1_j=em_geomean(d,"shoes")$out1.al
hw_em_geo_shoe1_j=em_geomean(d,"shoes")$out1.hw
tt_em_geo_shoe1_j=em_geomean(d,"shoes")$out1.tt

al_em_geo_shoe0_j=em_geomean(d,"shoes")$out0.al
hw_em_geo_shoe0_j=em_geomean(d,"shoes")$out0.hw
tt_em_geo_shoe0_j=em_geomean(d,"shoes")$out0.tt

rm(d, data)

save.image(file="~/Box Sync/WASHB Parasites/Results/Jade/sth_mean_em.RData")


