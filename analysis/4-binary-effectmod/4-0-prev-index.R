##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH 
# n, N, prevalence, and 95% CI by arm 

# Subgroup analyses

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
##############################################
rm(list=ls())
source(here::here("0-config.R"))

#----------------------------------------------
# load and pre-process analysis dataset 
#----------------------------------------------
data = read.csv(sth_data_path,stringsAsFactors=TRUE)

d=preprocess.sth(data)

#----------------------------------------------
# estimate prevalence in subgroups
#----------------------------------------------
# index child
al_em_prev_i1_j=emprev(d,"index")$out1.al
hw_em_prev_i1_j=emprev(d,"index")$out1.hw
tt_em_prev_i1_j=emprev(d,"index")$out1.tt
sth_em_prev_i1_j=emprev(d,"index")$out1.sth

al_em_prev_i0_j=emprev(d,"index")$out0.al
hw_em_prev_i0_j=emprev(d,"index")$out0.hw
tt_em_prev_i0_j=emprev(d,"index")$out0.tt
sth_em_prev_i0_j=emprev(d,"index")$out0.sth

# school age child
al_em_prev_psac1_j=emprev(d,"sac")$out0.al
hw_em_prev_psac1_j=emprev(d,"sac")$out0.hw
tt_em_prev_psac1_j=emprev(d,"sac")$out0.tt
sth_em_prev_psac1_j=emprev(d,"sac")$out0.sth

al_em_prev_psac0_j=emprev(d,"sac")$out1.al
hw_em_prev_psac0_j=emprev(d,"sac")$out1.hw
tt_em_prev_psac0_j=emprev(d,"sac")$out1.tt
sth_em_prev_psac0_j=emprev(d,"sac")$out1.sth

# N compound members
al_em_prev_ind1_j=emprev(d,"ncompover10")$out1.al
hw_em_prev_ind1_j=emprev(d,"ncompover10")$out1.hw
tt_em_prev_ind1_j=emprev(d,"ncompover10")$out1.tt
sth_em_prev_ind1_j=emprev(d,"ncompover10")$out1.sth

al_em_prev_ind0_j=emprev(d,"ncompover10")$out0.al
hw_em_prev_ind0_j=emprev(d,"ncompover10")$out0.hw
tt_em_prev_ind0_j=emprev(d,"ncompover10")$out0.tt
sth_em_prev_ind0_j=emprev(d,"ncompover10")$out0.sth

# N 5 to 14 years 
al_em_prev_ch1_j=emprev(d,"n5to14")$out1.al
hw_em_prev_ch1_j=emprev(d,"n5to14")$out1.hw
tt_em_prev_ch1_j=emprev(d,"n5to14")$out1.tt
sth_em_prev_ch1_j=emprev(d,"n5to14")$out1.sth

al_em_prev_ch0_j=emprev(d,"n5to14")$out0.al
hw_em_prev_ch0_j=emprev(d,"n5to14")$out0.hw
tt_em_prev_ch0_j=emprev(d,"n5to14")$out0.tt
sth_em_prev_ch0_j=emprev(d,"n5to14")$out0.sth

# defecated same day as KK
d$defday_rec=ifelse(d$defday=="Today",1,0)
d$defday_rec[d$defday==""]=NA
al_em_prev_def1_j=emprev(d,"defday_rec")$out1.al
hw_em_prev_def1_j=emprev(d,"defday_rec")$out1.hw
tt_em_prev_def1_j=emprev(d,"defday_rec")$out1.tt
sth_em_prev_def1_j=emprev(d,"defday_rec")$out1.sth

al_em_prev_def0_j=emprev(d,"defday_rec")$out0.al
hw_em_prev_def0_j=emprev(d,"defday_rec")$out0.hw
tt_em_prev_def0_j=emprev(d,"defday_rec")$out0.tt
sth_em_prev_def0_j=emprev(d,"defday_rec")$out0.sth

# latrine with seal and flush 
al_em_prev_lat1_j=emprev(d,"lat")$out1.al
hw_em_prev_lat1_j=emprev(d,"lat")$out1.hw
tt_em_prev_lat1_j=emprev(d,"lat")$out1.tt
sth_em_prev_lat1_j=emprev(d,"lat")$out1.sth

al_em_prev_lat0_j=emprev(d,"lat")$out0.al
hw_em_prev_lat0_j=emprev(d,"lat")$out0.hw
tt_em_prev_lat0_j=emprev(d,"lat")$out0.tt
sth_em_prev_lat0_j=emprev(d,"lat")$out0.sth

# dedicated scoop
al_em_prev_fec1_j=emprev(d,"scoop")$out1.al
hw_em_prev_fec1_j=emprev(d,"scoop")$out1.hw
tt_em_prev_fec1_j=emprev(d,"scoop")$out1.tt
sth_em_prev_fec1_j=emprev(d,"scoop")$out1.sth

al_em_prev_fec0_j=emprev(d,"scoop")$out0.al
hw_em_prev_fec0_j=emprev(d,"scoop")$out0.hw
tt_em_prev_fec0_j=emprev(d,"scoop")$out0.tt
sth_em_prev_fec0_j=emprev(d,"scoop")$out0.sth

# no open defecation
al_em_prev_odf1_j=emprev(d,"noopendef")$out1.al
hw_em_prev_odf1_j=emprev(d,"noopendef")$out1.hw
tt_em_prev_odf1_j=emprev(d,"noopendef")$out1.tt
sth_em_prev_odf1_j=emprev(d,"noopendef")$out1.sth

al_em_prev_odf0_j=emprev(d,"noopendef")$out0.al
hw_em_prev_odf0_j=emprev(d,"noopendef")$out0.hw
tt_em_prev_odf0_j=emprev(d,"noopendef")$out0.tt
sth_em_prev_odf0_j=emprev(d,"noopendef")$out0.sth

# wealth index
d$wealth_recode=ifelse(d$wealth==1,1,0)
al_em_prev_poor1_j=emprev(d,"wealth_recode")$out1.al
hw_em_prev_poor1_j=emprev(d,"wealth_recode")$out1.hw
tt_em_prev_poor1_j=emprev(d,"wealth_recode")$out1.tt
sth_em_prev_poor1_j=emprev(d,"wealth_recode")$out1.sth

al_em_prev_poor0_j=emprev(d,"wealth_recode")$out0.al
hw_em_prev_poor0_j=emprev(d,"wealth_recode")$out0.hw
tt_em_prev_poor0_j=emprev(d,"wealth_recode")$out0.tt
sth_em_prev_poor0_j=emprev(d,"wealth_recode")$out0.sth

# household dirt floor
al_em_prev_hmud1_j=emprev(d,"dirtfloor_hh")$out1.al
hw_em_prev_hmud1_j=emprev(d,"dirtfloor_hh")$out1.hw
tt_em_prev_hmud1_j=emprev(d,"dirtfloor_hh")$out1.tt
sth_em_prev_hmud1_j=emprev(d,"dirtfloor_hh")$out1.sth

al_em_prev_hmud0_j=emprev(d,"dirtfloor_hh")$out0.al
hw_em_prev_hmud0_j=emprev(d,"dirtfloor_hh")$out0.hw
tt_em_prev_hmud0_j=emprev(d,"dirtfloor_hh")$out0.tt
sth_em_prev_hmud0_j=emprev(d,"dirtfloor_hh")$out0.sth

# latrine dirt floor
al_em_prev_lmud1_j=emprev(d,"dirtfloor_lat")$out1.al
hw_em_prev_lmud1_j=emprev(d,"dirtfloor_lat")$out1.hw
tt_em_prev_lmud1_j=emprev(d,"dirtfloor_lat")$out1.tt
sth_em_prev_lmud1_j=emprev(d,"dirtfloor_lat")$out1.sth

al_em_prev_lmud0_j=emprev(d,"dirtfloor_lat")$out0.al
hw_em_prev_lmud0_j=emprev(d,"dirtfloor_lat")$out0.hw
tt_em_prev_lmud0_j=emprev(d,"dirtfloor_lat")$out0.tt
sth_em_prev_lmud0_j=emprev(d,"dirtfloor_lat")$out0.sth

# deworming
al_em_prev_dw1_j=emprev(d,"dw")$out1.al
hw_em_prev_dw1_j=emprev(d,"dw")$out1.hw
tt_em_prev_dw1_j=emprev(d,"dw")$out1.tt
sth_em_prev_dw1_j=emprev(d,"dw")$out1.sth

al_em_prev_dw0_j=emprev(d,"dw")$out0.al
hw_em_prev_dw0_j=emprev(d,"dw")$out0.hw
tt_em_prev_dw0_j=emprev(d,"dw")$out0.tt
sth_em_prev_dw0_j=emprev(d,"dw")$out0.sth

# geophagia
d$geoph=ifelse(d$geophagia=="yes",1,0)
d$geoph[d$geophagia==""]=NA
al_em_prev_geo1_j=emprev(d,"geoph")$out1.al
hw_em_prev_geo1_j=emprev(d,"geoph")$out1.hw
tt_em_prev_geo1_j=emprev(d,"geoph")$out1.tt
sth_em_prev_geo1_j=emprev(d,"geoph")$out1.sth

al_em_prev_geo0_j=emprev(d,"geoph")$out0.al
hw_em_prev_geo0_j=emprev(d,"geoph")$out0.hw
tt_em_prev_geo0_j=emprev(d,"geoph")$out0.tt
sth_em_prev_geo0_j=emprev(d,"geoph")$out0.sth

# shoes
al_em_prev_shoe1_j=emprev(d,"shoes")$out1.al
hw_em_prev_shoe1_j=emprev(d,"shoes")$out1.hw
tt_em_prev_shoe1_j=emprev(d,"shoes")$out1.tt
sth_em_prev_shoe1_j=emprev(d,"shoes")$out1.sth

al_em_prev_shoe0_j=emprev(d,"shoes")$out0.al
hw_em_prev_shoe0_j=emprev(d,"shoes")$out0.hw
tt_em_prev_shoe0_j=emprev(d,"shoes")$out0.tt
sth_em_prev_shoe0_j=emprev(d,"shoes")$out0.sth

rm(d, data)

save.image(file=paste0(save_data_path, "sth_prev_em.RData"))

