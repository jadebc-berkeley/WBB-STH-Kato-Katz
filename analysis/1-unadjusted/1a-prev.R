##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# STH 
# n, N, prevalence, and 95% CI by arm 

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
##############################################
rm(list=ls())
source(here::here("0-config.R"))

#----------------------------------------------
# load and pre-process analysis dataset 
#----------------------------------------------
data = read.csv(sth_data_path)

d=preprocess.sth(data)

#----------------------------------------------
# n and N for prevalence by arm
#----------------------------------------------
N.al=table(d$tr[!is.na(d$al)])
N.hw=table(d$tr[!is.na(d$hw)])
N.tt=table(d$tr[!is.na(d$tt)])
N.sth=table(d$tr[!is.na(d$sth)])

n.al=table(d$al,d$tr)[2,]
n.hw=table(d$hw,d$tr)[2,]
n.tt=table(d$tt,d$tr)[2,]
n.sth=table(d$sth,d$tr)[2,]

psth_n_prev_j=data.frame(cbind(n.al,N.al,n.hw,N.hw,
                               n.tt,N.tt,n.sth,N.sth))

#----------------------------------------------
# n and N for intensity by arm
#----------------------------------------------
N.int.al=table(d$tr[!is.na(d$alepg)])
N.int.hw=table(d$tr[!is.na(d$hwepg)])
N.int.tt=table(d$tr[!is.na(d$ttepg)])

psth_n_int_j=data.frame(cbind(N.int.al,N.int.hw,N.int.tt))

#----------------------------------------------
# prevalence by arm and time point
#----------------------------------------------
al_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$al[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
hw_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$hw[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
tt_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$tt[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
sth_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$sth[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(al_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_prev)=c("N","Prev","SD","Robust SE","lb","ub")

psth_prev_j=cbind(al_prev[,c("Prev","lb","ub")],
                  hw_prev[,c("Prev","lb","ub")],
                  tt_prev[,c("Prev","lb","ub")],
                  sth_prev[,c("Prev","lb","ub")])

colnames(psth_prev_j)=c("al-prev","al-lb","al-ub",
                        "hw-prev","hw-lb","hw-ub",
                        "tt-prev","tt-lb","tt-ub",
                        "sth-prev","sth-lb","sth-ub")

#----------------------------------------------
# prevalence of moderate/heavy infection by arm and time point
#----------------------------------------------
al_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$almh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
hw_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$hwmh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
tt_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ttmh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
sth_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$sthmh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(al_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(sth_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")

psth_prev_mh_j=cbind(al_mh_prev[,c("Prev","lb","ub")],
                     hw_mh_prev[,c("Prev","lb","ub")],
                      tt_mh_prev[,c("Prev","lb","ub")],
                     sth_mh_prev[,c("Prev","lb","ub")])

colnames(psth_prev_mh_j)=c("al-prev","al-lb","al-ub",
                           "hw-prev","hw-lb","hw-ub",
                           "tt-prev","tt-lb","tt-ub",
                           "sth-prev","sth-lb","sth-ub")


#----------------------------------------------
# arithmetic mean of intensity by arm and time point
# geo mean = exp[Sum(log_e(c+1))/n] – 1
#----------------------------------------------
al_int_mn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$alepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
hw_int_mn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$hwepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
tt_int_mn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ttepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(al_int_mn)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_int_mn)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_int_mn)=c("N","Prev","SD","Robust SE","lb","ub")

psth_mean_j=cbind(al_int_mn[,c("Prev","lb","ub")],
                  hw_int_mn[,c("Prev","lb","ub")],
                  tt_int_mn[,c("Prev","lb","ub")])

colnames(psth_mean_j)=c("al-mean","al-lb","al-ub","hw-mean",
      "hw-lb","hw-ub","tt-mean","tt-lb","tt-ub")
rownames(psth_mean_j)=levels(d$tr)


#----------------------------------------------
# geometric mean of intensity by arm and time point
# geo mean = exp[Sum(log_e(c+1))/n] – 1
#----------------------------------------------
d$ln.alepg=log(d$alepg+1)
d$ln.hwepg=log(d$hwepg+1)
d$ln.ttepg=log(d$ttepg+1)

al_int_gmn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ln.alepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
hw_int_gmn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ln.hwepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
tt_int_gmn=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ln.ttepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(al_int_gmn)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_int_gmn)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_int_gmn)=c("N","Prev","SD","Robust SE","lb","ub")

al_int_gmn=data.frame(al_int_gmn[,c("Prev","Robust SE")])
al_int_gmn$lb=al_int_gmn$Prev - (qnorm(0.975)*al_int_gmn[,"Robust.SE"])
al_int_gmn$ub=al_int_gmn$Prev + (qnorm(0.975)*al_int_gmn[,"Robust.SE"])
al_int_gmn[,"Robust.SE"]=NULL
al_int_gmn=exp(al_int_gmn)-1

hw_int_gmn=data.frame(hw_int_gmn[,c("Prev","Robust SE")])
hw_int_gmn$lb=hw_int_gmn$Prev - (qnorm(0.975)*hw_int_gmn[,"Robust.SE"])
hw_int_gmn$ub=hw_int_gmn$Prev + (qnorm(0.975)*hw_int_gmn[,"Robust.SE"])
hw_int_gmn[,"Robust.SE"]=NULL
hw_int_gmn=exp(hw_int_gmn)-1

tt_int_gmn=data.frame(tt_int_gmn[,c("Prev","Robust SE")])
tt_int_gmn$lb=tt_int_gmn$Prev - (qnorm(0.975)*tt_int_gmn[,"Robust.SE"])
tt_int_gmn$ub=tt_int_gmn$Prev + (qnorm(0.975)*tt_int_gmn[,"Robust.SE"])
tt_int_gmn[,"Robust.SE"]=NULL
tt_int_gmn=exp(tt_int_gmn)-1

psth_geomean_j=cbind(al_int_gmn,hw_int_gmn,tt_int_gmn)

colnames(psth_geomean_j)=c("al-geomean","al-lb","al-ub","hw-geomean",
    "hw-lb","hw-ub","tt-geomean","tt-lb","tt-ub")
rownames(psth_geomean_j)=levels(d$tr)

psth_n_prev_j
psth_n_int_j
psth_prev_j
psth_prev_mh_j
psth_mean_j
psth_geomean_j

#----------------------------------------------
# save prevalence estimates, infection intensity
# estimates, and geometric mean of EPG
#----------------------------------------------
save(al_prev,hw_prev,tt_prev,sth_prev,
     al_mh_prev,hw_mh_prev,tt_mh_prev,sth_mh_prev,
     al_int_mn,hw_int_mn,tt_int_mn,
     al_int_gmn,hw_int_gmn,tt_int_gmn,
     psth_n_prev_j,psth_n_int_j,psth_prev_j,psth_prev_mh_j,psth_mean_j,psth_geomean_j,
     file=paste0(save_data_path, "sth_prev.RData"))


