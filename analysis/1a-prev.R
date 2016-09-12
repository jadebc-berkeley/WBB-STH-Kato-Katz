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

psth_n_prev_j=data.frame(cbind(n.al,n.hw,n.tt,n.sth,
                               N.al,N.hw,N.tt,N.sth))

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

psth_prev_j=cbind(al_prev[,c("Prev","Robust SE","lb","ub")],
                  hw_prev[,c("Prev","Robust SE","lb","ub")],
                  tt_prev[,c("Prev","Robust SE","lb","ub")],
                  sth_prev[,c("Prev","Robust SE","lb","ub")])

colnames(psth_prev_j)=c("al-prev","al-se","al-lb","al-ub",
                        "hw-prev","hw-se","hw-lb","hw-ub",
                        "tt-prev","tt-se","tt-lb","tt-ub",
                        "sth-prev","sth-se","sth-lb","sth-ub")

#----------------------------------------------
# prevalence of moderate/heavy infection by arm and time point
#----------------------------------------------
al_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$almh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
hw_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$hwmh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
tt_mh_prev=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$ttmh[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(al_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(hw_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(tt_mh_prev)=c("N","Prev","SD","Robust SE","lb","ub")

psth_prev_mh_j=cbind(al_mh_prev[,c("Prev","Robust SE","lb","ub")],
                     hw_mh_prev[,c("Prev","Robust SE","lb","ub")],
                      tt_mh_prev[,c("Prev","Robust SE","lb","ub")])

colnames(psth_prev_mh_j)=c("al-prev","al-se","al-lb","al-ub",
                           "hw-prev","hw-se","hw-lb","hw-ub",
                           "tt-prev","tt-se","tt-lb","tt-ub")

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

psth_mean_j=cbind(al_int_mn[,c("Prev","SD")],
                  hw_int_mn[,c("Prev","SD")],
                  tt_int_mn[,c("Prev","SD")])

colnames(psth_mean_j)=c("al-mean","al-sd","hw-mean",
                           "hw-sd","tt-mean","tt-sd")
rownames(psth_mean_j)=levels(d$tr)


#----------------------------------------------
# geometric mean of intensity by arm and time point
# geo mean = exp[Sum(log_e(c+1))/n] – 1
#----------------------------------------------
d$ln.alepg=log(d$alepg+1)
d$ln.hwepg=log(d$hwepg+1)
d$ln.ttepg=log(d$ttepg+1)

al_int_gmn=sapply(levels(d$tr), function(x) exp(mean(d$ln.alepg[d$tr==x]))-1)
hw_int_gmn=sapply(levels(d$tr), function(x) exp(mean(d$ln.hwepg[d$tr==x]))-1)
tt_int_gmn=sapply(levels(d$tr), function(x) exp(mean(d$ln.ttepg[d$tr==x]))-1)

al_int_sd=sapply(levels(d$tr), function(x) exp(sd(d$ln.alepg[d$tr==x]))-1)
hw_int_sd=sapply(levels(d$tr), function(x) exp(sd(d$ln.hwepg[d$tr==x]))-1)
tt_int_sd=sapply(levels(d$tr), function(x) exp(sd(d$ln.ttepg[d$tr==x]))-1)

psth_geomean_j=cbind(al_int_gmn,al_int_sd,
                      hw_int_gmn,hw_int_sd,
                      tt_int_gmn,tt_int_sd)

colnames(psth_geomean_j)=c("al-geomean","al-sd","hw-geomean",
                           "hw-sd","tt-geomean","tt-sd")
rownames(psth_geomean_j)=levels(d$tr)

psth_n_prev_j
psth_n_int_j
psth_prev_j
psth_prev_mh_j
psth_mean_j
psth_geomean_j

save(psth_n_prev_j,psth_n_int_j,psth_prev_j,psth_prev_mh_j,psth_mean_j,psth_geomean_j,
     file="~/Dropbox/WASHB Parasites/Results/Jade/sth_prev.RData")


