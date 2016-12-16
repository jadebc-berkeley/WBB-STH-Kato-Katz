#----------------------------------------------------
# function to prepare data for sth analysis in R
#----------------------------------------------------
preprocess.sth=function(d){
  
  # reorder tr labels
  d$tr=factor(d$tr,levels(d$tr)[c(1,6,5,2,7,3,4)])
  
  # drop children missing all 3 sth outcomes
  d$allsth=ifelse(!is.na(d$al) & !is.na(d$tt) & !is.na(d$hw),1,0)
  d=d[d$allsth==1,]
  d$allsth=NULL
  
  d=d[d$hasoutcome==1,]
  
  return(d)
}

#----------------------------------------------------
# function to prepare data for sth analysis in R
# for IPCW - include cases with missing outcome
#----------------------------------------------------
preprocess.sth.ipcw=function(d){
  
  # order data
  d=d[order(d$block,d$clusterid,d$dataid,d$personid),]
  
  # reorder tr labels
  d$tr=factor(d$tr,levels(d$tr)[c(1,6,5,2,7,3,4)])
  
  # set missing values in outcome to 9 for binary outcomes
  d$al[is.na(d$al)]=9
  d$hw[is.na(d$hw)]=9
  d$tt[is.na(d$tt)]=9
  d$sth[is.na(d$sth)]=9
  d$almh[is.na(d$almh)]=9
  d$hwmh[is.na(d$hwmh)]=9
  d$ttmh[is.na(d$ttmh)]=9
  d$sthmh[is.na(d$sthmh)]=9
  
  d$logalepg[d$hasoutcome==0]=99
  d$loghwepg[d$hasoutcome==0]=99
  d$logttepg[d$hasoutcome==0]=99

  d$alepg[d$hasoutcome==0]=99999
  d$hwepg[d$hasoutcome==0]=99999
  d$ttepg[d$hasoutcome==0]=99999
  
  return(d)
}


#----------------------------------------------------
# function to prepare data for sth adjusted analysis in R
#----------------------------------------------------
preprocess.adj.sth=function(d){
  
  d=d[order(d$block,d$clusterid,d$dataid,d$personid),]
  
  # reorder tr labels
  d$month=as.factor(d$month)
  d$block=as.factor(d$block)
  d$counter=as.factor(d$counter)
  return(d)
}


#----------------------------------------------
# function that subsets the dataset to the
# treatment and control arms of interest 
# and converts factors to numeric
# in preparation for estimating the PR and
# mantel-hanszel p-values 
#----------------------------------------------
prep.mh=function(data, y, tr, cont){
  df=data[data$tr==tr|data$tr==cont,]
  df$tr=as.character(df$tr)
  df$upazila=as.factor(df$upazila)
  txtab=table(df$upazila[df$tr==tr],df[[y]][df$tr==tr])
  conttab=table(df$upazila[df$tr==cont],df[[y]][df$tr==cont])
  out=data.frame(cbind(rownames(txtab),txtab,conttab))
  colnames(out)=c("upazila","t0","t1","c0","c1")
  
  # convert factors to numeric
  for(i in 2:5){
    out[,i]=as.numeric(as.character(out[,i]))
  }
  rownames(out)=NULL
  return(out)

}



#----------------------------------------------
# function to extract results from epi.2by2
# input: output from epi.2by2
# output: pooled RR, 95% CI, and 
# stratified chi-square p-value
#----------------------------------------------
extract.mh=function(out, type){
  if(type=="RR"){
    PR=exp(out$b)
    lb=exp(out$ci.lb)
    ub=exp(out$ci.ub)
  }
  if(type=="RD"){
    PR=out$b
    lb=out$ci.lb
    ub=out$ci.ub
  }
  p.value=out$pval
  out=data.frame(PR=PR,PRmin95=lb,PRmax95=ub,MH.Pvalue=p.value)
  colnames(out)[1]=type
  return(out)
}


#----------------------------------------------------
# Format TMLE results
#----------------------------------------------------
format.tmle=function(out,family){
  if(family=="binomial"){
    rr.res=matrix(NA,length(out),4)
    for(i in 1:length(out)){
      rr.res[i,1]=out[[i]]$estimates$RR$psi
      rr.res[i,2]=out[[i]]$estimates$RR$CI[1]
      rr.res[i,3]=out[[i]]$estimates$RR$CI[2]
      rr.res[i,4]=out[[i]]$estimates$RR$pvalue
    }
    rr.res=as.data.frame(rr.res)
    colnames(rr.res)=c("rr","lb","ub","p-value") 
  }
  rd.res=matrix(NA,length(out),4)
  for(i in 1:length(out)){
    rd.res[i,1]=out[[i]]$estimates$ATE$psi
    rd.res[i,2]=out[[i]]$estimates$ATE$CI[1]
    rd.res[i,3]=out[[i]]$estimates$ATE$CI[2]
    rd.res[i,4]=out[[i]]$estimates$ATE$pvalue
  }
  
  rd.res=as.data.frame(rd.res)
  colnames(rd.res)=c("rd","lb","ub","p-value")  
  
  if(family=="binomial"){
    return(list(rr=rr.res,rd=rd.res))
  }
  return(list(rd=rd.res))
  
}


#----------------------------------------------------
# Format TMLEE EPG results
#----------------------------------------------------
format.epg.tmle=function(out){

  rr.res=matrix(NA,length(out),4)
  for(i in 1:length(out)){
    rr.res[i,1]=out[[i]]$estimates$FECR$psi
    rr.res[i,2]=out[[i]]$estimates$FECR$CI[1]
    rr.res[i,3]=out[[i]]$estimates$FECR$CI[2]
    rr.res[i,4]=out[[i]]$estimates$FECR$pvalue
  }
  
  rr.res=as.data.frame(rr.res)
  colnames(rr.res)=c("rr","lb","ub","p-value")  

  return(rr.res)
  
}


#----------------------------------------------------
# function to get prevalence for sth within effect modifier strata
# inputs:
  # data = dataset with outcome, treatment, effect modifier
  # em = string for name of effect modifier variable
# outputs:
  # list of data frames for prevalence by arm within 
  # effect modifier levels
#----------------------------------------------------
emprev=function(data,em){
  d1=data[data[[em]]==1,]
  d0=data[data[[em]]==0,]
  
  # d1
  al_prev_d1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$al[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  hw_prev_d1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$hw[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  tt_prev_d1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$tt[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  sth_prev_d1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$sth[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  
  colnames(al_prev_d1)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(hw_prev_d1)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(tt_prev_d1)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(sth_prev_d1)=c("N","Prev","SD","Robust SE","lb","ub")
  
  
  # d0
  al_prev_d0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$al[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  hw_prev_d0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$hw[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  tt_prev_d0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$tt[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  sth_prev_d0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$sth[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  
  colnames(al_prev_d0)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(hw_prev_d0)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(tt_prev_d0)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(sth_prev_d0)=c("N","Prev","SD","Robust SE","lb","ub")
  
  return(list(out1.al=al_prev_d1,out1.hw=hw_prev_d1,
              out1.tt=tt_prev_d1,out1.sth=sth_prev_d1,
              out0.al=al_prev_d0,out0.hw=hw_prev_d0,
              out0.tt=tt_prev_d0,out0.sth=sth_prev_d0 ))
}


#----------------------------------------------------
# function to get arithmetic mean for
# for sth eggs per gram within effect modifier strata
# inputs:
# data = dataset with outcome, treatment, effect modifier
# em = string for name of effect modifier variable
# outputs:
# list of data frames for arithmetic mean by arm within 
# effect modifier levels
#----------------------------------------------------
em_arimean=function(data,em){
  d1=data[data[[em]]==1,]
  d0=data[data[[em]]==0,]
  
  # d1
  al_int_mn1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$alepg[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  hw_int_mn1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$hwepg[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  tt_int_mn1=t(sapply(levels(d1$tr), function(x) washb_mean(
    Y=d1$ttepg[d1$tr==x],id=d1$clusterid[d1$tr==x],print=FALSE)))
  
  colnames(al_int_mn1)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(hw_int_mn1)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(tt_int_mn1)=c("N","Prev","SD","Robust SE","lb","ub")
  
  # d0
  al_int_mn0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$alepg[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  hw_int_mn0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$hwepg[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  tt_int_mn0=t(sapply(levels(d0$tr), function(x) washb_mean(
    Y=d0$ttepg[d0$tr==x],id=d0$clusterid[d0$tr==x],print=FALSE)))
  
  colnames(al_int_mn0)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(hw_int_mn0)=c("N","Prev","SD","Robust SE","lb","ub")
  colnames(tt_int_mn0)=c("N","Prev","SD","Robust SE","lb","ub")
  
  return(list(out1.al=al_int_mn1,out1.hw=hw_int_mn1,
              out1.tt=tt_int_mn1,
              out0.al=al_int_mn0,out0.hw=hw_int_mn0,
              out0.tt=tt_int_mn0))
}


#----------------------------------------------------
# function to get geometric mean for
# for sth eggs per gram within effect modifier strata
# inputs:
# data = dataset with outcome, treatment, effect modifier
# em = string for name of effect modifier variable
# outputs:
# list of data frames for arithmetic mean by arm within 
# effect modifier levels
#----------------------------------------------------
em_geomean=function(d,em){
  d$ln.alepg=log(d$alepg+1)
  d$ln.hwepg=log(d$hwepg+1)
  d$ln.ttepg=log(d$ttepg+1)
  
  d1=d[d[[em]]==1,]
  d0=d[d[[em]]==0,]

  #d1
  N_al1=data.frame(table(d1$tr[!is.na(d1$ln.alepg)]))[,2]
  N_hw1=data.frame(table(d1$tr[!is.na(d1$ln.hwepg)]))[,2]
  N_tt1=data.frame(table(d1$tr[!is.na(d1$ln.ttepg)]))[,2]
  
  al_int_gmn1=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.alepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  hw_int_gmn1=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.hwepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  tt_int_gmn1=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.ttepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  
  colnames(al_int_gmn1)=c("N","geomean","SD","Robust SE","lb","ub")
  colnames(hw_int_gmn1)=c("N","geomean","SD","Robust SE","lb","ub")
  colnames(tt_int_gmn1)=c("N","geomean","SD","Robust SE","lb","ub")
  
  al_int_gmn1=data.frame(al_int_gmn1[,c("geomean","Robust SE")])
  al_int_gmn1$lb=al_int_gmn1$geomean - (qnorm(0.975)*al_int_gmn1[,"Robust.SE"])
  al_int_gmn1$ub=al_int_gmn1$geomean + (qnorm(0.975)*al_int_gmn1[,"Robust.SE"])
  al_int_gmn1[,"Robust.SE"]=NULL
  al_int_gmn1=exp(al_int_gmn1)-1
  al_int_gmn1=cbind(N_al1,al_int_gmn1)
  colnames(al_int_gmn1)[1]="N"
  
  hw_int_gmn1=data.frame(hw_int_gmn1[,c("geomean","Robust SE")])
  hw_int_gmn1$lb=hw_int_gmn1$geomean - (qnorm(0.975)*hw_int_gmn1[,"Robust.SE"])
  hw_int_gmn1$ub=hw_int_gmn1$geomean + (qnorm(0.975)*hw_int_gmn1[,"Robust.SE"])
  hw_int_gmn1[,"Robust.SE"]=NULL
  hw_int_gmn1=exp(hw_int_gmn1)-1
  hw_int_gmn1=cbind(N_hw1,hw_int_gmn1)
  colnames(hw_int_gmn1)[1]="N"
  
  tt_int_gmn1=data.frame(tt_int_gmn1[,c("geomean","Robust SE")])
  tt_int_gmn1$lb=tt_int_gmn1$geomean - (qnorm(0.975)*tt_int_gmn1[,"Robust.SE"])
  tt_int_gmn1$ub=tt_int_gmn1$geomean + (qnorm(0.975)*tt_int_gmn1[,"Robust.SE"])
  tt_int_gmn1[,"Robust.SE"]=NULL
  tt_int_gmn1=exp(tt_int_gmn1)-1
  tt_int_gmn1=cbind(N_tt1,tt_int_gmn1)
  colnames(tt_int_gmn1)[1]="N"

  #d0
  N_al0=data.frame(table(d0$tr[!is.na(d0$ln.alepg)]))[,2]
  N_hw0=data.frame(table(d0$tr[!is.na(d0$ln.hwepg)]))[,2]
  N_tt0=data.frame(table(d0$tr[!is.na(d0$ln.ttepg)]))[,2]
  
  al_int_gmn0=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.alepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  hw_int_gmn0=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.hwepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  tt_int_gmn0=t(sapply(levels(d$tr), function(x) washb_mean(
    Y=d$ln.ttepg[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))
  
  colnames(al_int_gmn0)=c("N","geomean","SD","Robust SE","lb","ub")
  colnames(hw_int_gmn0)=c("N","geomean","SD","Robust SE","lb","ub")
  colnames(tt_int_gmn0)=c("N","geomean","SD","Robust SE","lb","ub")
  
  al_int_gmn0=data.frame(al_int_gmn0[,c("geomean","Robust SE")])
  al_int_gmn0$lb=al_int_gmn0$geomean - (qnorm(0.975)*al_int_gmn0[,"Robust.SE"])
  al_int_gmn0$ub=al_int_gmn0$geomean + (qnorm(0.975)*al_int_gmn0[,"Robust.SE"])
  al_int_gmn0[,"Robust.SE"]=NULL
  al_int_gmn0=exp(al_int_gmn0)-1
  al_int_gmn0=cbind(N_al1,al_int_gmn0)
  colnames(al_int_gmn0)[1]="N"

  hw_int_gmn0=data.frame(hw_int_gmn0[,c("geomean","Robust SE")])
  hw_int_gmn0$lb=hw_int_gmn0$geomean - (qnorm(0.975)*hw_int_gmn0[,"Robust.SE"])
  hw_int_gmn0$ub=hw_int_gmn0$geomean + (qnorm(0.975)*hw_int_gmn0[,"Robust.SE"])
  hw_int_gmn0[,"Robust.SE"]=NULL
  hw_int_gmn0=exp(hw_int_gmn0)-1
  hw_int_gmn0=cbind(N_hw1,hw_int_gmn0)
  colnames(hw_int_gmn0)[1]="N"

  tt_int_gmn0=data.frame(tt_int_gmn0[,c("geomean","Robust SE")])
  tt_int_gmn0$lb=tt_int_gmn0$geomean - (qnorm(0.975)*tt_int_gmn0[,"Robust.SE"])
  tt_int_gmn0$ub=tt_int_gmn0$geomean + (qnorm(0.975)*tt_int_gmn0[,"Robust.SE"])
  tt_int_gmn0[,"Robust.SE"]=NULL
  tt_int_gmn0=exp(tt_int_gmn0)-1
  tt_int_gmn0=cbind(N_tt1,tt_int_gmn0)
  colnames(tt_int_gmn0)[1]="N"

  return(list(out1.al=al_int_gmn1,out1.hw=hw_int_gmn1,
              out1.tt=tt_int_gmn1,
              out0.al=al_int_gmn0,out0.hw=hw_int_gmn0,
              out0.tt=tt_int_gmn0))
}

