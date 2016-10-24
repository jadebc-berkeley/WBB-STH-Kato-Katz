#----------------------------------------------------
# function to prepare data for diarrhea analysis in R
#----------------------------------------------------
preprocess.sth=function(d){
  
  # reorder tr labels
  d$tr=factor(d$tr,levels(d$tr)[c(1,6,5,2,7,3,4)])
  
  # drop children missing all 3 sth outcomes
  d$allsth=ifelse(!is.na(d$al) & !is.na(d$tt) & !is.na(d$hw),1,0)
  d=d[d$allsth==1,]
  d$allsth=NULL
  
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
    rr.res=matrix(NA,length(out),3)
    for(i in 1:length(out)){
      rr.res[i,1]=out[[i]]$estimates$RR$psi
      rr.res[i,2]=out[[i]]$estimates$RR$CI[1]
      rr.res[i,3]=out[[i]]$estimates$RR$CI[2]
    }
    rr.res=as.data.frame(rr.res)
    colnames(rr.res)=c("rr","lb","ub") 
  }
  rd.res=matrix(NA,length(out),3)
  for(i in 1:length(out)){
    rd.res[i,1]=out[[i]]$estimates$ATE$psi
    rd.res[i,2]=out[[i]]$estimates$ATE$CI[1]
    rd.res[i,3]=out[[i]]$estimates$ATE$CI[2]
  }
  
  rd.res=as.data.frame(rd.res)
  colnames(rd.res)=c("rd","lb","ub")  
  
  if(family=="binomial"){
    return(list(rr=rr.res,rd=rd.res))
  }
  return(list(rd=rd.res))
  
}
