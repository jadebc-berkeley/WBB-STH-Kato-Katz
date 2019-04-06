##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Functions to make tables

# by Jade
##############################################

# format point estimate and ci
pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}

# format prevalence
prev.f=function(obj,decimals,scale){
  obj=as.data.frame(obj)
  prevr=sprintf(paste("%0.0",decimals,"f",sep=""),apply(as.matrix(obj$Prev),1,function(x) round(x*scale, decimals) ))
  prev.f=paste(prevr,"\\%",sep="")
  return(prev.f)
}

# format prevalence for csv files
prev.f.csv=function(obj,decimals,scale){
  obj=as.data.frame(obj)
  prevr=sprintf(paste("%0.0",decimals,"f",sep=""),apply(as.matrix(obj$Prev),1,function(x) round(x*scale, decimals) ))
  prev.f=paste(prevr,"%",sep="")
  return(prev.f)
}

# format mean
mean.f=function(obj,decimals,scale){
  obj=as.data.frame(obj)
  prevr=sprintf(paste("%0.0",decimals,"f",sep=""),apply(as.matrix(obj$Prev),1,function(x) round(x*scale, decimals) ))
  return(prevr)
}

# format geomean
geomean.f=function(obj,decimals,scale){
  obj=as.data.frame(obj)
  prevr=sprintf(paste("%0.0",decimals,"f",sep=""),apply(as.matrix(obj$geomean),1,function(x) round(x*scale, decimals) ))
  return(prevr)
}

# H1 binary table
make.h1.bin.table=function(alprev,hwprev,ttprev,sthprev,
                           alunadjrr,hwunadjrr,ttunadjrr,sthunadjrr,aladjrr,hwadjrr,ttadjrr,sthadjrr,alipcwrr,hwipcwrr,ttipcwrr,sthipcwrr,
                           alunadjrd,hwunadjrd,ttunadjrd,sthunadjrd,aladjrd,hwadjrd,ttadjrd,sthadjrd,alipcwrd,hwipcwrd,ttipcwrd,sthipcwrd,decimals){
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]
  
  preval=prev.f(alprev,1,100)
  prevhw=prev.f(hwprev,1,100)
  prevtt=prev.f(ttprev,1,100)
  prevsth=prev.f(sthprev,1,100)
  
  tr=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")
  nall=c(nal,nhw,ntt,nsth)
  prevall=c(preval,prevhw,prevtt,prevsth)
  
  unadjrr=c("",apply(alunadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(hwunadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(ttunadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(sthunadjrr,1,pt.est.ci.f,decimals,1))
  
  adjrr=c("",apply(aladjrr,1,pt.est.ci.f,decimals,1),
        "",apply(hwadjrr,1,pt.est.ci.f,decimals,1),
        "",apply(ttadjrr,1,pt.est.ci.f,decimals,1),
        "",apply(sthadjrr,1,pt.est.ci.f,decimals,1))
  
  ipcwrr=c("",apply(alipcwrr,1,pt.est.ci.f,decimals,1),
         "",apply(hwipcwrr,1,pt.est.ci.f,decimals,1),
         "",apply(ttipcwrr,1,pt.est.ci.f,decimals,1),
         "",apply(sthipcwrr,1,pt.est.ci.f,decimals,1))
 
  unadjrd=c("",apply(alunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(hwunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(ttunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(sthunadjrd,1,pt.est.ci.f,decimals,100))
  
  adjrd=c("",apply(aladjrd,1,pt.est.ci.f,decimals,100),
          "",apply(hwadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(ttadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(sthadjrd,1,pt.est.ci.f,decimals,100))
  
  ipcwrd=c("",apply(alipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(hwipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(ttipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(sthipcwrd,1,pt.est.ci.f,decimals,100))
  
  table=data.frame(cbind(tr,nall,prevall,unadjrr,adjrr,ipcwrr,unadjrd,adjrd,ipcwrd))
  
  return(table)
}

# H2 binary table
make.h2.bin.table=function(alprev,hwprev,ttprev,sthprev,
                           alunadjrr,hwunadjrr,ttunadjrr,sthunadjrr,aladjrr,hwadjrr,ttadjrr,sthadjrr,alipcwrr,hwipcwrr,ttipcwrr,sthipcwrr,
                           alunadjrd,hwunadjrd,ttunadjrd,sthunadjrd,aladjrd,hwadjrd,ttadjrd,sthadjrd,alipcwrd,hwipcwrd,ttipcwrd,sthipcwrd,decimals){
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]   
  
  preval=prev.f(alprev,1,100)
  prevhw=prev.f(hwprev,1,100)
  prevtt=prev.f(ttprev,1,100)
  prevsth=prev.f(sthprev,1,100)
  
  tr=c("WSH","Water","Sanitation","Handwashing")
  nall=c(nal[c(5,2:4)],nhw[c(5,2:4)],ntt[c(5,2:4)],nsth[c(5,2:4)])
  prevall=c(preval[c(5,2:4)],prevhw[c(5,2:4)],prevtt[c(5,2:4)],prevsth[c(5,2:4)])
  
  unadjrr=c("",apply(alunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(hwunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(ttunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(sthunadjrr,1,pt.est.ci.f,decimals,1))
  
  adjrr=c("",apply(aladjrr,1,pt.est.ci.f,decimals,1),
          "",apply(hwadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(ttadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(sthadjrr,1,pt.est.ci.f,decimals,1))
  
  ipcwrr=c("",apply(alipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(hwipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(ttipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(sthipcwrr,1,pt.est.ci.f,decimals,1))
  
  unadjrd=c("",apply(alunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(hwunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(ttunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(sthunadjrd,1,pt.est.ci.f,decimals,100))
  
  adjrd=c("",apply(aladjrd,1,pt.est.ci.f,decimals,100),
          "",apply(hwadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(ttadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(sthadjrd,1,pt.est.ci.f,decimals,100))
  
  ipcwrd=c("",apply(alipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(hwipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(ttipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(sthipcwrd,1,pt.est.ci.f,decimals,100))
  
  table=data.frame(cbind(tr,nall,prevall,unadjrr,adjrr,ipcwrr,unadjrd,adjrd,ipcwrd))
  return(table)
}

# H3 binary table
make.h3.bin.table=function(alprev,hwprev,ttprev,sthprev,
                           alunadjrr,hwunadjrr,ttunadjrr,sthunadjrr,aladjrr,hwadjrr,ttadjrr,sthadjrr,alipcwrr,hwipcwrr,ttipcwrr,sthipcwrr,
                           alunadjrd,hwunadjrd,ttunadjrd,sthunadjrd,aladjrd,hwadjrd,ttadjrd,sthadjrd,alipcwrd,hwipcwrd,ttipcwrd,sthipcwrd,decimals){
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]
  
  preval=prev.f(alprev,1,100)
  prevhw=prev.f(hwprev,1,100)
  prevtt=prev.f(ttprev,1,100)
  prevsth=prev.f(sthprev,1,100)
  
  tr=c("Nutrition + WSH","WSH","Nutrition")
  nall=c(nal[c(7,5,6)],nhw[c(7,5,6)],ntt[c(7,5,6)],nsth[c(7,5,6)])
  prevall=c(preval[c(7,5,6)],prevhw[c(7,5,6)],prevtt[c(7,5,6)],prevsth[c(7,5,6)])
  
  unadjrr=c("",apply(alunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(hwunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(ttunadjrr,1,pt.est.ci.f,decimals,1),
            "",apply(sthunadjrr,1,pt.est.ci.f,decimals,1))
  
  adjrr=c("",apply(aladjrr,1,pt.est.ci.f,decimals,1),
          "",apply(hwadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(ttadjrr,1,pt.est.ci.f,decimals,1),
          "",apply(sthadjrr,1,pt.est.ci.f,decimals,1))
  
  ipcwrr=c("",apply(alipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(hwipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(ttipcwrr,1,pt.est.ci.f,decimals,1),
           "",apply(sthipcwrr,1,pt.est.ci.f,decimals,1))
  
  unadjrd=c("",apply(alunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(hwunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(ttunadjrd,1,pt.est.ci.f,decimals,100),
            "",apply(sthunadjrd,1,pt.est.ci.f,decimals,100))
  
  adjrd=c("",apply(aladjrd,1,pt.est.ci.f,decimals,100),
          "",apply(hwadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(ttadjrd,1,pt.est.ci.f,decimals,100),
          "",apply(sthadjrd,1,pt.est.ci.f,decimals,100))
  
  ipcwrd=c("",apply(alipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(hwipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(ttipcwrd,1,pt.est.ci.f,decimals,100),
           "",apply(sthipcwrd,1,pt.est.ci.f,decimals,100))
  
  table=data.frame(cbind(tr,nall,prevall,unadjrr,adjrr,ipcwrr,unadjrd,adjrd,ipcwrd))  
  return(table)
}

# H1 EPG table
make.h1.epg.table=function(almn,hwmn,ttmn,algmn,hwgmn,ttgmn,
                           alunadjari,hwunadjari,ttunadjari,aladjari,hwadjari,ttadjari,alipcwari,hwipcwari,ttipcwari,
                           alunadjgeo,hwunadjgeo,ttunadjgeo,aladjgeo,hwadjgeo,ttadjgeo,alipcwgeo,hwipcwgeo,ttipcwgeo,
                           decimals){
  nal=almn[,1]
  nhw=hwmn[,1]
  ntt=ttmn[,1]
  
  mnal=mean.f(almn,1,1)
  mnhw=mean.f(hwmn,1,1)
  mntt=mean.f(ttmn,1,1)

  gmnal=mean.f(algmn,1,1)
  gmnhw=mean.f(hwgmn,1,1)
  gmntt=mean.f(ttgmn,1,1)
  
  tr=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")
  nall=c(nal,nhw,ntt)
  ariall=c(mnal,mnhw,mntt)
  geoall=c(gmnal,gmnhw,gmntt)
  
  unadjari=c("",apply(alunadjari,1,pt.est.ci.f,decimals,1),
          "",apply(hwunadjari,1,pt.est.ci.f,decimals,1),
          "",apply(ttunadjari,1,pt.est.ci.f,decimals,1))

  adjari=c("",apply(aladjari,1,pt.est.ci.f,decimals,1),
        "",apply(hwadjari,1,pt.est.ci.f,decimals,1),
        "",apply(ttadjari,1,pt.est.ci.f,decimals,1))

  ipcwari=c("",apply(alipcwari,1,pt.est.ci.f,decimals,1),
         "",apply(hwipcwari,1,pt.est.ci.f,decimals,1),
         "",apply(ttipcwari,1,pt.est.ci.f,decimals,1))

  unadjgeo=c("",apply(alunadjgeo,1,pt.est.ci.f,decimals,1),
          "",apply(hwunadjgeo,1,pt.est.ci.f,decimals,1),
          "",apply(ttunadjgeo,1,pt.est.ci.f,decimals,1))
  
  adjgeo=c("",apply(aladjgeo,1,pt.est.ci.f,decimals,1),
        "",apply(hwadjgeo,1,pt.est.ci.f,decimals,1),
        "",apply(ttadjgeo,1,pt.est.ci.f,decimals,1))
  
  ipcwgeo=c("",apply(alipcwgeo,1,pt.est.ci.f,decimals,1),
         "",apply(hwipcwgeo,1,pt.est.ci.f,decimals,1),
         "",apply(ttipcwgeo,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,geoall,unadjgeo,adjgeo,ipcwgeo,unadjari,adjari,ipcwari))
  return(table)
}

# H2 EPG table
make.h2.epg.table=function(almn,hwmn,ttmn,algmn,hwgmn,ttgmn,
                           alunadjari,hwunadjari,ttunadjari,aladjari,hwadjari,ttadjari,alipcwari,hwipcwari,ttipcwari,
                           alunadjgeo,hwunadjgeo,ttunadjgeo,aladjgeo,hwadjgeo,ttadjgeo,alipcwgeo,hwipcwgeo,ttipcwgeo,
                           decimals){
  nal=almn[,1]
  nhw=hwmn[,1]
  ntt=ttmn[,1]
  
  mnal=mean.f(almn,1,1)
  mnhw=mean.f(hwmn,1,1)
  mntt=mean.f(ttmn,1,1)
  
  gmnal=mean.f(algmn,1,1)
  gmnhw=mean.f(hwgmn,1,1)
  gmntt=mean.f(ttgmn,1,1)
  
  tr=c("WSH","Water","Sanitation","Handwashing")
  nall=c(nal[c(5,2:4)],nhw[c(5,2:4)],ntt[c(5,2:4)])
  ariall=c(mnal[c(5,2:4)],mnhw[c(5,2:4)],mntt[c(5,2:4)])
  geoall=c(gmnal[c(5,2:4)],gmnhw[c(5,2:4)],gmntt[c(5,2:4)])
  
  unadjari=c("",apply(alunadjari,1,pt.est.ci.f,decimals,1),
             "",apply(hwunadjari,1,pt.est.ci.f,decimals,1),
             "",apply(ttunadjari,1,pt.est.ci.f,decimals,1))
  
  adjari=c("",apply(aladjari,1,pt.est.ci.f,decimals,1),
           "",apply(hwadjari,1,pt.est.ci.f,decimals,1),
           "",apply(ttadjari,1,pt.est.ci.f,decimals,1))
  
  ipcwari=c("",apply(alipcwari,1,pt.est.ci.f,decimals,1),
            "",apply(hwipcwari,1,pt.est.ci.f,decimals,1),
            "",apply(ttipcwari,1,pt.est.ci.f,decimals,1))
  
  unadjgeo=c("",apply(alunadjgeo,1,pt.est.ci.f,decimals,1),
             "",apply(hwunadjgeo,1,pt.est.ci.f,decimals,1),
             "",apply(ttunadjgeo,1,pt.est.ci.f,decimals,1))
  
  adjgeo=c("",apply(aladjgeo,1,pt.est.ci.f,decimals,1),
           "",apply(hwadjgeo,1,pt.est.ci.f,decimals,1),
           "",apply(ttadjgeo,1,pt.est.ci.f,decimals,1))
  
  ipcwgeo=c("",apply(alipcwgeo,1,pt.est.ci.f,decimals,1),
            "",apply(hwipcwgeo,1,pt.est.ci.f,decimals,1),
            "",apply(ttipcwgeo,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,geoall,unadjgeo,adjgeo,ipcwgeo,unadjari,adjari,ipcwari))
  return(table)
}

# H3 EPG table
make.h3.epg.table=function(almn,hwmn,ttmn,algmn,hwgmn,ttgmn,
                           alunadjari,hwunadjari,ttunadjari,aladjari,hwadjari,ttadjari,alipcwari,hwipcwari,ttipcwari,
                           alunadjgeo,hwunadjgeo,ttunadjgeo,aladjgeo,hwadjgeo,ttadjgeo,alipcwgeo,hwipcwgeo,ttipcwgeo,
                           decimals){
  nal=almn[,1]
  nhw=hwmn[,1]
  ntt=ttmn[,1]
  
  mnal=mean.f(almn,1,1)
  mnhw=mean.f(hwmn,1,1)
  mntt=mean.f(ttmn,1,1)

  gmnal=mean.f(algmn,1,1)
  gmnhw=mean.f(hwgmn,1,1)
  gmntt=mean.f(ttgmn,1,1)
  
  tr=c("Nutrition + WSH","WSH","Nutrition")
  nall=c(nal[c(7,5,6)],nhw[c(7,5,6)],ntt[c(7,5,6)])
  ariall=c(mnal[c(7,5,6)],mnhw[c(7,5,6)],mntt[c(7,5,6)])
  geoall=c(gmnal[c(7,5,6)],gmnhw[c(7,5,6)],gmntt[c(7,5,6)])
  
  unadjari=c("",apply(alunadjari,1,pt.est.ci.f,decimals,1),
             "",apply(hwunadjari,1,pt.est.ci.f,decimals,1),
             "",apply(ttunadjari,1,pt.est.ci.f,decimals,1))
  
  adjari=c("",apply(aladjari,1,pt.est.ci.f,decimals,1),
           "",apply(hwadjari,1,pt.est.ci.f,decimals,1),
           "",apply(ttadjari,1,pt.est.ci.f,decimals,1))
  
  ipcwari=c("",apply(alipcwari,1,pt.est.ci.f,decimals,1),
            "",apply(hwipcwari,1,pt.est.ci.f,decimals,1),
            "",apply(ttipcwari,1,pt.est.ci.f,decimals,1))
  
  unadjgeo=c("",apply(alunadjgeo,1,pt.est.ci.f,decimals,1),
             "",apply(hwunadjgeo,1,pt.est.ci.f,decimals,1),
             "",apply(ttunadjgeo,1,pt.est.ci.f,decimals,1))
  
  adjgeo=c("",apply(aladjgeo,1,pt.est.ci.f,decimals,1),
           "",apply(hwadjgeo,1,pt.est.ci.f,decimals,1),
           "",apply(ttadjgeo,1,pt.est.ci.f,decimals,1))
  
  ipcwgeo=c("",apply(alipcwgeo,1,pt.est.ci.f,decimals,1),
            "",apply(hwipcwgeo,1,pt.est.ci.f,decimals,1),
            "",apply(ttipcwgeo,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,geoall,unadjgeo,adjgeo,ipcwgeo,unadjari,adjari,ipcwari)) 
  return(table)
}


# Binary EM table for prevalence ratio for index child, index hh and psac

make.sub.table=function(alprev,hwprev,ttprev, sthprev,
                        alrr,hwrr,ttrr,sthrr,
                        alprevsub1,hwprevsub1,ttprevsub1,sthprevsub1,
                        alprevsub0,hwprevsub0,ttprevsub0,sthprevsub0,
                        alrrsub1,hwrrsub1,ttrrsub1, sthrrsub1,
                        alrrsub0,hwrrsub0,ttrrsub0, sthrrsub0,
                        decimals){
  
  tr=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")
  
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]
  nall=c(nal,nhw,ntt,nsth)
  
  nalsub1=alprevsub1[,1]
  nhwsub1=hwprevsub1[,1]
  nttsub1=ttprevsub1[,1]
  nsthsub1=sthprevsub1[,1]
  nallsub1=c(nalsub1,nhwsub1,nttsub1,nsthsub1)
  
  nalsub0=alprevsub0[,1]
  nhwsub0=hwprevsub0[,1]
  nttsub0=ttprevsub0[,1]
  nsthsub0=sthprevsub0[,1]
  nallsub0=c(nalsub0,nhwsub0,nttsub0,nsthsub0)
  
  preval=prev.f(alprev,1,100)
  prevhw=prev.f(hwprev,1,100)
  prevtt=prev.f(ttprev,1,100)
  prevsth=prev.f(sthprev,1,100)
  prevall=c(preval,prevhw,prevtt,prevsth)
  
  prevalsub1=prev.f(alprevsub1,1,100)
  prevhwsub1=prev.f(hwprevsub1,1,100)
  prevttsub1=prev.f(ttprevsub1,1,100)
  prevsthsub1=prev.f(sthprevsub1,1,100)
  prevallsub1=c(prevalsub1,prevhwsub1,prevttsub1,prevsthsub1)
  
  prevalsub0=prev.f(alprevsub0,1,100)
  prevhwsub0=prev.f(hwprevsub0,1,100)
  prevttsub0=prev.f(ttprevsub0,1,100)
  prevsthsub0=prev.f(sthprevsub0,1,100)
  prevallsub0=c(prevalsub0,prevhwsub0,prevttsub0,prevsthsub0)

  rr=c("",apply(alrr,1,pt.est.ci.f,decimals,1),
            "",apply(hwrr,1,pt.est.ci.f,decimals,1),
            "",apply(ttrr,1,pt.est.ci.f,decimals,1),
            "",apply(sthrr,1,pt.est.ci.f,decimals,1))
  
  rrsub1=c("",apply(alrrsub1,1,pt.est.ci.f,decimals,1),
       "",apply(hwrrsub1,1,pt.est.ci.f,decimals,1),
       "",apply(ttrrsub1,1,pt.est.ci.f,decimals,1),
       "",apply(sthrrsub1,1,pt.est.ci.f,decimals,1))
  
  rrsub0=c("",apply(alrrsub0,1,pt.est.ci.f,decimals,1),
       "",apply(hwrrsub0,1,pt.est.ci.f,decimals,1),
       "",apply(ttrrsub0,1,pt.est.ci.f,decimals,1),
       "",apply(sthrrsub0,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,prevall,rr,nallsub1,prevallsub1,rrsub1,nallsub0,prevallsub0,rrsub0))
  
  return(table)
}


# Binary EM table for index child and index hh 

make.index.table=function(alprev,hwprev,ttprev, sthprev,
                        alprevi1,hwprevi1,ttprevi1,sthprevi1,
                        alprevihh1,hwprevihh1,ttprevihh1,sthprevihh1,
                        alprevihh0,hwprevihh0,ttprevihh0,sthprevihh0,
                        alrr,hwrr,ttrr,sthrr,
                        alrri1,hwrri1,ttrri1, sthrri1,
                        alrrihh1,hwrrihh1,ttrrihh1, sthrrihh1,
                        alrrihh0,hwrrihh0,ttrrihh0, sthrrihh0,
                        decimals){
  
  tr=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")
  
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]
  nall=c(nal,nhw,ntt,nsth)
  
  nali1=alprevi1[,1]
  nhwi1=hwprevi1[,1]
  ntti1=ttprevi1[,1]
  nsthi1=sthprevi1[,1]
  nalli1=c(nali1,nhwi1,ntti1,nsthi1)
  
  nalihh1=alprevihh1[,1]
  nhwihh1=hwprevihh1[,1]
  nttihh1=ttprevihh1[,1]
  nsthihh1=sthprevihh1[,1]
  nallihh1=c(nalihh1,nhwihh1,nttihh1,nsthihh1)
  
  nalihh0=alprevihh0[,1]
  nhwihh0=hwprevihh0[,1]
  nttihh0=ttprevihh0[,1]
  nsthihh0=sthprevihh0[,1]
  nallihh0=c(nalihh0,nhwihh0,nttihh0,nsthihh0)
  
  preval=prev.f(alprev,1,100)
  prevhw=prev.f(hwprev,1,100)
  prevtt=prev.f(ttprev,1,100)
  prevsth=prev.f(sthprev,1,100)
  prevall=c(preval,prevhw,prevtt,prevsth)
  
  prevali1=prev.f(alprevi1,1,100)
  prevhwi1=prev.f(hwprevi1,1,100)
  prevtti1=prev.f(ttprevi1,1,100)
  prevsthi1=prev.f(sthprevi1,1,100)
  prevalli1=c(prevali1,prevhwi1,prevtti1,prevsthi1)
  
  prevalihh1=prev.f(alprevihh1,1,100)
  prevhwihh1=prev.f(hwprevihh1,1,100)
  prevttihh1=prev.f(ttprevihh1,1,100)
  prevsthihh1=prev.f(sthprevihh1,1,100)
  prevallihh1=c(prevalihh1,prevhwihh1,prevttihh1,prevsthihh1)
  
  prevalihh0=prev.f(alprevihh0,1,100)
  prevhwihh0=prev.f(hwprevihh0,1,100)
  prevttihh0=prev.f(ttprevihh0,1,100)
  prevsthihh0=prev.f(sthprevihh0,1,100)
  prevallihh0=c(prevalihh0,prevhwihh0,prevttihh0,prevsthihh0)
  
  rr=c("",apply(alrr,1,pt.est.ci.f,decimals,1),
       "",apply(hwrr,1,pt.est.ci.f,decimals,1),
       "",apply(ttrr,1,pt.est.ci.f,decimals,1),
       "",apply(sthrr,1,pt.est.ci.f,decimals,1))
  
  rri1=c("",apply(alrri1,1,pt.est.ci.f,decimals,1),
         "",apply(hwrri1,1,pt.est.ci.f,decimals,1),
         "",apply(ttrri1,1,pt.est.ci.f,decimals,1),
         "",apply(sthrri1,1,pt.est.ci.f,decimals,1))
  
  rrihh1=c("",apply(alrrihh1,1,pt.est.ci.f,decimals,1),
         "",apply(hwrrihh1,1,pt.est.ci.f,decimals,1),
         "",apply(ttrrihh1,1,pt.est.ci.f,decimals,1),
         "",apply(sthrrihh1,1,pt.est.ci.f,decimals,1))
  
  rrihh0=c("",apply(alrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(hwrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(ttrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(sthrrihh0,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,prevall,rr,nalli1,prevalli1,rri1,nallihh1,prevallihh1,rrihh1,nallihh0,prevallihh0,rrihh0))
  
  return(table)
}


make.index.table.csv=function(alprev,hwprev,ttprev, sthprev,
                          alprevi1,hwprevi1,ttprevi1,sthprevi1,
                          alprevihh1,hwprevihh1,ttprevihh1,sthprevihh1,
                          alprevihh0,hwprevihh0,ttprevihh0,sthprevihh0,
                          alrr,hwrr,ttrr,sthrr,
                          alrri1,hwrri1,ttrri1, sthrri1,
                          alrrihh1,hwrrihh1,ttrrihh1, sthrrihh1,
                          alrrihh0,hwrrihh0,ttrrihh0, sthrrihh0,
                          decimals){
  
  tr=c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH")
  
  nal=alprev[,1]
  nhw=hwprev[,1]
  ntt=ttprev[,1]
  nsth=sthprev[,1]
  nall=c(nal,nhw,ntt,nsth)
  
  nali1=alprevi1[,1]
  nhwi1=hwprevi1[,1]
  ntti1=ttprevi1[,1]
  nsthi1=sthprevi1[,1]
  nalli1=c(nali1,nhwi1,ntti1,nsthi1)
  
  nalihh1=alprevihh1[,1]
  nhwihh1=hwprevihh1[,1]
  nttihh1=ttprevihh1[,1]
  nsthihh1=sthprevihh1[,1]
  nallihh1=c(nalihh1,nhwihh1,nttihh1,nsthihh1)
  
  nalihh0=alprevihh0[,1]
  nhwihh0=hwprevihh0[,1]
  nttihh0=ttprevihh0[,1]
  nsthihh0=sthprevihh0[,1]
  nallihh0=c(nalihh0,nhwihh0,nttihh0,nsthihh0)
  
  preval=prev.f.csv(alprev,1,100)
  prevhw=prev.f.csv(hwprev,1,100)
  prevtt=prev.f.csv(ttprev,1,100)
  prevsth=prev.f.csv(sthprev,1,100)
  prevall=c(preval,prevhw,prevtt,prevsth)
  
  prevali1=prev.f.csv(alprevi1,1,100)
  prevhwi1=prev.f.csv(hwprevi1,1,100)
  prevtti1=prev.f.csv(ttprevi1,1,100)
  prevsthi1=prev.f.csv(sthprevi1,1,100)
  prevalli1=c(prevali1,prevhwi1,prevtti1,prevsthi1)
  
  prevalihh1=prev.f.csv(alprevihh1,1,100)
  prevhwihh1=prev.f.csv(hwprevihh1,1,100)
  prevttihh1=prev.f.csv(ttprevihh1,1,100)
  prevsthihh1=prev.f.csv(sthprevihh1,1,100)
  prevallihh1=c(prevalihh1,prevhwihh1,prevttihh1,prevsthihh1)
  
  prevalihh0=prev.f.csv(alprevihh0,1,100)
  prevhwihh0=prev.f.csv(hwprevihh0,1,100)
  prevttihh0=prev.f.csv(ttprevihh0,1,100)
  prevsthihh0=prev.f.csv(sthprevihh0,1,100)
  prevallihh0=c(prevalihh0,prevhwihh0,prevttihh0,prevsthihh0)
  
  rr=c("",apply(alrr,1,pt.est.ci.f,decimals,1),
       "",apply(hwrr,1,pt.est.ci.f,decimals,1),
       "",apply(ttrr,1,pt.est.ci.f,decimals,1),
       "",apply(sthrr,1,pt.est.ci.f,decimals,1))
  
  rri1=c("",apply(alrri1,1,pt.est.ci.f,decimals,1),
         "",apply(hwrri1,1,pt.est.ci.f,decimals,1),
         "",apply(ttrri1,1,pt.est.ci.f,decimals,1),
         "",apply(sthrri1,1,pt.est.ci.f,decimals,1))
  
  rrihh1=c("",apply(alrrihh1,1,pt.est.ci.f,decimals,1),
           "",apply(hwrrihh1,1,pt.est.ci.f,decimals,1),
           "",apply(ttrrihh1,1,pt.est.ci.f,decimals,1),
           "",apply(sthrrihh1,1,pt.est.ci.f,decimals,1))
  
  rrihh0=c("",apply(alrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(hwrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(ttrrihh0,1,pt.est.ci.f,decimals,1),
           "",apply(sthrrihh0,1,pt.est.ci.f,decimals,1))
  
  table=data.frame(cbind(tr,nall,prevall,rr,nalli1,prevalli1,rri1,nallihh1,prevallihh1,rrihh1,nallihh0,prevallihh0,rrihh0))
  
  return(table)
}