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

# Binary EM table for N and prevalence
make.em.bin.table=function(prev,pr,
                           previ0,previ1,pri0,pri1,
                           previhh0,previhh1,prihh0,prihh1,
                           prevpsac0,prevpsac1,prpsac0,prpsac1,
                           prevpoor0,prevpoor1,prpoor0,prpoor1,
                           prevdw0,prevdw1,prdw0,prdw1,
                           prevdef0,prevdef1,prdef0,prdef1,
                           prevind0,prevind1,prind0,prind1,
                           prevhmud0,prevhmud1,prhmud0,prhmud1,
                           prevlmud0,prevlmud1,prlmud0,prlmud1,
                           prevlat0,prevlat1,prlat0,prlat1,
                           prevfec0,prevfec1,prfec0,prfec1,
                           prevodf0,prevodf1,prodf0,prodf1,
                           decimals){
  
  sub=c("All","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes")
  
  n=prev[,1]
  ni0=previ0[,1]
  ni1=previ1[,1]
  nihh0=previhh0[,1]
  nihh1=previhh1[,1]
  npsac0=prevpsac0[,1]
  npsac1=prevpsac1[,1]
  npoor0=prevpoor0[,1]
  npoor1=prevpoor1[,1]
  ndw0=prevdw0[,1]
  ndw1=prevdw1[,1]
  ndef0=prevdef0[,1]
  ndef1=prevdef1[,1]
  nind0=prevind0[,1]
  nind1=prevind1[,1]
  nhmud0=prevhmud0[,1]
  nhmud1=prevhmud1[,1]
  nlmud0=prevlmud0[,1]
  nlmud1=prevlmud1[,1]
  nlat0=prevlat0[,1]
  nlat1=prevlat1[,1]
  nfec0=prevfec0[,1]
  nfec1=prevfec1[,1]
  nodf0=prevodf0[,1]
  nodf1=prevodf1[,1]

  mainprev=prev.f(prev,1,100)
  emprevi0=prev.f(previ0,1,100)
  emprevi1=prev.f(previ1,1,100)
  emprevihh0=prev.f(previhh0,1,100)
  emprevihh1=prev.f(previhh1,1,100)
  emprevpsac0=prev.f(prevpsac0,1,100)
  emprevpsac1=prev.f(prevpsac1,1,100)
  emprevpoor0=prev.f(prevpoor0,1,100)
  emprevpoor1=prev.f(prevpoor1,1,100)
  emprevdw0=prev.f(prevdw0,1,100)
  emprevdw1=prev.f(prevdw1,1,100)
  emprevdef0=prev.f(prevdef0,1,100)
  emprevdef1=prev.f(prevdef1,1,100)
  emprevind0=prev.f(prevind0,1,100)
  emprevind1=prev.f(prevind1,1,100)
  emprevhmud0=prev.f(prevhmud0,1,100)
  emprevhmud1=prev.f(prevhmud1,1,100)
  emprevlmud0=prev.f(prevlmud0,1,100)
  emprevlmud1=prev.f(prevlmud1,1,100)
  emprevlat0=prev.f(prevlat0,1,100)
  emprevlat1=prev.f(prevlat1,1,100)
  emprevfec0=prev.f(prevfec0,1,100)
  emprevfec1=prev.f(prevfec1,1,100)
  emprevodf0=prev.f(prevodf0,1,100)
  emprevodf1=prev.f(prevodf1,1,100)
  
  mainpr=apply(pr,1,pt.est.ci.f,decimals=2,scale=1)
  empri0=apply(pri0,1,pt.est.ci.f,decimals=2,scale=1)
  empri1=apply(pri1,1,pt.est.ci.f,decimals=2,scale=1)
  emprihh0=apply(prihh0,1,pt.est.ci.f,decimals=2,scale=1)
  emprihh1=apply(prihh1,1,pt.est.ci.f,decimals=2,scale=1)
  emprpsac0=apply(prpsac0,1,pt.est.ci.f,decimals=2,scale=1)
  emprpsac1=apply(prpsac1,1,pt.est.ci.f,decimals=2,scale=1)
  emprpoor0=apply(prpoor0,1,pt.est.ci.f,decimals=2,scale=1)
  emprpoor1=apply(prpoor1,1,pt.est.ci.f,decimals=2,scale=1)
  emprdw0=apply(prdw0,1,pt.est.ci.f,decimals=2,scale=1)
  emprdw1=apply(prdw1,1,pt.est.ci.f,decimals=2,scale=1)
  emprdef0=apply(prdef0,1,pt.est.ci.f,decimals=2,scale=1)
  emprdef1=apply(prdef1,1,pt.est.ci.f,decimals=2,scale=1)
  emprind0=apply(prind0,1,pt.est.ci.f,decimals=2,scale=1)
  emprind1=apply(prind1,1,pt.est.ci.f,decimals=2,scale=1)
  emprhmud0=apply(prhmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emprhmud1=apply(prhmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emprlmud0=apply(prlmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emprlmud1=apply(prlmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emprlat0=apply(prlat0,1,pt.est.ci.f,decimals=2,scale=1)
  emprlat1=apply(prlat1,1,pt.est.ci.f,decimals=2,scale=1)
  emprfec0=apply(prfec0,1,pt.est.ci.f,decimals=2,scale=1)
  emprfec1=apply(prfec1,1,pt.est.ci.f,decimals=2,scale=1)
  emprodf0=apply(prodf0,1,pt.est.ci.f,decimals=2,scale=1)
  emprodf1=apply(prodf1,1,pt.est.ci.f,decimals=2,scale=1)

  nall=rbind(n,ni0,ni1,nihh0,nihh1,npsac0,npsac1,npoor0,npoor1,ndw0,ndw1,ndef0,ndef1,nind0,nind1,nhmud0,nhmud1,nlmud0,nlmud1,nlat0,nlat1,nfec0,nfec1,nodf0,nodf1)
  prevall=rbind(mainprev,emprevi0,emprevi1,emprevihh0,emprevihh1,emprevpsac0,emprevpsac1,emprevpoor0,emprevpoor1,emprevdw0,emprevdw1,emprevdef0,emprevdef1,emprevind0,emprevind1,emprevhmud0,emprevhmud1,emprevlmud0,emprevlmud1,emprevlat0,emprevlat1,emprevfec0,emprevfec1,emprevodf0,emprevodf1)
  prall=rbind(mainpr,empri0,empri1,emprihh0,emprihh1,emprpsac0,emprpsac1,emprpoor0,emprpoor1,emprdw0,emprdw1,emprdef0,emprdef1,emprind0,emprind1,emprhmud0,emprhmud1,emprlmud0,emprlmud1,emprlat0,emprlat1,emprfec0,emprfec1,emprodf0,emprodf1)
  
  
  table=cbind(sub,nall[,1],prevall[,1],nall[,2],prevall[,2],nall[,3],prevall[,3],nall[,4],prevall[,4],nall[,5],prevall[,5],nall[,6],prevall[,6],nall[,7],prevall[,7])
  colnames(table)=c("Sub","Cn","Cprev","Wn","Wprev","Sn","Sprev","Hn","Hprev","WSHn","WSHprev","Nn","Nprev","WSHNn","WSHNprev")
  table=data.frame(table)
  
  return(table)
}

# Binary EM table for prevalence ratio
make.sub.bin.table=function(prev,pr,
                           previ0,previ1,pri0,pri1,
                           previhh0,previhh1,prihh0,prihh1,
                           prevpsac0,prevpsac1,prpsac0,prpsac1,
                           prevpoor0,prevpoor1,prpoor0,prpoor1,
                           prevdw0,prevdw1,prdw0,prdw1,
                           prevdef0,prevdef1,prdef0,prdef1,
                           prevind0,prevind1,prind0,prind1,
                           prevhmud0,prevhmud1,prhmud0,prhmud1,
                           prevlmud0,prevlmud1,prlmud0,prlmud1,
                           prevlat0,prevlat1,prlat0,prlat1,
                           prevfec0,prevfec1,prfec0,prfec1,
                           prevodf0,prevodf1,prodf0,prodf1,
                           decimals){
  
  sub=c("All","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes")
  
  n=prev[,1]
  ni0=previ0[,1]
  ni1=previ1[,1]
  nihh0=previhh0[,1]
  nihh1=previhh1[,1]
  npsac0=prevpsac0[,1]
  npsac1=prevpsac1[,1]
  npoor0=prevpoor0[,1]
  npoor1=prevpoor1[,1]
  ndw0=prevdw0[,1]
  ndw1=prevdw1[,1]
  ndef0=prevdef0[,1]
  ndef1=prevdef1[,1]
  nind0=prevind0[,1]
  nind1=prevind1[,1]
  nhmud0=prevhmud0[,1]
  nhmud1=prevhmud1[,1]
  nlmud0=prevlmud0[,1]
  nlmud1=prevlmud1[,1]
  nlat0=prevlat0[,1]
  nlat1=prevlat1[,1]
  nfec0=prevfec0[,1]
  nfec1=prevfec1[,1]
  nodf0=prevodf0[,1]
  nodf1=prevodf1[,1]
  
  mainprev=prev.f(prev,1,100)
  emprevi0=prev.f(previ0,1,100)
  emprevi1=prev.f(previ1,1,100)
  emprevihh0=prev.f(previhh0,1,100)
  emprevihh1=prev.f(previhh1,1,100)
  emprevpsac0=prev.f(prevpsac0,1,100)
  emprevpsac1=prev.f(prevpsac1,1,100)
  emprevpoor0=prev.f(prevpoor0,1,100)
  emprevpoor1=prev.f(prevpoor1,1,100)
  emprevdw0=prev.f(prevdw0,1,100)
  emprevdw1=prev.f(prevdw1,1,100)
  emprevdef0=prev.f(prevdef0,1,100)
  emprevdef1=prev.f(prevdef1,1,100)
  emprevind0=prev.f(prevind0,1,100)
  emprevind1=prev.f(prevind1,1,100)
  emprevhmud0=prev.f(prevhmud0,1,100)
  emprevhmud1=prev.f(prevhmud1,1,100)
  emprevlmud0=prev.f(prevlmud0,1,100)
  emprevlmud1=prev.f(prevlmud1,1,100)
  emprevlat0=prev.f(prevlat0,1,100)
  emprevlat1=prev.f(prevlat1,1,100)
  emprevfec0=prev.f(prevfec0,1,100)
  emprevfec1=prev.f(prevfec1,1,100)
  emprevodf0=prev.f(prevodf0,1,100)
  emprevodf1=prev.f(prevodf1,1,100)
  
  mainpr=apply(pr,1,pt.est.ci.f,decimals=2,scale=1)
  empri0=apply(pri0,1,pt.est.ci.f,decimals=2,scale=1)
  empri1=apply(pri1,1,pt.est.ci.f,decimals=2,scale=1)
  emprihh0=apply(prihh0,1,pt.est.ci.f,decimals=2,scale=1)
  emprihh1=apply(prihh1,1,pt.est.ci.f,decimals=2,scale=1)
  emprpsac0=apply(prpsac0,1,pt.est.ci.f,decimals=2,scale=1)
  emprpsac1=apply(prpsac1,1,pt.est.ci.f,decimals=2,scale=1)
  emprpoor0=apply(prpoor0,1,pt.est.ci.f,decimals=2,scale=1)
  emprpoor1=apply(prpoor1,1,pt.est.ci.f,decimals=2,scale=1)
  emprdw0=apply(prdw0,1,pt.est.ci.f,decimals=2,scale=1)
  emprdw1=apply(prdw1,1,pt.est.ci.f,decimals=2,scale=1)
  emprdef0=apply(prdef0,1,pt.est.ci.f,decimals=2,scale=1)
  emprdef1=apply(prdef1,1,pt.est.ci.f,decimals=2,scale=1)
  emprind0=apply(prind0,1,pt.est.ci.f,decimals=2,scale=1)
  emprind1=apply(prind1,1,pt.est.ci.f,decimals=2,scale=1)
  emprhmud0=apply(prhmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emprhmud1=apply(prhmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emprlmud0=apply(prlmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emprlmud1=apply(prlmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emprlat0=apply(prlat0,1,pt.est.ci.f,decimals=2,scale=1)
  emprlat1=apply(prlat1,1,pt.est.ci.f,decimals=2,scale=1)
  emprfec0=apply(prfec0,1,pt.est.ci.f,decimals=2,scale=1)
  emprfec1=apply(prfec1,1,pt.est.ci.f,decimals=2,scale=1)
  emprodf0=apply(prodf0,1,pt.est.ci.f,decimals=2,scale=1)
  emprodf1=apply(prodf1,1,pt.est.ci.f,decimals=2,scale=1)
  
  nall=rbind(n,ni0,ni1,nihh0,nihh1,npsac0,npsac1,npoor0,npoor1,ndw0,ndw1,ndef0,ndef1,nind0,nind1,nhmud0,nhmud1,nlmud0,nlmud1,nlat0,nlat1,nfec0,nfec1,nodf0,nodf1)
  prevall=rbind(mainprev,emprevi0,emprevi1,emprevihh0,emprevihh1,emprevpsac0,emprevpsac1,emprevpoor0,emprevpoor1,emprevdw0,emprevdw1,emprevdef0,emprevdef1,emprevind0,emprevind1,emprevhmud0,emprevhmud1,emprevlmud0,emprevlmud1,emprevlat0,emprevlat1,emprevfec0,emprevfec1,emprevodf0,emprevodf1)
  prall=rbind(mainpr,empri0,empri1,emprihh0,emprihh1,emprpsac0,emprpsac1,emprpoor0,emprpoor1,emprdw0,emprdw1,emprdef0,emprdef1,emprind0,emprind1,emprhmud0,emprhmud1,emprlmud0,emprlmud1,emprlat0,emprlat1,emprfec0,emprfec1,emprodf0,emprodf1)
  
  table=cbind(sub,prall[,1],prall[,2],prall[,3],prall[,4],prall[,5],prall[,6])
  colnames(table)=c("Sub","Wpr","Spr","Hpr","WSHpr","Npr","WSHNpr")
  table=data.frame(table)
  
  return(table)
}

# EPG EM table for N and geomean
make.em.epg.table=function(mn,gmn,fecr,
                           gmni0,gmni1,fecri0,fecri1,
                           gmnihh0,gmnihh1,fecrihh0,fecrihh1,
                           gmnpsac0,gmnpsac1,fecrpsac0,fecrpsac1,
                           gmnpoor0,gmnpoor1,fecrpoor0,fecrpoor1,
                           gmndw0,gmndw1,fecrdw0,fecrdw1,
                           gmndef0,gmndef1,fecrdef0,fecrdef1,
                           gmnind0,gmnind1,fecrind0,fecrind1,
                           gmnhmud0,gmnhmud1,fecrhmud0,fecrhmud1,
                           gmnlmud0,gmnlmud1,fecrlmud0,fecrlmud1,
                           gmnlat0,gmnlat1,fecrlat0,fecrlat1,
                           gmnfec0,gmnfec1,fecrfec0,fecrfec1,
                           gmnodf0,gmnodf1,fecrodf0,fecrodf1,
                           decimals){
  
  sub=c("All","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes")
  
  n=mn[,1]
  ni0=gmni0[,1]
  ni1=gmni1[,1]
  nihh0=gmnihh0[,1]
  nihh1=gmnihh1[,1]
  npsac0=gmnpsac0[,1]
  npsac1=gmnpsac1[,1]
  npoor0=gmnpoor0[,1]
  npoor1=gmnpoor1[,1]
  ndw0=gmndw0[,1]
  ndw1=gmndw1[,1]
  ndef0=gmndef0[,1]
  ndef1=gmndef1[,1]
  nind0=gmnind0[,1]
  nind1=gmnind1[,1]
  nhmud0=gmnhmud0[,1]
  nhmud1=gmnhmud1[,1]
  nlmud0=gmnlmud0[,1]
  nlmud1=gmnlmud1[,1]
  nlat0=gmnlat0[,1]
  nlat1=gmnlat1[,1]
  nfec0=gmnfec0[,1]
  nfec1=gmnfec1[,1]
  nodf0=gmnodf0[,1]
  nodf1=gmnodf1[,1]
  
  maingmn=mean.f(gmn,1,1)
  emgmni0=geomean.f(gmni0,1,1)
  emgmni1=geomean.f(gmni1,1,1)
  emgmnihh0=geomean.f(gmnihh0,1,1)
  emgmnihh1=geomean.f(gmnihh1,1,1)
  emgmnpsac0=geomean.f(gmnpsac0,1,1)
  emgmnpsac1=geomean.f(gmnpsac1,1,1)
  emgmnpoor0=geomean.f(gmnpoor0,1,1)
  emgmnpoor1=geomean.f(gmnpoor1,1,1)
  emgmndw0=geomean.f(gmndw0,1,1)
  emgmndw1=geomean.f(gmndw1,1,1)
  emgmndef0=geomean.f(gmndef0,1,1)
  emgmndef1=geomean.f(gmndef1,1,1)
  emgmnind0=geomean.f(gmnind0,1,1)
  emgmnind1=geomean.f(gmnind1,1,1)
  emgmnhmud0=geomean.f(gmnhmud0,1,1)
  emgmnhmud1=geomean.f(gmnhmud1,1,1)
  emgmnlmud0=geomean.f(gmnlmud0,1,1)
  emgmnlmud1=geomean.f(gmnlmud1,1,1)
  emgmnlat0=geomean.f(gmnlat0,1,1)
  emgmnlat1=geomean.f(gmnlat1,1,1)
  emgmnfec0=geomean.f(gmnfec0,1,1)
  emgmnfec1=geomean.f(gmnfec1,1,1)
  emgmnodf0=geomean.f(gmnodf0,1,1)
  emgmnodf1=geomean.f(gmnodf1,1,1)
  
  mainfecr=apply(fecr,1,pt.est.ci.f,decimals=2,scale=1)
  emfecri0=apply(fecri0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecri1=apply(fecri1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrihh0=apply(fecrihh0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrihh1=apply(fecrihh1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpsac0=apply(fecrpsac0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpsac1=apply(fecrpsac1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpoor0=apply(fecrpoor0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpoor1=apply(fecrpoor1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdw0=apply(fecrdw0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdw1=apply(fecrdw1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdef0=apply(fecrdef0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdef1=apply(fecrdef1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrind0=apply(fecrind0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrind1=apply(fecrind1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrhmud0=apply(fecrhmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrhmud1=apply(fecrhmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlmud0=apply(fecrlmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlmud1=apply(fecrlmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlat0=apply(fecrlat0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlat1=apply(fecrlat1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrfec0=apply(fecrfec0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrfec1=apply(fecrfec1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrodf0=apply(fecrodf0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrodf1=apply(fecrodf1,1,pt.est.ci.f,decimals=2,scale=1)
  
  nall=rbind(n,ni0,ni1,nihh0,nihh1,npsac0,npsac1,npoor0,npoor1,ndw0,ndw1,ndef0,ndef1,nind0,nind1,nhmud0,nhmud1,nlmud0,nlmud1,nlat0,nlat1,nfec0,nfec1,nodf0,nodf1)
  gmnall=rbind(maingmn,emgmni0,emgmni1,emgmnihh0,emgmnihh1,emgmnpsac0,emgmnpsac1,emgmnpoor0,emgmnpoor1,emgmndw0,emgmndw1,emgmndef0,emgmndef1,emgmnind0,emgmnind1,emgmnhmud0,emgmnhmud1,emgmnlmud0,emgmnlmud1,emgmnlat0,emgmnlat1,emgmnfec0,emgmnfec1,emgmnodf0,emgmnodf1)
  fecrall=rbind(mainfecr,emfecri0,emfecri1,emfecrihh0,emfecrihh1,emfecrpsac0,emfecrpsac1,emfecrpoor0,emfecrpoor1,emfecrdw0,emfecrdw1,emfecrdef0,emfecrdef1,emfecrind0,emfecrind1,emfecrhmud0,emfecrhmud1,emfecrlmud0,emfecrlmud1,emfecrlat0,emfecrlat1,emfecrfec0,emfecrfec1,emfecrodf0,emfecrodf1)
  
  table=cbind(sub,nall[,1],gmnall[,1],nall[,2],gmnall[,2],nall[,3],gmnall[,3],nall[,4],gmnall[,4],nall[,5],gmnall[,5],nall[,6],gmnall[,6],nall[,7],gmnall[,7])
  colnames(table)=c("Sub","Cn","Cgmn","Wn","Wgmn","Sn","Sgmn","Hn","Hgmn","WSHn","WSHgmn","Nn","Ngmn","WSHNn","WSHNgmn")
  table=data.frame(table)
  
  return(table)
}

# EPG EM table for FECR
make.sub.epg.table=function(mn,gmn,fecr,
                           gmni0,gmni1,fecri0,fecri1,
                           gmnihh0,gmnihh1,fecrihh0,fecrihh1,
                           gmnpsac0,gmnpsac1,fecrpsac0,fecrpsac1,
                           gmnpoor0,gmnpoor1,fecrpoor0,fecrpoor1,
                           gmndw0,gmndw1,fecrdw0,fecrdw1,
                           gmndef0,gmndef1,fecrdef0,fecrdef1,
                           gmnind0,gmnind1,fecrind0,fecrind1,
                           gmnhmud0,gmnhmud1,fecrhmud0,fecrhmud1,
                           gmnlmud0,gmnlmud1,fecrlmud0,fecrlmud1,
                           gmnlat0,gmnlat1,fecrlat0,fecrlat1,
                           gmnfec0,gmnfec1,fecrfec0,fecrfec1,
                           gmnodf0,gmnodf1,fecrodf0,fecrodf1,
                           decimals){
  
  sub=c("All","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes")
  
  n=mn[,1]
  ni0=gmni0[,1]
  ni1=gmni1[,1]
  nihh0=gmnihh0[,1]
  nihh1=gmnihh1[,1]
  npsac0=gmnpsac0[,1]
  npsac1=gmnpsac1[,1]
  npoor0=gmnpoor0[,1]
  npoor1=gmnpoor1[,1]
  ndw0=gmndw0[,1]
  ndw1=gmndw1[,1]
  ndef0=gmndef0[,1]
  ndef1=gmndef1[,1]
  nind0=gmnind0[,1]
  nind1=gmnind1[,1]
  nhmud0=gmnhmud0[,1]
  nhmud1=gmnhmud1[,1]
  nlmud0=gmnlmud0[,1]
  nlmud1=gmnlmud1[,1]
  nlat0=gmnlat0[,1]
  nlat1=gmnlat1[,1]
  nfec0=gmnfec0[,1]
  nfec1=gmnfec1[,1]
  nodf0=gmnodf0[,1]
  nodf1=gmnodf1[,1]
  
  maingmn=mean.f(gmn,1,1)
  emgmni0=geomean.f(gmni0,1,1)
  emgmni1=geomean.f(gmni1,1,1)
  emgmnihh0=geomean.f(gmnihh0,1,1)
  emgmnihh1=geomean.f(gmnihh1,1,1)
  emgmnpsac0=geomean.f(gmnpsac0,1,1)
  emgmnpsac1=geomean.f(gmnpsac1,1,1)
  emgmnpoor0=geomean.f(gmnpoor0,1,1)
  emgmnpoor1=geomean.f(gmnpoor1,1,1)
  emgmndw0=geomean.f(gmndw0,1,1)
  emgmndw1=geomean.f(gmndw1,1,1)
  emgmndef0=geomean.f(gmndef0,1,1)
  emgmndef1=geomean.f(gmndef1,1,1)
  emgmnind0=geomean.f(gmnind0,1,1)
  emgmnind1=geomean.f(gmnind1,1,1)
  emgmnhmud0=geomean.f(gmnhmud0,1,1)
  emgmnhmud1=geomean.f(gmnhmud1,1,1)
  emgmnlmud0=geomean.f(gmnlmud0,1,1)
  emgmnlmud1=geomean.f(gmnlmud1,1,1)
  emgmnlat0=geomean.f(gmnlat0,1,1)
  emgmnlat1=geomean.f(gmnlat1,1,1)
  emgmnfec0=geomean.f(gmnfec0,1,1)
  emgmnfec1=geomean.f(gmnfec1,1,1)
  emgmnodf0=geomean.f(gmnodf0,1,1)
  emgmnodf1=geomean.f(gmnodf1,1,1)
  
  mainfecr=apply(fecr,1,pt.est.ci.f,decimals=2,scale=1)
  emfecri0=apply(fecri0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecri1=apply(fecri1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrihh0=apply(fecrihh0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrihh1=apply(fecrihh1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpsac0=apply(fecrpsac0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpsac1=apply(fecrpsac1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpoor0=apply(fecrpoor0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrpoor1=apply(fecrpoor1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdw0=apply(fecrdw0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdw1=apply(fecrdw1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdef0=apply(fecrdef0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrdef1=apply(fecrdef1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrind0=apply(fecrind0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrind1=apply(fecrind1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrhmud0=apply(fecrhmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrhmud1=apply(fecrhmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlmud0=apply(fecrlmud0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlmud1=apply(fecrlmud1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlat0=apply(fecrlat0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrlat1=apply(fecrlat1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrfec0=apply(fecrfec0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrfec1=apply(fecrfec1,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrodf0=apply(fecrodf0,1,pt.est.ci.f,decimals=2,scale=1)
  emfecrodf1=apply(fecrodf1,1,pt.est.ci.f,decimals=2,scale=1)
  
  nall=rbind(n,ni0,ni1,nihh0,nihh1,npsac0,npsac1,npoor0,npoor1,ndw0,ndw1,ndef0,ndef1,nind0,nind1,nhmud0,nhmud1,nlmud0,nlmud1,nlat0,nlat1,nfec0,nfec1,nodf0,nodf1)
  gmnall=rbind(maingmn,emgmni0,emgmni1,emgmnihh0,emgmnihh1,emgmnpsac0,emgmnpsac1,emgmnpoor0,emgmnpoor1,emgmndw0,emgmndw1,emgmndef0,emgmndef1,emgmnind0,emgmnind1,emgmnhmud0,emgmnhmud1,emgmnlmud0,emgmnlmud1,emgmnlat0,emgmnlat1,emgmnfec0,emgmnfec1,emgmnodf0,emgmnodf1)
  fecrall=rbind(mainfecr,emfecri0,emfecri1,emfecrihh0,emfecrihh1,emfecrpsac0,emfecrpsac1,emfecrpoor0,emfecrpoor1,emfecrdw0,emfecrdw1,emfecrdef0,emfecrdef1,emfecrind0,emfecrind1,emfecrhmud0,emfecrhmud1,emfecrlmud0,emfecrlmud1,emfecrlat0,emfecrlat1,emfecrfec0,emfecrfec1,emfecrodf0,emfecrodf1)
  
  table=cbind(sub,fecrall[,1],fecrall[,2],fecrall[,3],fecrall[,4],fecrall[,5],fecrall[,6])
  colnames(table)=c("Sub","Wfecr","Sfecr","Hfecr","WSHfecr","Nfecr","WSHNfecr")
  table=data.frame(table)
  
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