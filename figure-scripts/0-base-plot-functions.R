##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Functions to make plots

# by Jade
##############################################

library(ggplot2)
library(grid)
library(gridExtra)

source("~/Box Sync/WASHB Parasites/Scripts/Figures/theme_complete_bw.R")

# format point estimate and ci
pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}

# format ci
ci.f=function(obj,decimals,scale){
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste("(",b,", ",c,")",sep=""))
}

# prepare prevalence data for plotting
sth.plot.prep=function(prev){
  
  prev=as.data.frame(prev)
  prev$tr=as.factor(rownames(prev))
  prev$prev.f=as.numeric(sprintf("%0.1f",prev$Prev*100))
  prev$lb.f=as.numeric(sprintf("%0.0f",prev$lb*100))
  prev$ub.f=as.numeric(sprintf("%0.0f",prev$ub*100))
  prev$x=as.factor(seq(1,7,1))
  
  return(prev)
}

# prepare prevalence data for plotting
sth.epg.plot.prep=function(mn){
  
  mn=as.data.frame(mn)
  mn$tr=as.factor(rownames(mn))
  colnames(mn)[1]="geomean"
  mn$mn.f=as.numeric(sprintf("%0.2f",mn$geomean))
  mn$x=as.factor(seq(1,7,1))
  
  return(mn)
}


# make single plot
makeplot=function(n,prev,prh1,prh2,prh3,ytitle,ylim){
  
  # format estimates
  n=paste("(N=",format(n,big.mark=","),")",sep="")
  rr=apply(as.matrix(prh1),1,pt.est.ci.f,decimals=2,scale=1)
  rrh1=c("ref",rr)
  rrh2w=c("","ref","","",pt.est.ci.f(prh2[1,],decimals=2,scale=1),"","")
  rrh2s=c("","","ref","",pt.est.ci.f(prh2[2,],decimals=2,scale=1),"","")
  rrh2h=c("","","","ref",pt.est.ci.f(prh2[3,],decimals=2,scale=1),"","")
  rrh3wsh=c("","","","","ref","",pt.est.ci.f(prh3[1,],decimals=2,scale=1))
  rrh3n=c("","","","","","ref",pt.est.ci.f(prh3[2,],decimals=2,scale=1))
  
  cliney=as.numeric(prev$prev.f[1])
  
  # define color palette
  black = "#000004FF"
  blue = "#3366AA"
  teal = "#11AA99"
  green = "#66AA55"
  chartr = "#CCCC55"
  magent = "#992288"
  red = "#EE3333"
  orange = "#EEA722"
  yellow = "#FFEE33"
  grey = "#777777"
  cols=c(black,blue,teal,green,orange,red,magent,blue)
  
  colh2w=c("#5e5f60",cols[2],"#5e5f60","#5e5f60","#5e5f60","#5e5f60","#5e5f60")
  colh2s=c("#5e5f60","#5e5f60",cols[3],"#5e5f60","#5e5f60","#5e5f60","#5e5f60")
  colh2h=c("#5e5f60","#5e5f60","#5e5f60",cols[4],"#5e5f60","#5e5f60","#5e5f60")
  colh3wsh=c("#5e5f60","#5e5f60","#5e5f60","#5e5f60",cols[5],"#5e5f60","#5e5f60")
  colh3n=c("#5e5f60","#5e5f60","#5e5f60","#5e5f60","#5e5f60",cols[6],"#5e5f60")
  
  if(ytitle=="Ascaris"){
    ytitlex=-0.75
    ylim=c(0,60)
    pylim=58
    linediff=0.038
    secdiff=0.07  
    gap=5
  }
  if(ytitle=="Hookworm"){
    ytitlex=-0.6
    ylim=c(0,20)
    pylim=20
    linediff=0.06
    secdiff=0.1
    gap=2
  }
  if(ytitle=="Trichuris"){
    ytitlex=-0.7
    ylim=c(0,20)
    pylim=20
    linediff=0.06
    secdiff=0.1
    gap=2
  }
  if(ytitle=="Any STH"){
    ytitlex=-0.69
    ylim=c(20,60)
    pylim=58
    linediff=0.038
    secdiff=0.07  
    gap=5
  }

  g1=ggplot(prev,aes(x=x,y=prev.f))+
    geom_point(aes(col=x),alpha=0.7,size=1.5,show.legend=FALSE)+
    geom_errorbar(aes(ymin=lb.f,ymax=ub.f,col=x),width=0.11,show.legend=FALSE)+
    geom_hline(yintercept=cliney,linetype="dashed")+
    coord_cartesian(ylim = ylim,xlim=c(1,7)) +
    scale_color_manual("",values=cols)+
    ylab("Prevalence\nat 2-year\nfollow-up (%)")+xlab("")+
    scale_y_continuous(breaks=seq(ylim[1],ylim[2],gap),labels=seq(ylim[1],ylim[2],gap))+
    theme_complete_bw() +
    theme(plot.margin = unit(c(6.8, 0.5, 0.5, 3.5), "lines"))+
  
    annotate(geom="text",x=0.65,y=cliney+0.05,label="Control",size=2.5)+
    annotate(geom="text",x=0.62,y=cliney+0.02,label="mean",size=2.5)+
    annotate(geom="text",x=ytitlex,y=pylim*(1.1+linediff*4+secdiff*2.95),label=ytitle,size=5.5)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*3.4),
             label=c("Control","Water","Sanitation","Handwashing",
                     "Combined","Nutrition","Combined"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*2.8),
             label=c("","","","","WSH","","Nutrition+WSH"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*2.1),
             label=n,size=3,color="#5e5f60")+
    annotate(geom="text",x=-0.3,y=pylim*(1.1+linediff*4+secdiff*2.02), label="Prevalence Ratio (95% CI)",size=3.5)+
    annotate(geom="text",x=-0.1,y=pylim*(1.1+linediff*3+secdiff*2), label="Intervention vs. Control",size=2.7,col="#5e5f60")+
    
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*3+secdiff*2),label=rrh1,size=2.7,col="#5e5f60")+
    
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff*3+secdiff), label="WSH vs. W",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff*2+secdiff), label="WSH vs. S",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff+secdiff), label="WSH vs. H",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*3+secdiff),label=rrh2w,size=2.7,col=colh2w)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*2+secdiff),label=rrh2s,size=2.7,col=colh2s)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff+secdiff),label=rrh2h,size=2.7,col=colh2h)+
    
    annotate(geom="text",x=-.16,y=pylim*(1.1+linediff), label="Nutrition + WSH vs. WSH",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=-.23,y=pylim*1.1, label="Nutrition + WSH vs. Nutrition",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff),label=rrh3wsh,size=2.7,col=colh3wsh)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*1.1,label=rrh3n,size=2.7,col=colh3n)+
  annotate(geom="text",x=seq(1,7,1)+.15,y=prev$prev.f,label=prev$prev.f,size=2.7,col=cols[1:7])
  
  # remove clipping of x axis labels
  g2 <- ggplot_gtable(ggplot_build(g1))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"
  return(arrangeGrob(g2))
}



# make single plot
makeepgplot=function(n,mn,prh1,prh2,prh3,ytitle){
  
  # format estimates
  # FECR was calculated as RR-1, but since effect is assumed
  # to be protective, switching to 1-RR
  # prh1=-prh1
  # prh2=-prh2
  # prh3=-prh3
  n=paste("(N=",format(n,big.mark=","),")",sep="")
  colnames(prh1)=c("rr","ub","lb","p-value")
  colnames(prh2)=c("rr","ub","lb","p-value")
  colnames(prh3)=c("rr","ub","lb","p-value")
  rr=apply(as.matrix(prh1),1,pt.est.ci.f,decimals=2,scale=1)
  rrh1=c("ref",rr)
  rrh2w=c("","ref","","",pt.est.ci.f(prh2[1,],decimals=2,scale=1),"","")
  rrh2s=c("","","ref","",pt.est.ci.f(prh2[2,],decimals=2,scale=1),"","")
  rrh2h=c("","","","ref",pt.est.ci.f(prh2[3,],decimals=2,scale=1),"","")
  rrh3wsh=c("","","","","ref","",pt.est.ci.f(prh3[1,],decimals=2,scale=1))
  rrh3n=c("","","","","","ref",pt.est.ci.f(prh3[2,],decimals=2,scale=1))
  cliney=as.numeric(mn$mn.f[1])
  
  # define color palette
  black = "#000004FF"
  blue = "#3366AA"
  teal = "#11AA99"
  green = "#66AA55"
  chartr = "#CCCC55"
  magent = "#992288"
  red = "#EE3333"
  orange = "#EEA722"
  yellow = "#FFEE33"
  grey = "#777777"
  cols=c(black,blue,teal,green,orange,red,magent,blue)
  
  colh2w=c("#5e5f60",cols[2],"#5e5f60","#5e5f60","#5e5f60","#5e5f60","#5e5f60")
  colh2s=c("#5e5f60","#5e5f60",cols[3],"#5e5f60","#5e5f60","#5e5f60","#5e5f60")
  colh2h=c("#5e5f60","#5e5f60","#5e5f60",cols[4],"#5e5f60","#5e5f60","#5e5f60")
  colh3wsh=c("#5e5f60","#5e5f60","#5e5f60","#5e5f60",cols[5],"#5e5f60","#5e5f60")
  colh3n=c("#5e5f60","#5e5f60","#5e5f60","#5e5f60","#5e5f60",cols[6],"#5e5f60")
  
  linediff=0.06
  secdiff=0.1
  
  if(ytitle=="Ascaris"){
    ytitlex=-0.73
    pbreaks=seq(0,10,1)
    plabels=seq(0,10,1)
    pylim=10
    pymin=0
    cmny2=cliney+((pylim-cliney)/20)*3
    cmny1=cliney+((pylim-cliney)/20)*1
  }
  if(ytitle=="Hookworm"){
    ytitlex=-0.58
    pbreaks=seq(0,1,.1)
    plabels=seq(0,1,.1)
    pylim=1
    pymin=0
    cmny2=cliney+((pylim-cliney)/20)*3.5
    cmny1=cliney+((pylim-cliney)/20)*1.5
  }
  if(ytitle=="Trichuris"){
    ytitlex=-0.7
    pbreaks=seq(0,1,.1)
    plabels=seq(0,1,.1)
  pylim=1
  pymin=0
  cmny2=cliney+((pylim-cliney)/20)*3.5
  cmny1=cliney+((pylim-cliney)/20)*1.5
  }
  
  g1=ggplot(mn,aes(x=x,y=mn.f))+
    geom_point(aes(col=x),alpha=0.7,size=1.5,show.legend=FALSE)+
    geom_errorbar(aes(ymin=lb,ymax=ub,col=x),width=0.11,show.legend=FALSE)+
    geom_hline(yintercept=cliney,linetype="dashed")+
    coord_cartesian(ylim = c(pymin, pylim),xlim=c(1,7)) +
    scale_color_manual("",values=cols)+
    ylab("Geometric mean\neggs per gram\nat 2-year\nfollow-up")+xlab("")+
    scale_y_continuous(breaks=pbreaks,labels=plabels)+
    theme_complete_bw() +
    theme(plot.margin = unit(c(7, 0.5, 0.5, 3.5), "lines"))+
    
    annotate(geom="text",x=0.65,y=cmny2,label="Control",size=2.5)+
    annotate(geom="text",x=0.62,y=cmny1,label="mean",size=2.5)+
    annotate(geom="text",x=ytitlex,y=pylim*(1.1+linediff*5+secdiff*2+0.15),label=ytitle,size=5.5)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*2+0.16),
             label=c("Control","Water","Sanitation","Handwashing",
                     "Combined","Nutrition","Combined"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*2+0.1),
             label=c("","","","","WSH","","Nutrition+WSH"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*4+secdiff*2+0.02),
             label=n,color="#5e5f60",size=3)+
    annotate(geom="text",x=-1.06,y=pylim*(1.1+linediff*5+secdiff*2+0.04), hjust=0, label="Fecal Egg Count Reduction",size=3.5)+
    annotate(geom="text",x=-1.06,y=pylim*(1.1+linediff*4+secdiff*2+0.02), hjust=0, label="(95% CI)",size=3.5)+
    annotate(geom="text",x=-0.1,y=pylim*(1.1+linediff*3+secdiff*2), label="Intervention vs. Control",size=2.7,col="#5e5f60")+

    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*3+secdiff*2),label=rrh1,size=2.7,col="#5e5f60")+
    
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff*3+secdiff), label="WSH vs. W",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff*2+secdiff), label="WSH vs. S",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=0.22,y=pylim*(1.1+linediff+secdiff), label="WSH vs. H",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*3+secdiff),label=rrh2w,size=2.7,col=colh2w)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff*2+secdiff),label=rrh2s,size=2.7,col=colh2s)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff+secdiff),label=rrh2h,size=2.7,col=colh2h)+

    annotate(geom="text",x=-.16,y=pylim*(1.1+linediff), label="Nutrition + WSH vs. WSH",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=-.23,y=pylim*1.1, label="Nutrition + WSH vs. Nutrition",size=2.7,col="#5e5f60")+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(1.1+linediff),label=rrh3wsh,size=2.7,col=colh3wsh)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*1.1,label=rrh3n,size=2.7,col=colh3n)+
  annotate(geom="text",x=seq(1,7,1)+.15,y=mn$mn.f,label=mn$mn.f,size=2.7,col=cols[1:7])
  
  # remove clipping of x axis labels
  g2 <- ggplot_gtable(ggplot_build(g1))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"

  return(arrangeGrob(g2))
}


# save 4 binary sth plots in one pdf
saveplot=function(alplot,hwplot,ttplot,sthplot,lab, fig_dir){
  pdf(paste(fig_dir,"fig-prev-",lab,".pdf",sep=""),width=9,height=15.5)
  grid.arrange(alplot,hwplot,ttplot,sthplot,ncol=1,nrow=4)
  dev.off()
}


# save 3 binary sth plots in one pdf
saveepgplot=function(alplot,hwplot,ttplot,lab, fig_dir){
  pdf(paste(fig_dir,"fig-epg-",lab,".pdf",sep=""),width=10,height=11)
  grid.arrange(alplot,hwplot,ttplot,ncol=1,nrow=3)
  dev.off()
}


# wrapper function
sth.bin.plot=function(aln,hwn,ttn,sthn,alprev,hwprev,ttprev,sthprev,alprh1,alprh2,alprh3,hwprh1,hwprh2,hwprh3,ttprh1,ttprh2,ttprh3,sthprh1,sthprh2,sthprh3,lab, fig_dir){
  # prep data for plotting
  alprev=sth.plot.prep(alprev)
  hwprev=sth.plot.prep(hwprev)
  ttprev=sth.plot.prep(ttprev)
  sthprev=sth.plot.prep(sthprev)
  
  alplot=makeplot(aln,alprev,alprh1,alprh2,alprh3,ytitle="Ascaris",ylim)
  hwplot=makeplot(hwn,hwprev,hwprh1,hwprh2,hwprh3,ytitle="Hookworm",ylim)
  ttplot=makeplot(ttn,ttprev,ttprh1,ttprh2,ttprh3,ytitle="Trichuris",ylim)
  sthplot=makeplot(sthn,sthprev,sthprh1,sthprh2,sthprh3,ytitle="Any STH",ylim)
  
  saveplot(alplot,hwplot,ttplot,sthplot,lab,fig_dir)
}



# wrapper function
sth.epg.plot=function(aln,hwn,ttn,almn,hwmn,ttmn,algeoh1,algeoh2,algeoh3,hwgeoh1,hwgeoh2,hwgeoh3,ttgeoh1,ttgeoh2,ttgeoh3,lab, fig_dir){
  # prep data for plotting
  almn=sth.epg.plot.prep(almn)
  hwmn=sth.epg.plot.prep(hwmn)
  ttmn=sth.epg.plot.prep(ttmn)

  alplot=makeepgplot(aln,almn,algeoh1,algeoh2,algeoh3,ytitle="Ascaris")
  hwplot=makeepgplot(hwn,hwmn,hwgeoh1,hwgeoh2,hwgeoh3,ytitle="Hookworm")
  ttplot=makeepgplot(ttn,ttmn,ttgeoh1,ttgeoh2,ttgeoh3,ytitle="Trichuris")

  saveepgplot(alplot,hwplot,ttplot,lab, fig_dir)
}




theme_complete_bw <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line =         element_blank(),
      # axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.text.x =       element_blank(),
      axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.ticks.y =      element_line(colour = "black"),
      axis.ticks.x =      element_blank(),
      axis.title.x =      element_text(size = base_size, vjust = 0.5),
      axis.title.y =      element_text(size = base_size * 0.8, angle = 0, vjust = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      legend.background = element_rect(colour=NA), 
      legend.key =        element_rect(fill = NA, colour = "black", size = 0.25),
      legend.key.size =   unit(1.2, "lines"),
      legend.text =       element_text(size = base_size * 0.8),
      legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
      legend.position =   "right",
      
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border =     element_rect(fill = NA, colour = "grey50"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_line(colour = "grey50", size = 0.1, linetype="dashed"), 
      panel.spacing =     unit(0.5, "lines"),
      
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x =     element_text(colour = "black", size = base_size * 0.8, face="bold", 
                                      margin = margin(0.3,0.1,0.1,0.1,"cm")),
      strip.text.y =     element_text(colour = "black", size = base_size * 0.8, angle = -90),
      
      plot.background =  element_rect(colour = NA, fill = "white"),
      #plot.title =       element_text(size = base_size * 1.2),
      plot.margin =      unit(c(1, 1, 0.5, 0.5), "lines"))
}
