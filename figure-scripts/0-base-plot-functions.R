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
  mn$mn.f=as.numeric(sprintf("%0.1f",mn$geomean))
  mn$x=as.factor(seq(1,7,1))
  
  return(mn)
}


# make single plot
makeplot=function(n,prev,prh1,ytitle,ylim){
  
  # format estimates
  n=paste("(N=",format(n,big.mark=","),")",sep="")
  rr=apply(as.matrix(prh1),1,pt.est.ci.f,decimals=2,scale=1)
  rrh1=c("ref",rr)
  
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
  
  if(ytitle=="A. lumbricoides"){
    ytitlex=-0.2
    ylim=c(25,50)
    pylim=50
    linediff=0.039
    secdiff=0.029
    gap=5
    ytitle_f = "italic('A. lumbricoides')"
  }
  if(ytitle=="Hookworm"){
    ytitlex=0
    ylim=c(2,14)
    pylim=14
    linediff=0.065
    secdiff=0.045
    gap=2
    ytitle_f = "Hookworm"
  }
  if(ytitle=="T. trichiura"){
    ytitlex=-0.15
    ylim=c(2,14)
    pylim=14
    linediff=0.065
    secdiff=0.045
    gap=2
    ytitle_f = "italic('T. trichiura')"
    
  }
  if(ytitle=="Any STH"){
    ytitlex=-0.69
    ylim=c(20,60)
    pylim=58
    linediff=0.038
    secdiff=0.07  
    gap=5
    ytitle_f = "Any STH"
    
  }
  
  yaxis_shift = 1

  g1=ggplot(prev,aes(x=x,y=prev.f))+
    geom_point(aes(col=x),alpha=0.7,size=1.5,show.legend=FALSE)+
    geom_errorbar(aes(ymin=lb.f,ymax=ub.f,col=x),width=0.11,show.legend=FALSE)+
    geom_hline(yintercept=cliney,linetype="dashed")+
    coord_cartesian(ylim = ylim,xlim=c(1,7)) +
    scale_color_manual("",values=cols)+
    ylab("Prevalence (%)")+xlab("")+
    scale_y_continuous(breaks=seq(ylim[1],ylim[2],gap),
                       labels=seq(ylim[1],ylim[2],gap))+
    theme_complete_bw() +
    theme(plot.margin = unit(c(t = 3, r = 0.2, b = -0.5, l = 2), "lines"))+
  
    # label control mean 
    annotate(geom="text",x=0.65,y=cliney+0.05,label="Control",size=2.5)+
    annotate(geom="text",x=0.62,y=cliney+0.02,label="mean",size=2.5)+
    
    # helminth name
    annotate(geom="text",x=ytitlex,y=pylim*(yaxis_shift+linediff*4+secdiff*0.5),
             label=ytitle_f,size=5.5, parse=TRUE)+
    
    # label treatment arms
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*4+secdiff*0.5),
             label=c("Control","Water","Sanitation","Handwashing",
                     "Combined","Nutrition","Combined"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*3+secdiff*0.5),
             label=c("","","","","WSH","","Nutrition+WSH"),
             color=cols[1:7],size=3)+
    
    # add ns
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*1+secdiff*0.5),
             label=n,size=2.7,color="#5e5f60")+
    
    # print PR on plot
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*2+secdiff*0.5),
             label=rrh1,size=2.7,col="#5e5f60")+
    
    # plot labels
    annotate(geom="text",x=0,y=pylim*(yaxis_shift+linediff*2+secdiff*0.5), 
             label="PR (95% CI)",size=3.5)+

    # print prevalence on plot
  annotate(geom="text",x=seq(1,7,1)+.22,y=prev$prev.f,label=as.character(prev$prev.f),
           size=4,col=cols[1:7])
  
  # remove clipping of x axis labels
  g2 <- ggplot_gtable(ggplot_build(g1))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"
  return(arrangeGrob(g2))
}



# make single plot
makeepgplot=function(n,mn,ytitle){
  
  # format estimates

  n=paste("(N=",format(n,big.mark=","),")",sep="")
  # colnames(prh1)=c("rr","ub","lb","p-value")
  # rr=apply(as.matrix(prh1),1,pt.est.ci.f,decimals=1,scale=1)
  # rrh1=c("ref",rr)
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

  linediff=0.06
  secdiff=0.1
  
  if(ytitle=="Ascaris"){
    ytitlex=0.5
    pbreaks=seq(0,10,1)
    plabels=seq(0,10,1)
    pylim=10
    pymin=0
    cmny2=cliney+((pylim-cliney)/20)*3
    cmny1=cliney+((pylim-cliney)/20)*1
    ytitle_f = "italic('A. lumbricoides')"
  }
  if(ytitle=="Hookworm"){
    ytitlex=0.2
    pbreaks=seq(0,1,.1)
    plabels=seq(0,1,.1)
    pylim=1
    pymin=0
    cmny2=cliney+((pylim-cliney)/20)*3.5
    cmny1=cliney+((pylim-cliney)/20)*1.5
    ytitle_f = "Hookworm"
  }
  if(ytitle=="Trichuris"){
    ytitlex=0.2
    pbreaks=seq(0,1,.1)
    plabels=seq(0,1,.1)
  pylim=1
  pymin=0
  cmny2=cliney+((pylim-cliney)/20)*3.5
  cmny1=cliney+((pylim-cliney)/20)*1.5
  ytitle_f = "italic('T. trichiura')"
  }
  
  yaxis_shift = 0.64
    
  g1=ggplot(mn,aes(x=x,y=mn.f))+
    geom_point(aes(col=x),alpha=0.7,size=1.5,show.legend=FALSE)+
    geom_errorbar(aes(ymin=lb,ymax=ub,col=x),width=0.11,show.legend=FALSE)+
    geom_hline(yintercept=cliney,linetype="dashed")+
    coord_cartesian(ylim = c(pymin, pylim),xlim=c(1,7)) +
    scale_color_manual("",values=cols)+
    ylab("Geometric mean\neggs per gram")+xlab("")+
    scale_y_continuous(breaks=pbreaks,labels=plabels)+
    theme_complete_bw() +
    theme(plot.margin = unit(c(t = 3.7, r = 0.1, b = -0.5, l = 0.1), "lines"))+
    
    # add control mean to plot
    annotate(geom="text",x=0.6,y=cmny2,label="Control",size=3)+
    annotate(geom="text",x=0.58,y=cmny1,label="mean",size=3)+
    
    # add sth name
    annotate(geom="text",x=ytitlex,y=pylim*(yaxis_shift+linediff*3+secdiff*2+0.4),label=ytitle_f,size=5.5, 
             parse = TRUE)+
    
    # add treatment arm labels
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*4+secdiff*2+0.2),
             label=c("Control","Water","Sanitation","Handwashing",
                     "Combined","Nutrition","Combined"),
             color=cols[1:7],size=3)+
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*4+secdiff*2+0.12),
             label=c("","","","","WSH","","Nutrition+WSH"),
             color=cols[1:7],size=3)+
    
    # add n's to plot
    annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*4+secdiff*2+0.02),
             label=n,color="#5e5f60",size=3)+
    
    # add label to plot
    # annotate(geom="text",x=-0.3,y=pylim*(yaxis_shift+linediff*3+secdiff*2), hjust=0, label="FECR (95% CI)",size=3.5)+

    # add FECR to plot
    # annotate(geom="text",x=seq(1,7,1),y=pylim*(yaxis_shift+linediff*3+secdiff*2),label=rrh1,size=2.7,col="#5e5f60")+
  
    # add means to plot
    annotate(geom="text",x=seq(1,7,1)+.15,y=mn$mn.f,label=mn$mn.f,size=4,col=cols[1:7])
  
  # remove clipping of x axis labels
  g2 <- ggplot_gtable(ggplot_build(g1))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"

  return(arrangeGrob(g2))
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
      # axis.title.y =      element_text(size = base_size * 0.8),
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
