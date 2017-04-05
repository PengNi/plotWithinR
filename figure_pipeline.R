library(reshape2)
library(ggplot2)
library(gridExtra)
library(grid)



#blank plot
grid.rect(gp=gpar(col="white"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

go_human_y2h_bp<-read.table("GO_human_y2h_BP.txt",sep = "\t",header = TRUE, stringsAsFactors = F,check.names = FALSE)
go_human_y2h_cc<-read.table("GO_human_y2h_CC.txt",sep = "\t",header = TRUE,stringsAsFactors = F,check.names = FALSE)
go_human_y2h_mf<-read.table("GO_human_y2h_MF.txt",sep = "\t",header = TRUE,stringsAsFactors = F,check.names = FALSE)


melt_go_human_y2h_bp<-melt(go_human_y2h_bp,id.vars = "xaxis")
melt_go_human_y2h_cc<-melt(go_human_y2h_cc,id.vars = "xaxis")
melt_go_human_y2h_mf<-melt(go_human_y2h_mf,id.vars = "xaxis")

bp<-ggplot(data = melt_go_human_y2h_bp, 
          aes(x=xaxis,
              y = value,
              group=variable,
              colour=variable,
              shape=variable))+
  geom_line(size=0.8)+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(0:2,4,15:18),name="methods")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
        panel.grid.major = element_line(colour="azure3", size=0.5))+
  labs(x="x",y="y",title="go_human_y2h_bp")+
  theme(legend.position="bottom")+
  scale_color_discrete(name="methods")+
  scale_x_continuous(limits = c(0.05,1),
                     breaks=seq(0.1, 1, 0.1),
                     minor_breaks = seq(0.05,1,0.05))+
  scale_y_continuous(#limits = c(0,0.4),
    #breaks = seq(0.0,0.4,0.05),
    minor_breaks = waiver())
cc<-ggplot(data = melt_go_human_y2h_cc, 
          aes(x=xaxis,
              y = value,
              group=variable,
              colour=variable,
              shape=variable))+
  geom_line(size=0.8)+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(0:2,4,15:18),name="methods")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
        panel.grid.major = element_line(colour="azure3", size=0.5))+
  labs(x="x",y="y",title="go_human_y2h_cc")+
  theme(legend.position="bottom")+
  scale_color_discrete(name="methods")+
  scale_x_continuous(limits = c(0.05,1),
                     breaks=seq(0.1, 1, 0.1),
                     minor_breaks = seq(0.05,1,0.05))+
  scale_y_continuous(#limits = c(0,0.4),
    #breaks = seq(0.0,0.4,0.05),
    minor_breaks = waiver())
mf<-ggplot(data = melt_go_human_y2h_mf, 
          aes(x=xaxis,
              y = value,
              group=variable,
              colour=variable,
              shape=variable))+
  geom_line(size=0.8)+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(0:2,4,15:18),name="methods")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
        panel.grid.major = element_line(colour="azure3", size=0.5))+
  labs(x="x",y="y",title="go_human_y2h_mf")+
  theme(legend.position="bottom")+
  scale_color_discrete(name="methods")+
  scale_x_continuous(limits = c(0.05,1),
                     breaks=seq(0.1, 1, 0.1),
                     minor_breaks = seq(0.05,1,0.05))+
  scale_y_continuous(#limits = c(0,0.4),
    #breaks = seq(0.0,0.4,0.05),
    minor_breaks = waiver())


mylegend<-g_legend(bp)
go_human_y2h <- grid.arrange(arrangeGrob(bp + theme(legend.position="none"),
                                         cc + theme(legend.position="none"),
                                         mf + theme(legend.position="none"),
                                         nrow=1),
                             mylegend, nrow=2,heights=c(10, 1))





goplot<-function(bpfile,ccfile,mffile,bptitle="bp",
                 cctitle="cc",mftitle="mf",xtitle="x",
                 ytitle="y"){
  bp<-read.table(bpfile,sep = "\t",
                              header = TRUE, stringsAsFactors = F,
                              check.names = FALSE)
  cc<-read.table(ccfile,sep = "\t",header = TRUE,
                              stringsAsFactors = F,check.names = FALSE)
  mf<-read.table(mffile,sep = "\t",header = TRUE,
                              stringsAsFactors = F,check.names = FALSE)
  bp<-melt(bp,id.vars = "xaxis")
  cc<-melt(cc,id.vars = "xaxis")
  mf<-melt(mf,id.vars = "xaxis")
  
  bp<-ggplot(data = bp, 
             aes(x=xaxis,
                 y = value,
                 group=variable,
                 colour=variable,
                 shape=variable))+
    geom_line(size=0.8)+
    geom_point(size=2.5)+
    scale_shape_manual(values=c(0:2,4,6,15,17,19))+
    theme_bw()+
    theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
          panel.grid.major = element_line(colour="azure3", size=0.5),
          legend.position="bottom",
          legend.title=element_blank(),
          legend.direction="horizontal",
          plot.title = element_text(hjust = -0.15))+
    labs(x=xtitle,y=ytitle,title=bptitle)+
    scale_x_continuous(limits = c(0.05,1),
                       breaks=seq(0.1, 1, 0.1),
                       minor_breaks = seq(0.05,1,0.05))+
    scale_y_continuous(#limits = c(0,0.4),
      #breaks = seq(0.0,0.4,0.05),
      minor_breaks = waiver())
  
  cc<-ggplot(data = cc, 
             aes(x=xaxis,
                 y = value,
                 group=variable,
                 colour=variable,
                 shape=variable))+
    geom_line(size=0.8)+
    geom_point(size=2.5)+
    scale_shape_manual(values=c(0:2,4,6,15,17,19),name="methods")+
    theme_bw()+
    theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
          panel.grid.major = element_line(colour="azure3", size=0.5))+
    labs(x=xtitle,y=ytitle,title=cctitle)+
    theme(legend.position="bottom",
          plot.title = element_text(hjust = -0.15))+
    scale_color_discrete(name="methods")+
    scale_x_continuous(limits = c(0.05,1),
                       breaks=seq(0.1, 1, 0.1),
                       minor_breaks = seq(0.05,1,0.05))+
    scale_y_continuous(#limits = c(0,0.4),
      #breaks = seq(0.0,0.4,0.05),
      minor_breaks = waiver())
  
  mf<-ggplot(data = mf, 
             aes(x=xaxis,
                 y = value,
                 group=variable,
                 colour=variable,
                 shape=variable))+
    geom_line(size=0.8)+
    geom_point(size=2.5)+
    scale_shape_manual(values=c(0:2,4,6,15,17,19),name="methods")+
    theme_bw()+
    theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
          panel.grid.major = element_line(colour="azure3", size=0.5))+
    labs(x=xtitle,y=ytitle,title=mftitle)+
    theme(legend.position="bottom",
          plot.title = element_text(hjust = -0.15))+
    scale_color_discrete(name="methods")+
    scale_x_continuous(limits = c(0.05,1),
                       breaks=seq(0.1, 1, 0.1),
                       minor_breaks = seq(0.05,1,0.05))+
    scale_y_continuous(#limits = c(0,0.4),
      #breaks = seq(0.0,0.4,0.05),
      minor_breaks = waiver())
  
  mylegend<-g_legend(bp)
  go<- grid.arrange(arrangeGrob(bp + theme(legend.position="none"),
                                           cc + theme(legend.position="none"),
                                           mf + theme(legend.position="none"),
                                           nrow=1),
                               mylegend, nrow=2,heights=c(10, 1))
}



go_human_y2h<-goplot("GO_human_y2h_BP.txt","GO_human_y2h_CC.txt","GO_human_y2h_MF.txt",bptitle = "BP(Y2H_Human)",cctitle = "CC(Y2H_Human)",mftitle = "MF(Y2H_Human)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")
go_human_phy<-goplot(bpfile = "GO_human_phy_BP.txt",ccfile = "GO_human_phy_CC.txt",mffile = "GO_human_phy_MF.txt",bptitle = "BP(Physical_Human)",cctitle = "CC(Physical_Human)",mftitle = "MF(Physical_Human)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")
go_human_ms<-goplot(bpfile = "GO_human_ms_BP.txt",ccfile = "GO_human_ms_CC.txt",mffile = "GO_human_ms_MF.txt",bptitle = "BP(MS_Human)",cctitle = "CC(MS_Human)",mftitle = "MF(MS_Human)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")

go_yeast_y2h<-goplot(bpfile = "GO_yeast_y2h_BP.txt",ccfile = "GO_yeast_y2h_CC.txt",mffile = "GO_yeast_y2h_MF.txt",bptitle = "A                        BP(Y2H_Yeast)",cctitle = "B                        CC(Y2H_Yeast)",mftitle = "C                        MF(Y2H_Yeast)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")
go_yeast_phy<-goplot(bpfile = "GO_yeast_phy_BP.txt",ccfile = "GO_yeast_phy_CC.txt",mffile = "GO_yeast_phy_MF.txt",bptitle = "A                        BP(Physical_Yeast)",cctitle = "B                        CC(Physical_Yeast)",mftitle = "C                        MF(Physical_Yeast)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")
go_yeast_ms<-goplot(bpfile = "go_yeast_ms_BP.txt",ccfile = "go_yeast_ms_CC.txt",mffile = "go_yeast_ms_MF.txt",bptitle = "A                        BP(MS_Yeast)",cctitle = "B                        CC(MS_Yeast)",mftitle = "C                        MF(MS_Yeast)",xtitle = "Proportion of Top-scoring interactions",ytitle = "Average GO semantic similarity")



#-------------------------------------------dip_yeast and hippie_human---
single_plot<-function(file,title,xtitle,ytitle){
  f<-read.table(file,sep = "\t",
                 header = TRUE, stringsAsFactors = F,
                 check.names = FALSE)
  f<-melt(f,id.vars = "xaxis")
  
  f<-ggplot(data = f, 
             aes(x=xaxis,
                 y = value,
                 group=variable,
                 colour=variable,
                 shape=variable))+
    geom_line(size=0.8)+
    geom_point(size=2.5)+
    scale_shape_manual(values=c(0:2,4,6,15,17,19))+
    theme_bw()+
    theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
          panel.grid.major = element_line(colour="azure3", size=0.5),
          legend.position="bottom",
          legend.title=element_blank(),
          legend.direction="horizontal",
          plot.title = element_text(hjust = -0.15))+
    labs(x=xtitle,y=ytitle,title=title)+
    scale_x_continuous(limits = c(0.05,1),
                       breaks=seq(0.1, 1, 0.1),
                       minor_breaks = seq(0.05,1,0.05))+
    scale_y_continuous(#limits = c(0,0.4),
      #breaks = seq(0.0,0.4,0.05),
      minor_breaks = waiver())
  return(f)
}

dip_yeast_y2h<-single_plot("DIP_yeast_y2h.txt","A                              Y2H_Yeast","Proportion of Top-scoring interactions","Overlaps with DIP core set of yeast")
dip_yeast_phy<-single_plot("DIP_yeast_phy.txt","B                             Physical_Yeast","Proportion of Top-scoring interactions","Overlaps with DIP core set of yeast")
dip_yeast_ms<-single_plot("DIP_yeast_ms.txt","C                             MS_Yeast","Proportion of Top-scoring interactions","Overlaps with DIP core set of yeast")
dip_yeast<-grid.arrange(arrangeGrob(dip_yeast_y2h+theme(legend.text=element_text(size=6)),dip_yeast_phy+theme(legend.text=element_text(size=8)),dip_yeast_ms+theme(legend.text=element_text(size=6)),nrow=1),nrow=1)
dip_yeast<-grid.arrange(arrangeGrob(dip_yeast_y2h+theme(legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.text=element_text(size=6),legend.direction="vertical"),dip_yeast_phy+theme(legend.text=element_text(size=8),legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.direction="vertical"),dip_yeast_ms+theme(legend.text=element_text(size=6.5),legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.direction="vertical"),nrow=1),nrow=1)

hippie_human_y2h<-single_plot("hippie_human_y2h.txt","A                          Y2H_Human","Proportion of Top-scoring interactions","Overlaps with 10% top-scoring interactions in Hippie database")
hippie_human_phy<-single_plot("hippie_human_phy.txt","B                          Physical_Human","Proportion of Top-scoring interactions","Overlaps with 10% top-scoring interactions in Hippie database")
hippie_human_ms<-single_plot("hippie_human_ms.txt","C                          MS_Human","Proportion of Top-scoring interactions","Overlaps with 10% top-scoring interactions in Hippie database")
hippie_human<-grid.arrange(arrangeGrob(hippie_human_y2h+theme(legend.text=element_text(size=6)),hippie_human_phy+theme(legend.text=element_text(size=8)),hippie_human_ms+theme(legend.text=element_text(size=6)),nrow=1),nrow=1)
hippie_human<-grid.arrange(arrangeGrob(hippie_human_y2h+theme(legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.text=element_text(size=6),legend.direction="vertical",axis.title.y=element_text(size=11)),hippie_human_phy+theme(legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.text=element_text(size=8),legend.direction="vertical",axis.title.y=element_text(size=11)),hippie_human_ms+theme(legend.justification=c(1,0), legend.position=c(1.022,-0.023),legend.text=element_text(size=8),legend.direction="vertical",axis.title.y=element_text(size=11)),nrow=1),nrow=1)

#------------------------------------------------------------
PR<-read.table("PR_data.txt",header = T,sep = "\t",stringsAsFactors = F,check.names = F)
ROC<-read.table("ROC_data.txt",header = T,sep = "\t",stringsAsFactors = F,check.names = F)

PR_coexpression<-PR[,c("coexpression.GDS1067_Pr","coexpression.GDS1067_Re","coexpression.GDS1096_Pr","coexpression.GDS1096_Re","coexpression.GDS1329_Pr","coexpression.GDS1329_Re","coexpression.GDS1479_Pr","coexpression.GDS1479_Re","coexpression.GDS1815_Pr","coexpression.GDS1815_Re","coexpression.GDS181_Pr","coexpression.GDS181_Re","coexpression.GDS1956_Pr","coexpression.GDS1956_Re","coexpression.GDS1975_Pr","coexpression.GDS1975_Re","coexpression.GDS2201_Pr","coexpression.GDS2201_Re","coexpression.GDS2250_Pr","coexpression.GDS2250_Re","coexpression.GDS2520_Pr","coexpression.GDS2520_Re","coexpression.GDS2545_Pr","coexpression.GDS2545_Re","coexpression.GDS2649_Pr","coexpression.GDS2649_Re","coexpression.GDS2737_Pr","coexpression.GDS2737_Re","coexpression.GDS2785_Pr","coexpression.GDS2785_Re","coexpression.GDS2819_Pr","coexpression.GDS2819_Re","coexpression.GDS596_Pr","coexpression.GDS596_Re","coexpression.GDS963_Pr","coexpression.GDS963_Re")]
PR_ortho<-PR[,c("ortho.mouse_Pr","ortho.mouse_Re","ortho.rat_Pr","ortho.rat_Re","ortho.fly_Pr","ortho.fly_Re","ortho.worm_Pr","ortho.worm_Re","ortho.yeast_Pr","ortho.yeast_Re","para.known.score_Pr","para.known.score_Re")]
PR_GO<-PR[,c("GO_BP_Pr","GO_BP_Re","GO_CC_Pr","GO_CC_Re","GO_MF_Pr","GO_MF_Re")]
PR_topo_known<-PR[,c("topo.known.nShared_Pr","topo.known.nShared_Re","topo.known.pShared_Pr","topo.known.pShared_Re","topo.known.ScottRatio_Pr","topo.known.ScottRatio_Re")]
PR_final_adjust<-PR[,c("final_adjusted_prob_Pr","final_adjusted_prob_Re","mergedFeatures_Pr","mergedFeatures_Re","domains_Pr","domains_Re","PTMs_Pr","PTMs_Re","physChem_Pr","physChem_Re")]

ROC_ortho<-ROC[,c("ortho.mouse_TPR","ortho.mouse_FPR","ortho.rat_TPR","ortho.rat_FPR","ortho.fly_TPR","ortho.fly_FPR","ortho.worm_TPR","ortho.worm_FPR","ortho.yeast_TPR","ortho.yeast_FPR","para.known.score_TPR","para.known.score_FPR")]
ROC_coexpression<-ROC[,c("coexpression.GDS1067_TPR","coexpression.GDS1067_FPR","coexpression.GDS1096_TPR","coexpression.GDS1096_FPR","coexpression.GDS1329_TPR","coexpression.GDS1329_FPR","coexpression.GDS1479_TPR","coexpression.GDS1479_FPR","coexpression.GDS1815_TPR","coexpression.GDS1815_FPR","coexpression.GDS181_TPR","coexpression.GDS181_FPR","coexpression.GDS1956_TPR","coexpression.GDS1956_FPR","coexpression.GDS1975_TPR","coexpression.GDS1975_FPR","coexpression.GDS2201_TPR","coexpression.GDS2201_FPR","coexpression.GDS2250_TPR","coexpression.GDS2250_FPR","coexpression.GDS2520_TPR","coexpression.GDS2520_FPR","coexpression.GDS2545_TPR","coexpression.GDS2545_FPR","coexpression.GDS2649_TPR","coexpression.GDS2649_FPR","coexpression.GDS2737_TPR","coexpression.GDS2737_FPR","coexpression.GDS2785_TPR","coexpression.GDS2785_FPR","coexpression.GDS2819_TPR","coexpression.GDS2819_FPR","coexpression.GDS596_TPR","coexpression.GDS596_FPR","coexpression.GDS963_TPR","coexpression.GDS963_FPR")]
ROC_GO<-ROC[,c("GO_BP_TPR","GO_BP_FPR","GO_CC_TPR","GO_CC_FPR","GO_MF_TPR","GO_MF_FPR")]
ROC_topo<-ROC[,c("topo.known.nShared_TPR","topo.known.nShared_FPR","topo.known.pShared_TPR","topo.known.pShared_FPR","topo.known.ScottRatio_TPR","topo.known.ScottRatio_FPR")]
ROC_final_adjust<-ROC[,c("final_adjusted_prob_TPR","final_adjusted_prob_FPR","mergedFeatures_TPR","mergedFeatures_FPR","domains_TPR","domains_FPR","PTMs_TPR","PTMs_FPR","physChem_TPR","physChem_FPR")]

mymelt<-function(df,taillen=3){
  x<-vector()
  y<-vector()
  variable<-vector()
  for(i in 1:ncol(df)){
    if(i %% 2==0){
      cname<-colnames(df)[i]
      vname<-substr(cname,1,nchar(cname)-taillen)
      variable<-c(variable,rep(vname,nrow(df)))
      x<-c(x,df[,i])
    }else{
      y<-c(y,df[,i])
    }
  }
  return(data.frame(x,y,variable))
}

p<-ggplot(data=PR_GO_melt,aes(x=x,y=y,group=variable,colour=variable,
                              shape=variable))+
  geom_line(size=0.8)+geom_point(size=2.5)+theme_bw()+
  scale_shape_manual(values=c(0:2,4,6,15,17,19))+
  theme(panel.grid.minor = element_line(colour="azure2", size=0.5),
        panel.grid.major = element_line(colour="azure3", size=0.5),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal")+
  labs(x="Recall",y="Precision",title="title")+
  scale_x_continuous(limits = c(0.0,1),
                     breaks=seq(0.0, 1, 0.1),
                     minor_breaks = seq(0.05,1,0.05))+
  scale_y_continuous(minor_breaks = waiver())

prplot<-function(data,title="",xtitle="Recall",ytitle="Precision"){
  return(ggplot(data=data,aes(x=x,y=y,group=variable,colour=variable,
                                    shape=variable))+
           geom_line(size=0.5)+geom_point(size=1.0)+theme_bw()+
           scale_shape_manual(values=c(0:2,4,6,15,17,19,3,5,7:14,16,18,20))+
           theme(panel.grid.minor = element_line(colour="azure2", size=0.4),
                 panel.grid.major = element_line(colour="azure3", size=0.4),
                 legend.position="bottom",
                 legend.title=element_blank(),
                 legend.direction="horizontal")+
           labs(x=xtitle,y=ytitle,title=title)+
           scale_x_continuous(limits = c(0.0,1),
                              breaks=seq(0.0, 1, 0.1),
                              minor_breaks = seq(0.05,1,0.05))+
           scale_y_continuous(minor_breaks = waiver()))
}
prplot(data = PR_coexpression_melt,title = "")+theme(legend.text=element_text(size=7))

prcoexpression<-prplot(data = PR_coexpression_melt,title = "")
prgo<-prplot(data = PR_GO_melt)
prfinal<-prplot(data = PR_final_melt,title = "")
prortho<-prplot(data = PR_ortho_melt,title = "")
prtopo<-prplot(data = PR_topo_melt,title = "")

grid.arrange(arrangeGrob(prgo,prtopo+theme(legend.text=element_text(size=7.5)),prfinal+theme(legend.text=element_text(size=7.5)),nrow=1),arrangeGrob(prortho,prcoexpression+theme(legend.text=element_text(size=4.5),legend.key.size=unit(3.5, "mm")),grid.rect(gp=gpar(col="white")),nrow = 1), nrow=2,heights=c(1, 1))


rocplot<-function(data,title="",xtitle="False  Positive  Rate",ytitle="True  Positive  Rate"){
  return(ggplot(data=data,aes(x=x,y=y,group=variable,colour=variable,shape=variable))+
           geom_line(size=0.5)+geom_point(size=1.0)+theme_bw()+
           scale_shape_manual(values=c(0:2,4,6,15,17,19,3,5,7:14,16,18,20))+
           theme(panel.grid.minor = element_line(colour="azure2", size=0.4),
                 panel.grid.major = element_line(colour="azure3", size=0.4),
                 legend.position="bottom",
                 legend.title=element_blank(),
                 legend.direction="horizontal")+
           labs(x=xtitle,y=ytitle,title=title)+
           scale_x_continuous(expand = c(0, 0),
                              limits = c(0.0,1),
                              breaks=seq(0.0, 1, 0.1),
                              minor_breaks = NULL)+
           scale_y_continuous(expand = c(0, 0),
                              limits = c(0.0,1),
                              breaks=seq(0.0, 1, 0.1),
                              minor_breaks = NULL))
}
rocplot(ROC_GO_melt)+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure4",size=0.8)


roctopo<-rocplot(ROC_topo_melt)+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure3",size=0.3)
rocortho<-rocplot(ROC_ortho_melt)+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure3",size=0.3)
roccoexpression<-rocplot(ROC_coexpression_melt)+theme(legend.text=element_text(size=7))+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure3",size=0.3)
rocfinal<-rocplot(ROC_final_melt)+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure3",size=0.3)
rocgo<-rocplot(ROC_GO_melt)+geom_abline(slope = 1,intercept = 0,linetype="longdash",colour="azure3",size=0.3)

grid.arrange(arrangeGrob(rocgo,roctopo+theme(legend.text=element_text(size=7.5)),rocfinal+theme(legend.text=element_text(size=7.5)),nrow=1),arrangeGrob(rocortho,roccoexpression+theme(legend.text=element_text(size=4.5),legend.key.size=unit(3.5, "mm")),grid.rect(gp=gpar(col="white")),nrow = 1), nrow=2,heights=c(1, 1))

#--------------------------------------------------------

#--------write plots to tiff file ------------------------------------------------
load("figure.RData")
ppi= 300

tiff("hippie_human.tiff", 
     width = ppi*15, height = ppi*5, 
     units = 'px', res = ppi)
plot(hippie_human)
dev.off()

tiff("dip_yeast.tiff", 
     width = ppi*15, height = ppi*5, 
     units = 'px', res = ppi)
plot(dip_yeast)
dev.off()

tiff("go_human_ms.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_human_ms)
dev.off()

tiff("go_human_physical.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_human_phy)
dev.off()

tiff("go_human_y2h.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_human_y2h)
dev.off()

tiff("go_yeast_ms.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_yeast_ms)
dev.off()

tiff("go_yeast_physical.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_yeast_phy)
dev.off()

tiff("go_yeast_y2h.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_yeast_y2h)
dev.off()

tiff("go_yeast_ms.tiff", 
     width = ppi*15, height = ppi*5.46, 
     units = 'px', res = ppi)
plot(go_yeast_ms)
dev.off()

tiff("pr.tiff", 
     width = ppi*20, height = ppi*13, 
     units = 'px', res = ppi)
grid.arrange(arrangeGrob(prgo+theme(legend.text=element_text(size=12)),
                         prtopo+theme(legend.text=element_text(size=12)),
                         prfinal+theme(legend.text=element_text(size=12)),
                         nrow=1),
             arrangeGrob(prortho+theme(legend.text=element_text(size=12)),
                         prcoexpression+theme(legend.text=element_text(size=5),
                                              legend.key.size=unit(3.5, "mm")),
                         grid.rect(gp=gpar(col="white")),nrow = 1), 
             nrow=2,heights=c(1, 1))
dev.off()

tiff("roc.tiff", 
     width = ppi*20, height = ppi*13, 
     units = 'px', res = ppi)
grid.arrange(arrangeGrob(rocgo+theme(legend.text=element_text(size=12)),
                         roctopo+theme(legend.text=element_text(size=12)),
                         rocfinal+theme(legend.text=element_text(size=12)),
                         nrow=1),
             arrangeGrob(rocortho+theme(legend.text=element_text(size=12)),
                         roccoexpression+theme(legend.text=element_text(size=5),
                                               legend.key.size=unit(3.5, "mm")),
                         grid.rect(gp=gpar(col="white")),nrow = 1), 
             nrow=2,heights=c(1, 1))
dev.off()
#---------------------------------------------------------





