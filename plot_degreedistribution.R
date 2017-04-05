# ---plot-------
library(ggplot2)
library(dSimer)
library(grid)
library(gridExtra)

# ---disease gene-----
p <- plot_dgDegreeDistribution("d:/Documents/workspace/rworkspace/helloworld_R/gene_disease_assos_doid2entrezid_disgenetcutoff006.tsv", 
                               1, 2, "A                                                     DisGeNET")
p1 <- plot_dgDegreeDistribution("d:/Documents/workspace/rworkspace/helloworld_R/rwr_dgassos_sidd_entrezid.tab", 
                                1, 2, "B                                                          SIDD")
mylegend<-g_legend(p)
mplots <- grid.arrange(arrangeGrob(p + theme(legend.position="none"),
                                   p1 + theme(legend.position="none"),
                                   nrow=1),
                       mylegend, nrow=2,heights=c(10, 1))
# ---------------


# ---disease mirna
p <- plot_dgDegreeDistribution("d:/Documents/workspace/pyworkspace/BiRW/data/birw_mim2mirna/omim2mirnapre_mimminerneedle.txt", 
                               1, 2, "")

# ----------------------------------------------------



# ---plot function----
plot_dgDegreeDistribution <- function(dgfile, dcol, gcol, plottitle){
  dgdf<-read.table(dgfile, 
                   sep = '\t', 
                   stringsAsFactors = FALSE, 
                   header = FALSE)
  dglist <- x2y_df2list(dgdf, xcol = dcol, ycol = gcol)
  gdlist <- x2y_conv2_y2x(dglist)
  
  dgcount <- sapply(dglist, length)
  gdcount <- sapply(gdlist, length)
  ddensity <- hist(dgcount, -1:max(dgcount), plot = FALSE)$density
  gdensity <- hist(gdcount, -1:max(gdcount), plot = FALSE)$density
  
  dmelt <- data.frame(type=rep('disease', length(ddensity)), 
                      xs=c(0:(length(ddensity)-1)), 
                      ys=ddensity)
  gmelt <- data.frame(type=rep('gene', length(gdensity)), 
                      xs=c(0:(length(gdensity)-1)), 
                      ys=gdensity)
  pmelt <- rbind.data.frame(dmelt, gmelt)
  pmelt$type <- as.character(pmelt$type)
  pmelt <- pmelt[pmelt$ys != 0.0, ]
  
  p <- ggplot(pmelt, aes(x=xs, y=ys, group = type)) + 
    geom_point(aes(shape=type, colour=type), size = 3, stroke = 2) + 
    theme_bw() + 
    scale_shape_manual(values=c(1,2)) + 
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)), 
                  limits = c(10^-5, 1)) + 
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)), 
                  limits = c(1, 3000)) + 
    theme(legend.position="bottom", 
          legend.title=element_blank(),
          legend.text = element_text(size = 20), 
          axis.ticks = element_blank(),
          axis.text=element_text(size=15), 
          axis.title=element_text(size=15), 
          plot.title = element_text(hjust = -0.13)) + 
    labs(title=plottitle, x='Degree', y='Frequency') + 
    annotation_logticks(short = unit(.05,"cm"),
                        mid = unit(0.1,"cm"),
                        long = unit(0.2,"cm")) 
  return(p)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(a)
mplots <- grid.arrange(arrangeGrob(a + theme(legend.position="none"),
                                         b + theme(legend.position="none"),
                                         c + theme(legend.position="none"),
                                         nrow=1),
                             mylegend, nrow=2,heights=c(10, 1))
# -------------------


# -- ref in igraph -----
library(igraph)
degree_distribution()
# ----------------------