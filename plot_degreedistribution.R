# ---plot-------
library(ggplot2)
library(dSimer)
library(grid)
library(gridExtra)

# ---disease gene---------------------------------------------------------
p1 <- plot_dgDegreeDistribution("data/gene_disease_assos_doid2entrezid_disgenetcutoff006.tsv", 
                               1, 2, "A                                DisGeNET")
p2 <- plot_dgDegreeDistribution("d:/Documents/workspace/rworkspace/helloworld_R/rwr_dgassos_sidd_entrezid.tab", 
                                1, 2, "B                                  SIDD")
mylegend<-g_legend(p1)
mplots <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                   p2 + theme(legend.position="none"),
                                   nrow=1),
                       mylegend, nrow=2,heights=c(10, 1))
# ------------------------------------------------------------------------


# ---disease mirna--------------------------------------------------------
p <- plot_dgDegreeDistribution("d:/Documents/workspace/pyworkspace/BiRW/data/birw_mim2mirna/omim2mirnapre_all.txt", 
                               1, 2, "")

# ------------------------------------------------------------------------


# boxplot and histogram---------------------------------------------------
lccinfo <- read.table('data//lccinfo_zscore_sidd_hppin_manual.txt', 
                      header = T, stringsAsFactors = F, sep = "\t")
gnumcf <- 10
lccinfocf <- lccinfo[lccinfo$dgingraph>=gnumcf, ]
p3 <- ggplot(lccinfocf, aes(x=lccratio)) + 
  geom_histogram(aes(y=..count../sum(..count..)),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="#d7191c") +
  # geom_density(alpha=.2, fill="#FF6666")
  theme_bw() + 
  theme(axis.text=element_text(size=15), 
        axis.title=element_text(size=15), 
        plot.title = element_text(size = 20, hjust = -0.10)) + 
  scale_x_continuous(breaks=seq(0, 1, 0.1)) + 
  labs(title="A", x='Fraction of disease genes in LCC', y='Frequency')
p3

# --single disease lcc size compared with random exception--
# -DOID:6000
randomlccsize_6000 <- read.table('D:/Documents/workspace/pyworkspace/dsimModuleTheory/evaresult/randomlccsize_doid6000.txt', 
                                 header = F, stringsAsFactors = F, sep = "\t")
colnames(randomlccsize_6000)<-c('randomlccsize')

p4 <- ggplot(randomlccsize_6000, aes(x=randomlccsize)) + 
  geom_histogram(aes(y=..count../sum(..count..)),      # Histogram with density instead of count on y-axis
                 binwidth=5,
                 colour="black", fill="#fdae61") + 
  annotate("text", x = 100, y = 0.17, label = "random", size=5) + 
  annotate("rect", xmin = 108, xmax = 118, ymin = 0.165, ymax = 0.175, 
           alpha = 1, fill="#fdae61") + 
  annotate("text", x = 107, y = 0.11, label = "z-score = 6.9210", size=5) + 
  annotate("text", x = 106, y = 0.10, label = "Observed LCC: 109", size=5) + 
  annotate("segment", x = 109, xend = 109, y = 0.09, yend = 0.002, 
           colour="#d7191c", size=1.2, arrow=arrow()) + 
  # geom_density(alpha=.2, fill="#FF6666")
  theme_bw() + 
  theme(axis.text=element_text(size=15), 
        axis.title=element_text(size=15), 
        plot.title = element_text(size = 20, hjust = -0.10)) + 
  scale_x_continuous(breaks=seq(0, 120, 10), limits = c(0, 120)) + 
  labs(title="B", 
       x='LCC size', y='Frequency')
# LCC size of 'congestive heart failure' compared to random exception
p4


mplots <- grid.arrange(arrangeGrob(p3, p4, nrow=1))

# --total diseases lcc z-score distribution compared with random exception
p5 <- ggplot(lccinfocf, aes(y=zscore, x="z-score")) + 
  geom_boxplot(fill = "#7fcdbb",size=2) + 
  geom_hline(aes(yintercept = 1.6), linetype = 2, size=1.5) +
  theme_bw() + 
  theme(axis.text=element_text(size=15), 
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=12),
        plot.title = element_text(size = 20, hjust = -0.15)) + 
  scale_y_continuous(breaks=c(-1, 1.6, seq(0, 30, 5)), limits = c(-1, 30)) + 
  labs(title="C", 
       x='Significance of LCC sizes', y='z-score distribution')
p5


mplots <- grid.arrange(p3, p4, p5, nrow=1, widths=c(3,3,1))
# ---------------------------------------------------------------------




# ---plot function----------------------------------------------------
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
    geom_point(aes(shape=type, colour=type), size = 5, stroke = 2) + 
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
          legend.text = element_text(size = 25), 
          axis.ticks = element_blank(),
          axis.text=element_text(size=25), 
          axis.title=element_text(size=25), 
          plot.title = element_text(size=28, hjust = -0.12)) + 
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
# ----------------------------------------------------------------------


# -- ref in igraph -----
library(igraph)
degree_distribution()
# ----------------------