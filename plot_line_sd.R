# ---plot function---
# from cookbook for R http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

rawauc2bardata <-function(auc, dataname='a'){
  mauc <- melt(auc,id=c('trial'))
  bardata <- summarySE(mauc, measurevar = "value", groupvars = c('variable'))
  bardata$data = rep(dataname, nrow(bardata))
  bardata
}
# ---------------
# -----plot------
library(ggplot2)
library(reshape2)
auc_sh <- read.table('data/auc_sh.txt', 
                   header = TRUE, sep = '\t', stringsAsFactors = FALSE)
bar_sh = rawauc2bardata(auc_sh, 'sh')
auc_si <- read.table('data/auc_si.txt', 
                     header = TRUE, sep = '\t', stringsAsFactors = FALSE)
bar_si = rawauc2bardata(auc_si, 'si')
auc_dh <- read.table('data/auc_dh.txt', 
                     header = TRUE, sep = '\t', stringsAsFactors = FALSE)
bar_dh = rawauc2bardata(auc_dh, 'dh')
auc_di <- read.table('data/auc_di.txt', 
                     header = TRUE, sep = '\t', stringsAsFactors = FALSE)
bar_di = rawauc2bardata(auc_di, 'di')

bars = rbind(bar_sh, bar_si, bar_dh, bar_di)


cbPalette <- c('#addd8e','#d9f0a3', '#006837', '#78c679', '#31a354')
p <- ggplot(data=bars, aes(x=data, y=value, group=variable, colour=variable)) + 
  geom_line(size=2, position=position_dodge(0.08)) + 
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), 
                width=0.5, size=1, position=position_dodge(0.08)) + 
  geom_point(# aes(shape=variable),
             size=2.5, 
             position=position_dodge(0.08)) + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm"), 
        axis.text=element_text(size=21), 
        axis.title=element_text(size=25)) + 
  scale_colour_manual(values = cbPalette, 
                      # name  ="method",
                      breaks=c("Hamaneh", "FunSim", "NetSim", 
                               "Sun_topo", "ModuleSim"),
                      labels=c("Hamaneh", "FunSim", "NetSim", 
                               "Sun_topo", "ModuleSim")) + 
  # scale_shape_manual(values=c(0, 1, 2, 5, 6), 
  #                    breaks=c("Hamaneh", "FunSim", "NetSim", 
  #                             "Sun_topo", "ModuleSim"),
  #                    labels=c("Hamaneh", "FunSim", "NetSim", 
  #                             "Sun_topo", "ModuleSim")) + 
  labs(title='', x='Dataset', 
       y='Average of AUC') + 
  scale_x_discrete(limits=c("di","dh","si", 'sh'), 
                   labels=c("DisGeNET_interactome", "DisGeNET_hPPIN", 
                            "SIDD_interactome", "SIDD_hPPIN")) + 
  scale_y_continuous(limits=c(0.90, 0.98), breaks = seq(0.90, 0.98, 0.01))
# --------------
