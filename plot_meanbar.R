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

# ----plot---
library(ggplot2)
library(reshape2)
aucs <- read.table('data/auc_sh.txt', 
                  header = TRUE, sep = '\t', stringsAsFactors = FALSE)
maucs <- melt(aucs,id=c('trial'))
bardata <- summarySE(maucs, measurevar = "value", groupvars = c('variable'))
# bardata <- bardata[order(bardata$value),]

cbPalette <- c('#addd8e','#d9f0a3', '#006837', '#78c679', '#31a354')
p <- ggplot(bardata, aes(x=variable, y=value, fill=variable)) + 
  geom_bar(width=0.6, stat="identity", 
           position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_manual(values = cbPalette, 
                    breaks = c("Hamaneh", "FunSim", "NetSim", "Sun_topo", "ModuleSim"), 
                    labels = c("Hamaneh", "FunSim", "NetSim", "Sun_topo", "ModuleSim")) + 
  theme(legend.position="none", 
                     axis.text=element_text(size=16), 
                     axis.title=element_text(size=16), 
                     plot.title = element_text(hjust = -0.08)) +
  coord_cartesian(ylim=c(0.85, 1)) + 
  scale_x_discrete(limits=c("Hamaneh", "FunSim", "NetSim", "Sun_topo", "ModuleSim")) + 
  labs(title='A', x='method', y='Average of AUC')
# -----------
# ---plot2---
library(ggplot2)
library(reshape2)
avgv_di <- read.table('D:/Documents/workspace/pyworkspace/dsimModuleTheory/evaresult/evaclassification_di.tsv', 
                   header = TRUE, sep = '\t', stringsAsFactors = FALSE)
avgv_dh <- read.table('D:/Documents/workspace/pyworkspace/dsimModuleTheory/evaresult/evaclassification_dh.tsv', 
                      header = TRUE, sep = '\t', stringsAsFactors = FALSE)
avgv_si <- read.table('D:/Documents/workspace/pyworkspace/dsimModuleTheory/evaresult/evaclassification_si.tsv', 
                      header = TRUE, sep = '\t', stringsAsFactors = FALSE)
avgv_sh <- read.table('D:/Documents/workspace/pyworkspace/dsimModuleTheory/evaresult/evaclassification_sh.tsv', 
                      header = TRUE, sep = '\t', stringsAsFactors = FALSE)
avgv <- rbind(avgv_di, avgv_dh, avgv_si, avgv_sh)
mavgv <- summarySE(avgv, measurevar = 'value', groupvars = c('dataset', 'category'))
mavgv$category <- factor(mavgv$category, levels = c('all', 'same', 'diff'))

cbPalette <- c('#fc8d59','#e34a33', '#b30000')

p <- ggplot(mavgv, aes(x=dataset, y=value, fill=category)) + 
  geom_bar(size=.6, 
           stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd),
                size=.6, 
                width=.3,                    # Width of the error bars
                position=position_dodge(0.9)) + 
  theme_bw() + 
  scale_fill_manual(values=cbPalette, 
                    breaks=c('all', 'same', 'diff'),
                    labels=c('all', 'same', 'different')) + 
  theme(legend.position="bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 18),
        axis.text=element_text(size=18), 
        axis.title=element_text(size=18)) +
  coord_cartesian(ylim=c(-0.05, 0.6)) + 
  scale_x_discrete(limits=c("di", "dh", "si", "sh"), 
                   labels=c("DisGeNET_interactome", "DisGeNET_hPPIN", 
                            "SIDD_interactome", "SIDD_hPPIN")) + 
  labs(title='', x='dataset', y='Average similarity score of disease pairs')
# -----------





