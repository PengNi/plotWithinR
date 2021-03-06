# ---plot---
library(ggplot2)
library(reshape2)
pairrank <- read.table("data/answerpairs_sh.txt", 
                       header = TRUE, stringsAsFactors = F, sep = '\t')
mpairrank<-melt(pairrank,id=c("rank"))

cbPalette <- c('#addd8e','#d9f0a3', '#006837', '#78c679', '#31a354')
p<-ggplot(data=mpairrank, aes(x=rank, y=value, 
                               group=variable, 
                               colour=variable)) +
  geom_line(size=1.5) + theme_bw() + 
  theme(legend.position="bottom",
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm"), 
        axis.text=element_text(size=16), 
        axis.title=element_text(size=16), 
        plot.title = element_text(hjust = -0.07)) + 
  scale_colour_manual(values = cbPalette, 
                      # name  ="method",
                      breaks=c("Hamaneh", "FunSim", "NetSim", 
                               "Sun_topo", "ModuleSim"),
                      labels=c("Hamaneh", "FunSim", "NetSim", 
                               "Sun_topo", "ModuleSim")) +  
  scale_x_continuous(limits=c(0, 450), breaks = seq(0, 450, 50)) + 
  scale_y_continuous(limits=c(0, 70), breaks = seq(0, 70, 10)) +
  labs(title='B', x='The number of top-ranking dsiease pairs', 
       y='The number of answer pairs')
# ----------