# ---plot---
library(ggplot2)
library(reshape2)
pairrank <- read.table("data/answerpairs_sh.txt", 
                       header = TRUE, stringsAsFactors = F, sep = '\t')
mpairrank<-melt(pairrank,id=c("rank"))
p<-ggplot(data=mpairrank, aes(x=rank, y=value, 
                               group=variable, 
                               colour=variable)) +
  geom_line(size=1.5) + theme_bw() + 
  theme(legend.position="bottom",
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm"), 
        axis.text=element_text(size=15), 
        axis.title=element_text(size=15), 
        plot.title = element_text(hjust = -0.08)) + 
  scale_colour_brewer(palette="Set1", 
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