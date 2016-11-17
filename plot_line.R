# ---plot---
library(ggplot2)
library(reshape2)
pairrank <- read.table("D:/Program Files/JetBrains/PyWorkspace/dsimModuleTheory/evaresult/eva70_i_answerpairs.txt", 
                       header = TRUE, stringsAsFactors = F, sep = '\t')
mpairrank<-melt(pairrank,id=c("rank"))
p<-ggplot(data=mpairrank, aes(x=rank, y=value, 
                               group=variable, 
                               colour=variable)) +
  geom_line(size=1.5) + theme_bw() + 
  theme(legend.position="bottom",
        axis.text=element_text(size=13), 
        axis.title=element_text(size=13)) + 
  scale_colour_brewer(palette="Set1", 
                      name  ="method",
                      breaks=c("FunSim", "Hamaneh", "Sun_topo", 
                               "NetSim", "ModuleSim"),
                      labels=c("FunSim", "Hamaneh", "Sun_topo", 
                               "NetSim", "ModuleSim")) +  
  scale_x_continuous(limits=c(0, 450), breaks = seq(0, 450, 50)) + 
  scale_y_continuous(limits=c(0, 70), breaks = seq(0, 70, 10)) +
  labs(title='', x='The number of top-ranking dsiease pairs', 
       y='The number of answer pairs')
# ----------