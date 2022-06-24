library(tidyverse)

library(openxlsx)
install.packages(rio)

export(spindle_analysis, "CBS_spindle_analysis.xlsx")

library(tibble)
library(ggpubr)

#making a summary of the data worked with YouTube vid Data science with Yan

spindle_analysis3<-read.csv(file.choose())

spindle_analysis3$gene <- factor(spindle_analysis3$gene, levels =c("Ncon",  "Pcon","siFAM207A","siRBMS2","siPDXDC1", "siHN1L","siPRRC2C","siFAM208B", "siKIAA1671","siKIAA1143"))
spindle_analysis3$count <- as.numeric(spindle_analysis3$count)

CBS_sa<- ggplot(spindle_analysis3, aes(y=count, x=gene, fill = phenotype ))+
  geom_bar(stat ="identity", position = "dodge") + 
  theme( axis.line = element_line(colour = "black", 
                                  size = 1, linetype = "solid")) +
  labs(title="Spindle phenotype siCyclin B Subtrates transfected \n in U2OS CDK1-AS cells after 48 hours ",
          x ="Genes", y = "Number of Cells")
  

CBS_sa2 <-CBS_sa +
  theme(axis.line = element_line(colour = "black", size = 1,linetype = "solid"))+
  theme_classic()

CBS_sa2 + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(color = "black", size = 20))

ggsave("Fig_spindle_analysis_CBS_211119.jpg", width = 40, height = 16, units = c("cm"), dpi = 300)
