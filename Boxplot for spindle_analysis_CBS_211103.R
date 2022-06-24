library(tidyverse)
library(ggpubr)
New_spindle_data<-read.csv(file.choose())

spindle_data <- New_spindle_data %>%
  mutate(siRNA = factor(siRNA, levels=c("N_control", 
                                      "siFAM207A","siRBMS2","siPDXDC1", "siHN1L",
                                      "siPRRC2C","siFAM208B", "siKIAA1671","siKIAA1143")))




NSD <- ggboxplot(spindle_data, x = "siRNA", y = "Spindle_length.in.um", 
          add = "jitter", fill='#A4A4A4', color="black") + 
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(New_spindle_data$Spindle_length.in.um), linetype = 2)+ 
   ggtitle("Active CDK1 Substrates Mean Spindle Length") +
   xlab("Gene name") + ylab(" Spindle Length (um)") +
   theme(plot.title = element_text(hjust = 0.5)) +
   ylim(5, 22) +
   stat_compare_means(label = "p.signif", method = "t.test",
                      ref.group = "N_control", hide.ns = TRUE)
NSD


NSD_width <- ggboxplot(spindle_data, x = "siRNA", y = "Spindle_width.in.um", 
                 add = "jitter", fill='#A4A4A4', color="black") + 
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(New_spindle_data$Spindle_width.in.um), linetype = 2)+ 
  ggtitle("Active CDK1 Substrates Mean Spindle Width") +
  xlab("Gene name") + ylab(" Spindle Width (um)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(5, 20) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control", hide.ns = TRUE)

NSD_width


NSD_metaphase_plate <- ggboxplot(spindle_data, x = "siRNA", y = "chromosome_width.in.um", 
                       add = "jitter", fill='#A4A4A4', color="black") + 
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(New_spindle_data$chromosome_width.in.um), linetype = 2)+ 
  ggtitle("Active CDK1 Substrates Mean Metaphase Plate Length") +
  xlab("Gene name") + ylab(" Metaphase plate length (um)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 15) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control", hide.ns = TRUE)

NSD_metaphase_plate 


NSD_Spindle_AR <- ggboxplot(spindle_data, x = "siRNA", y = "Aspect_ratio", 
                                  add = "jitter", fill='#A4A4A4', color="black") + 
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(New_spindle_data$Aspect_ratio), linetype = 2)+ 
  ggtitle("Active CDK1 Substrates Mean Aspect Ratio") +
  xlab("Gene name") + ylab(" Spindle Aspect ratio ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 3) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control", hide.ns = TRUE)

NSD_Spindle_AR



ggarrange(NSD, NSD_width, NSD_metaphase_plate , NSD_Spindle_AR  + rremove(FALSE), 
          labels = c(""),
          ncol = 2, nrow = 2)

ggsave("Boxplot Spindle Analysis.pdf", width = 60, height = 32, units = c("cm"), dpi = 300)
