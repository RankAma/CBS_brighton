
# boxplot for each variable 

library(tidyverse)
library(ggpubr)

spindle_M<-read.csv(file.choose())

#T-test of siCBS Spindle length compared to N_control

compare_means(Spindle_length.in.um ~ siRNA,  
              data = spindle_M, ref.group ="N_control",  method = "t.test")

#This is bar plot for Spindle length that i will assign to a variable 

pt1 <- ggbarplot(spindle_M, x = 'siRNA', y = 'Spindle_length.in.um',
            add = c('mean_sd'), fill = 'gray') +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control") + 
  ggtitle("") +
  xlab("siRNA Cyclin B substrate") + ylab("Spindle Length (um)")



Length <- pt1 + theme(axis.title.x = element_text(color="black", size=12, face="bold"),
axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme( 
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid"))

Length

ggsave("FigB_spindleLength.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)

#T-test of siCBS Spindle width compared to N_control

compare_means(Spindle_width.in.um ~ siRNA,  
              data = spindle_M, ref.group ="N_control",  method = "t.test")
#Spindle width

pt2 <- ggbarplot(spindle_M, x = 'siRNA', y = 'Spindle_width.in.um',
                 add = c('mean_sd'), fill = 'gray') +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control") + 
  ggtitle("") +
  xlab("siRNA Cyclin B substrate") + ylab("Spindle Width (um)")



Width <- pt2 + theme(axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold")) + 
  scale_y_continuous(name="Spindle Width (um)", limits=c(0, 20)) +
  theme( 
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid")
  )

Width

ggsave("FigC_spindlewidth.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)

#metaphase plate width

pt3 <- ggbarplot(spindle_M, x = 'siRNA', y = 'chromosome_width.in.um',
                 add = c('mean_sd'), fill ='gray') +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control") + 
  labs(title = "",
       x = "siRNA Cyclin B substrate",
       y = "Metaphase plate width (um)")


Metaphase_width <- pt3 +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
                     axis.title.y = element_text(color="black", size=12, face="bold")) + 
  scale_y_continuous(name="Metaphase plate width (um)", limits=c(0, 10)) +
  theme( 
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid")
  )

Metaphase_width

ggsave("FigD_spindle_MP.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)

# Spindle aspect ratio

Aspect_ratio<-ggboxplot(spindle_M, x = "siRNA", y = "Aspect_ratio", fill = "gray",
                          legend = "siCyclin B substrates Mitosis Spindle length") +
  rotate_x_text(angle = 45) +
  geom_hline(yintercept = mean(spindle_M$Aspect_ratio), linetype = 2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  ggtitle("") +
  xlab("siRNA Cyclin B substrate") + ylab("Aspect ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "N_control") + 
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
                                                      axis.title.y = element_text(color="black", size=12, face="bold"))
Aspect_ratio

ggsave("FigE_spindle_AR.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)

# adding charts together
ggarrange(Length, Width, Metaphase_width, Aspect_ratio + rremove(FALSE), 
          labels = c("A"),
          ncol = 2, nrow = 2)

# try and make df using the means

Average_spindle_M <-spindle_M%>%
  group_by(siRNA) %>%
  summarise_at(vars(-Image_name), funs(mean(., na.rm=TRUE)))

class(Average_spindle_M)

# Relationships

# spindle width

SWvSL<-ggplot(spindle_M, aes(x=Spindle_length.in.um, y=Spindle_width.in.um, color=siRNA)) +
  geom_point() + 
  scale_shape_manual(values=seq(0,9)) + 
  geom_smooth(aes(color = siRNA), method = lm, 
              se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(values = c("#db181f", "#39db18", "#1d138f", "#8f8d13", "#bd66ba", "#133fa8", "#a86713","#e3ce14", "#120f0c")) +
  ggpubr::stat_cor(aes(color = siRNA), label.x = 5) +
  xlim(5, 20) +
  ylim(5, 20) +
  theme_minimal()

SWvSL +  ggtitle("") +
  xlab("Spindle Length (um)") + 
  ylab("Spindle Width (um)") + 
  theme(axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))

# Spindle width and metaphase plate

SWvMP<-ggplot(spindle_M, aes(x=chromosome_width.in.um, y=Spindle_width.in.um, color=siRNA)) +
  geom_point() + 
  scale_shape_manual(values=seq(0,9)) + 
  scale_color_manual(values = c("#db181f", "#39db18", "#1d138f", "#8f8d13", "#bd66ba", "#133fa8", "#a86713","#e3ce14", "#120f0c")) +
  theme_minimal()

SWvMP +  ggtitle("") +
  xlab("Metaphase Plate (um)") + 
  ylab("Spindle Width (um)") + 
  theme(axis.line = element_line(color = "black", 
                                 size = 1, linetype = "solid")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))

# spindle length and metaphase plate

SLvMP<-ggplot(spindle_M, aes(x=chromosome_width.in.um, y=Spindle_length.in.um, color=siRNA)) +
  geom_point() + 
  scale_shape_manual(values=seq(0,9)) + 
  geom_smooth(aes(color = siRNA), method = lm, 
              se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(values = c("#db181f", "#39db18", "#1d138f", "#8f8d13", "#bd66ba", "#133fa8", "#a86713","#e3ce14", "#120f0c")) +
  ggpubr::stat_cor(aes(color = siRNA), label.x = 10) +
  theme_minimal()

SLvMP +  ggtitle("") +
  xlab("Metaphase Plate (um)") + 
  ylab("Spindle length (um)") + 
  theme(axis.line = element_line(color = "black", 
                                 size = 1, linetype = "solid")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))


