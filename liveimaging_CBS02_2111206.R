library(tidyverse)
liveimaging_1<-read.csv(file.choose())

# choose the file 'CBS02_mitosis_liveimaging_211206_CSV' in CBS folder titled Raw data 

liveimaging_2 <- liveimaging_1

liveimaging_2$Gene_name <- as.factor(liveimaging_2$Gene_name)

#G2 Boxplot

G2_boxplot_3<-ggplot(liveimaging_2, aes(x=Gene_name, y=G2_length, color=Gene_name)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(limits=c("NCon", 
                            "FAM207A","RBMS2","PDXDC1", "HN1L",
                            "PRRC2C","FAM208B", "KIAA1671","KIAA1143"))

G2_boxplot_4<-G2_boxplot_3 + labs(title="Cyclin B substrates G2 duration exp 02", x="Gene",
                 y= "G2 length (minutes)") + scale_y_continuous(breaks=seq(0, 700, 100)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

G2_boxplot_4

# Finding the mean of groups in the dataframe for G2 & M

liveimaging_1 %>%
  group_by(Gene_name) %>%
  summarise_at(vars(G2_length, M_length), list(name = mean))

# i want to add an anova test for G2

FFS_plotA<-ggboxplot(liveimaging_1, x = "Gene_name", y = "G2_length", 
                     add = "jitter", legend = "siCyclin B substrates G2 duration") +
  rotate_x_text(angle = 45) +
  geom_hline(yintercept = mean(liveimaging_1$G2_length), linetype = 2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  ggtitle("siCyclin B substrates G2 duration exp 02") +
  xlab("Gene name") + ylab("G2 Duration (min)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 800) +        
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NCon")

FFS_plotA

#M Boxplot

M_boxplot_r1<-ggplot(liveimaging_2, aes(x=Gene_name, y= M_length)) +
  geom_boxplot(notch=FALSE) + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(limits=c("NCon", 
                            "FAM207A","RBMS2","PDXDC1", "HN1L",
                            "PRRC2C","FAM208B", "KIAA1671","KIAA1143"))


M_boxplot_2 <-M_boxplot_r1 + labs(title="Cyclin B substrates M duration exp 02", x="Gene",
                                 y= "M length (minutes)") + scale_y_continuous(breaks=seq(0, 350, 50)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

M_boxplot_2

# Add an annova test to the M plot

FFS_plotB<-ggboxplot(liveimaging_1, x = "Gene_name", y = "M_length",
                     add = "jitter", legend = "Cyclin B substrates M duration") +
  rotate_x_text(angle = 45) +
  geom_hline(yintercept = mean(liveimaging_1$M_length), linetype = 2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  ggtitle("siCyclin B substrates Mitosis duration exp 02") +
  xlab("Gene name") + ylab("M Duration (min)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +        
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NCon")

FFS_plotB


ggarrange(FFS_plotA, FFS_plotB + rremove(FALSE), 
          labels = c("A"),
          ncol = 2, nrow = 1)

ggsave("Fig_liveimaging_CBSexp02.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)
