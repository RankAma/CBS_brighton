###### Everything below this line worked.
# Loading dotplot file

library(tidyverse)

library(ggpubr)

library(rstatix)
liveimaging_A<-read.csv(file.choose())

# choose the file 'mitosis_liveimaging_211119_format' in CBS folder titled Raw data
# mitosis_liveimaging_211119_format without positive control


liveimaging_B <- liveimaging_A

liveimaging_B$Gene_name <- as.factor(liveimaging_B$Gene_name)



# Finding the mean of groups in the dataframe for G2 & M

liveimaging_A %>%
  group_by(Gene_name) %>%
  summarise_at(vars(G2_length, M_length), list(name = mean))

# If i want to add an Anova test for G2

FFS_plot2<-ggboxplot(liveimaging_A, x = "Gene_name", y = "G2_length", 
                     add = "jitter", legend = "Cyclin B substrates G2 duration") +
  rotate_x_text(angle = 45) +
  geom_hline(yintercept = mean(liveimaging_A$G2_length), linetype = 2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  ggtitle("siCyclin B substrates G2 duration exp 01") +
  xlab("Gene name") + ylab("G2 Duration (min)") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 800) +       
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NCon")

FFS_plot2

 

# If i want to add an Anova test for M

FFS_plot3<-ggboxplot(liveimaging_A, x = "Gene_name", y = "M_length", 
                     add = "jitter", legend = "Cyclin B substrates M duration") +
  rotate_x_text(angle = 45) +
  geom_hline(yintercept = mean(liveimaging_A$M_length), linetype = 2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  ggtitle("siCyclin B substrates Mitosis duration exp 01") +
  xlab("Gene name") + ylab("M Duration (min)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 400) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NCon")
FFS_plot3


# putting both charts together and form a figure
ggarrange(FFS_plot2, FFS_plot3 + rremove(FALSE), 
          labels = c("A"),
          ncol = 2, nrow = 1)

ggsave("Fig_liveimaging_CBSexp01.pdf", width = 40, height = 16, units = c("cm"), dpi = 300)


#### A pair wise comparison for G2


compare_means(G2_length ~ Gene_name,  data = liveimaging_A, ref.group ="NCon",  method = "t.test")

my_comparisons <- list(c("NCon", "FAM207A"), c("NCon", "KIAA1671"), c("NCon", "KIAA1143"))

ggboxplot(liveimaging_A, x = "Gene_name", y = "G2_length",add = "jitter")+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+ 
  stat_compare_means(method = "anova",label.y = 45)

#### A pair wise comparison for M

compare_means(M_length ~ Gene_name,  data = liveimaging_A, ref.group ="NCon",  method = "t.test")

my_comparisons <- list(c("NCon", "FAM207A"), c("NCon", "KIAA1671"), c("NCon", "KIAA1143"))

ggboxplot(liveimaging_A, x = "Gene_name", y = "M_length",add = "jitter")+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")
