library(extrafont)
font_import()
loadfonts()  
library(ggpubr)
# Volume of grey matter in Crus II Cerebellum (right)
# Mean FA in cingulum cingulate gyrus on FA skeleton (right)
# Mean MD in inferior cerebellar peduncle on FA skeleton (right)
i = 3
feature_name = features_for_plot[i]
load(sprintf("%s/%s/%s.RData",mainDir,feature_name,feature_name))
DF <- data.frame(p = bootstrap_results$bootstrap_results[,1],
                 q = bootstrap_results$bootstrap_results[,2])
ggscatter(DF, x = "p", y = "q",
          color = 'black', palette = "jco",
          size = 1, alpha = 0.6) +
  scale_x_continuous(name = waiver(), breaks = c(0,0.5, 1), labels = c(0,0.5, 1), limits = c(0,1)) + 
  scale_y_continuous(name = waiver(), breaks = c(0,0.5, 1), labels = c(0,0.5, 1), limits = c(0,1)) + 
  geom_hline(yintercept = bootstrap_results$original_em_results_list[2], linetype = "dashed", color = "black") +
  geom_vline(xintercept = bootstrap_results$original_em_results_list[1], linetype = "dashed", color = "black")  +
  theme(text=element_text(family="Ariel", face="bold", size=32),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_fixed()
