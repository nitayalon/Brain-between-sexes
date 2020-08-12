i = 3
feature_name = features_for_plot[i]
load(sprintf("%s/%s/%s.RData",mainDir,feature_name,feature_name))
DF <- data.frame(p = bootstrap_results$bootstrap_results[,1],
                 q = bootstrap_results$bootstrap_results[,2])
ggscatter(DF, x = "p", y = "q",
              color = 'black', palette = "jco",
              size = 0.4, alpha = 0.6) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) + 
  geom_hline(yintercept = bootstrap_results$original_em_results_list[2], linetype = "dashed", color = "black") +
  geom_vline(xintercept = bootstrap_results$original_em_results_list[1], linetype = "dashed", color = "black")  +
  theme(text = element_text(size = 20)) +
  coord_fixed()
