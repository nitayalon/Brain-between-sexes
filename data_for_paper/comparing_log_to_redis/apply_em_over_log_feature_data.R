library(effsize)
library(dplyr)
library(tidyverse)

log_feature_em = lapply(log_feature_data_for_em, function(x){applyHypothesisOverResiduals(x$data_for_em, including_equal_mixture_model = F)})
save(log_feature_em, file = 'log_feature_em_results.RData')

log_feature_fdr_analysis = fullFdrAnalysis(log_feature_em)
write.csv(log_feature_fdr_analysis, file = 'log_feature_FDR.csv')

table(log_feature_fdr_analysis$bh_pvalue < 0.01)
table(log_feature_fdr_analysis$bh_pvalue < 0.05)
table(log_feature_fdr_analysis$bh_pvalue < 0.1)

ggplot(log_feature_fdr_analysis[log_feature_fdr_analysis$bins_for_fdr %in% levels(log_feature_fdr_analysis$bins_for_fdr)[c(1,2)],], aes(p,q, colour = Cohen_D)) +
  geom_point() +
  # geom_point(aes(shape = bins_for_fdr)) +
  scale_shape_manual(values=c(15, 17))+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  ggtitle("Values of p and q", subtitle = "Shape indicate FDR pv") + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() + 
  scale_color_gradient2(midpoint=-0.2, 
                        low="tomato", 
                        mid="yellow",
                        high="dodgerblue", 
                        space ="Lab" )

