#' TODO(nitay) export features names to Daphy
#' TODO(nitay) export raw data to Isaco

# jpeg('residual_data_corrplot_all_features_alphabet.jpg')
# corrplot(combined_correaltion_residuals, title="Residual data - all", order = "alphabet",
#          tl.cex = 0.1,tl.srt = 45)
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals, order = "alphabet"),
#       file = "residual_data_all_feature_order.txt")
# 
# jpeg('residual_data_corrplot_FA_features_alphabet.jpg')
# corrplot(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], method="color", title="Residual data - FA",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], order = "alphabet"),
#       file = "residual_data_FA_feature_order.txt")
# 
# jpeg('residual_data_corrplot_MD_features_alphabet.jpg')
# corrplot(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], method="color", title="Residual data - MD",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], order = "alphabet"),
#       file = "residual_data_MD_feature_order.txt")
# 
# jpeg('residual_data_corrplot_Volume_features_alphabet.jpg')
# corrplot(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], method="color", title="Residual data - Volume",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], order = "alphabet"),
#       file = "residual_data_volume_feature_order.txt")
# ##################################### PCA ##########################################################################################3
# jpeg('residual_data_corrplot_all_features_FPC.jpg')
# corrplot(combined_correaltion_residuals, title="Residual data - all", order = "FPC",
#          tl.cex = 0.1,tl.srt = 45)
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals, order = "FPC"),
#       file = "residual_data_all_feature_order.txt")
# 
# jpeg('residual_data_corrplot_FA_features_FPC.jpg')
# corrplot(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], method="color", title="Residual data - FA",
#          tl.cex = 0.1,tl.srt = 45, order = "FPC")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], order = "FPC"),
#       file = "residual_data_FA_feature_order.txt")
# 
# jpeg('residual_data_corrplot_MD_features_FPC.jpg')
# corrplot(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], method="color", title="Residual data - MD",
#          tl.cex = 0.1,tl.srt = 45, order = "FPC")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], order = "FPC"),
#       file = "residual_data_MD_feature_order.txt")
# 
# jpeg('residual_data_corrplot_Volume_features_FPC.jpg')
# corrplot(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], method="color", title="Residual data - Volume",
#          tl.cex = 0.1,tl.srt = 45, order = "FPC")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], order = "FPC"),
#       file = "residual_data_volume_feature_order.txt")
# 
# # Log volume plots --------------------------------------------------------
# 
# jpeg('standard_data_corrplot_all_features_alphabet.jpg')
# corrplot(combined_correaltion_standard, title="standard data - all", order = "alphabet",
#          tl.cex = 0.1,tl.srt = 45)
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard, order = "alphabet"),
#       file = "standard_data_all_feature_order.txt")
# 
# jpeg('standard_data_corrplot_FA_features_alphabet.jpg')
# corrplot(combined_correaltion_standard[FA_features_standard,FA_features_standard], method="color", title="Standard data - FA",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard[FA_features_standard,FA_features_standard], order = "alphabet"),
#       file = "Standard_data_FA_feature_order.txt")
# 
# jpeg('Standard_data_corrplot_MD_features_alphabet.jpg')
# corrplot(combined_correaltion_standard[MD_features_standard,MD_features_standard], method="color", title="Standard data - MD",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard[MD_features_standard,MD_features_standard], order = "alphabet"),
#       file = "Standard_data_MD_feature_order.txt")
# 
# jpeg('Standard_data_corrplot_Volume_features_alphabet.jpg')
# corrplot(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], method="color", title="Standard data - Volume",
#          tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], order = "alphabet"),
#       file = "Standard_data_volume_feature_order.txt")
# ################################## FPC ############################################
# jpeg('standard_data_corrplot_all_features_FPC.jpg')
# corrplot(combined_correaltion_standard, title="standard data - all", order = "FPC",
#          tl.cex = 0.1,tl.srt = 45)
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard, order = "FPC"),
#       file = "standard_data_all_feature_order.txt")
# 
# jpeg('standard_data_corrplot_FA_features_FPC.jpg')
# corrplot(combined_correaltion_standard[FA_features_standard,FA_features_standard], method="color", title="Standard data - FA",
#          tl.cex = 0.1,tl.srt = 45, order = "FPC")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard[FA_features_standard,FA_features_standard], order = "FPC"),
#       file = "Standard_data_FA_feature_order.txt")
# 
# jpeg('Standard_data_corrplot_MD_features_FPC.jpg')
# corrplot(combined_correaltion_standard[MD_features_standard,MD_features_standard], method="color", title="Standard data - MD",
#          tl.cex = 0.1,tl.srt = 45, order = "FPC")  
# dev.off()
# write(x = corrMatOrder(combined_correaltion_standard[MD_features_standard,MD_features_standard], order = "FPC"),
#       file = "Standard_data_MD_feature_order.txt")
# 
# jpeg('residuals_pure_type_volume_features.jpg')
# corrplot(pure_type_combined_correaltion_residuals, order = "alphabet",
#          tl.cex = 0.5,tl.srt = 45)
# dev.off()
# 
# write(x = corrMatOrder(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], order = "FPC"),
#       file = "Standard_data_volume_feature_order.txt")
# 
# 
# # Centers -----------------------------------------------------------------
# 
# jpeg('log_residual_centers.jpg')
# ggplot(combined_df_long_sorted_residuals_with_means[combined_df_long_sorted_residuals_with_means$bins_for_fdr %in% levels(combined_df_long_sorted_residuals_with_means$bins_for_fdr)[c(1,2,3)],], aes(V1 ,V2, colour = region)) +
#   geom_point(aes(shape = bins_for_fdr)) +
#   scale_shape_manual(values=c(3, 6))+
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   ggtitle("Distance between empirical and theretical centers", subtitle = "Shape indicate FDR pv") + 
#   xlab("Men - Women") + 
#   ylab("Masculine - Feminine")
# dev.off()
# 
# jpeg('log_volume_centers.jpg')
# ggplot(combined_df_long_sorted_standard_with_means[combined_df_long_sorted_standard_with_means$bins_for_fdr %in% levels(combined_df_long_sorted_standard_with_means$bins_for_fdr)[c(1,2,3)],], aes(V1 ,V2, colour = region)) +
#   geom_point(aes(shape = bins_for_fdr)) +
#   scale_shape_manual(values=c(3, 6, 18))+
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   ggtitle("Distance between empirical and theretical centers", subtitle = "Shape indicate FDR pv") + 
#   xlab("Men - Women") + 
#   ylab("Masculine - Feminine")
# dev.off()
# 
# 
# # Final plots for paper --------------------------------------------------
# 
# ## Write csv's
# write.csv(pure_types_residual_model_residual_table, "pure_types_features_residual_data.csv")
# write.csv(single_model_mosaic_mixture_model_features, "equal_mixture_model_features_residuals.csv")
# write.csv(mosaic_mixture_model_features, "non_equal_mixture_model_features_residuals.csv")

jpeg('p_vs_q_residuals_plot_color_indicates_cohen_d_shape_indicates_p_equal_q_or_not.jpeg')
ggplot(df_p_vs_q, aes(p,q)) +
  geom_point(aes(colour = bins_for_cohen_d,shape = p_equal_q)) +
  scale_shape_manual(values=c(19, 3))+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  ggtitle("Values of p and q", subtitle = "shape is p=q hypothesis") + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_manual(name = "Cohen D",
                     values = c("(-0.3,-0.2]" = "red4",
                                "(-0.2,-0.1]" = "red1",
                                "(-0.1,0.1]" = "orange1",
                                "(0.1,0.2]" = "darkslategray1",
                                "(0.2,0.3]" = "dodgerblue1",
                                "(0.3,0.4]" = "blue4"),
                     labels = c("<-0.2","0.2<=d<-0.1","-0.1<=d<0.1","0.1<=d<0.2","0.2<=d<3","0.3<=d<0.4")) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=7),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=7)) +
  coord_fixed()
dev.off()

# Mixture model example
mixture_model_feature_name <- 'Volume of grey matter in Lingual Gyrus (left)'
jpeg(sprintf('histogram of %s log volume.jpeg', mixture_model_feature_name))
plotGenderHistogram(biobank_standardized_data[[mixture_model_feature_name]],
                    biobank_feature_standard_analysis[[mixture_model_feature_name]],
mixture_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', mixture_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[mixture_model_feature_name]],
                    biobank_feature_residual_analysis[[mixture_model_feature_name]], mixture_model_feature_name)
dev.off()

# Equal proportions
single_mixture_model_feature_name <- 'Volume of grey matter in Occipital Fusiform Gyrus (left)'
jpeg(sprintf('histogram of %s log volume.jpeg', single_mixture_model_feature_name))
plotGenderHistogram(biobank_standardized_data[[single_mixture_model_feature_name]],
                    biobank_feature_standard_analysis[[single_mixture_model_feature_name]],
                    single_mixture_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', single_mixture_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[single_mixture_model_feature_name]],
                    biobank_feature_residual_analysis[[single_mixture_model_feature_name]], single_mixture_model_feature_name)
dev.off()

# Pure types
# Plot only the underline puretype
pure_type_model_feature_name <- 'Volume of grey matter in Postcentral Gyrus (left)'
jpeg(sprintf('histogram of %s log volume.jpeg', pure_type_model_feature_name))
plotGenderHistogram(biobank_standardized_data[[pure_type_model_feature_name]],
                    biobank_feature_standard_analysis[[pure_type_model_feature_name]],
                    pure_type_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', pure_type_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[pure_type_model_feature_name]],
                    biobank_feature_residual_analysis[[pure_type_model_feature_name]], pure_type_model_feature_name)
dev.off()

#### Scatter plots
i <- 2
j <- 5
x_feature <- strsplit(volume_features_for_plot[i], split="in")[[1]][2] %>% trimws()
y_feature <- strsplit(volume_features_for_plot[j], split="in")[[1]][2] %>% trimws()
jpeg(sprintf('scatter_plot_of_%s_vs_%s_volume.jpeg',volume_features_for_plot[i],
             volume_features_for_plot[j]))
  inner_join(biobank_standardized_data[[volume_features_for_plot[i]]],
           biobank_standardized_data[[volume_features_for_plot[j]]],
           by = 'eid') %>% 
  select(`Planum Polare (right)` = value.x,
         `Insular Cortex (right)` = value.y,
         sex = sex.x) %>% 
  mutate(sex = factor(sex)) %>% 
  ggplot(aes(`Planum Polare (right)`, `Insular Cortex (right)`,
             color = sex)) + 
  geom_point(shape=20, size = 1) + 
  labs(title = sprintf('%s vs %s',
                       x_feature, y_feature
                       ),
         subtitle = "Log volume",
         caption = "Measure:Volume of gery matter") + 
  coord_fixed() 
dev.off()

jpeg(sprintf('scatter_plot_of_%s_vs_%s_residuals.jpeg',volume_features_for_plot[i],volume_features_for_plot[j]))
  inner_join(biobank_residuals_data[[volume_features_for_plot[i]]],
           biobank_residuals_data[[volume_features_for_plot[j]]],
           by = 'eid') %>% 
    select(`Planum Polare (right)` = value.x,
           `Insular Cortex (right)` = value.y,
           sex = sex.x) %>% 
    mutate(sex = factor(sex)) %>% 
    ggplot(aes(`Planum Polare (right)`, `Insular Cortex (right)`,
               color = sex)) + 
    geom_point(shape=20, size = 1) + 
    labs(title = sprintf('%s vs %s',
                         x_feature, y_feature
    ),
    subtitle = "Residuals",
    caption = "Measure:Volume of gery matter") + 
  coord_fixed() 
dev.off()


# Corrplots ---------------------------------------------------------------

## Pure types
write.csv(pure_type_correlation_plot_legend, "pure_type_correlation_plot_legend.csv")
jpeg('pure_types_probability_correlation_matrix.jpeg')
corrplot(pure_type_combined_correaltion_residuals, order = "alphabet", 
         method = 'circle',tl.cex = 1.0,tl.srt = 45,tl.col="black")
dev.off()

## Single mixture model
write.csv(single_mixture_model_corrplot_legend, "single_mixture_model_corrplot_legend.csv")

jpeg('single_mixture_model_probability_correlation_matrix_volume_features.jpeg')
corrplot(combined_correaltion_residuals[volume_features_single_mixture_model,volume_features_single_mixture_model], 
         order = "alphabet",tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

jpeg('single_mixture_model_probability_correlation_matrix_md_features.jpeg')
corrplot(combined_correaltion_residuals[MD_features_single_mixture_model,MD_features_single_mixture_model], order = "alphabet",
         tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

jpeg('single_mixture_model_probability_correlation_matrix_fa_features.jpeg')
corrplot(combined_correaltion_residuals[FA_features_single_mixture_model,FA_features_single_mixture_model], order = "alphabet",
         tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

## Two mixture model
write.csv(two_mixture_model_corrplot_legend, "two_mixture_model_corrplot_legend.csv")

jpeg('two_mixture_model_probability_correlation_matrix_volume_features.jpeg')
corrplot(combined_correaltion_residuals[volume_features_two_mixture_model,volume_features_two_mixture_model], 
         order = "alphabet",tl.cex = 0.6,tl.col="black")
dev.off()

jpeg('two_mixture_model_probability_correlation_matrix_md_features.jpeg')
corrplot(combined_correaltion_residuals[MD_features_two_mixture_model,MD_features_two_mixture_model], order = "alphabet",
         tl.cex = 1.0,tl.srt = 45,tl.col="black")
dev.off()

jpeg('two_mixture_model_probability_correlation_matrix_fa_features.jpeg')
corrplot(combined_correaltion_residuals[FA_features_two_mixture_model,FA_features_two_mixture_model], order = "alphabet",
         tl.cex = 0.8,tl.col="black")
dev.off()