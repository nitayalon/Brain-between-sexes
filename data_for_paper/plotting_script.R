#' TODO(nitay) export features names to Daphy
#' TODO(nitay) export raw data to Isaco

jpeg('residual_data_corrplot_all_features_alphabet.jpg')
corrplot(combined_correaltion_residuals, title="Residual data - all", order = "alphabet",
         tl.cex = 0.1,tl.srt = 45)
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals, order = "alphabet"),
      file = "residual_data_all_feature_order.txt")

jpeg('residual_data_corrplot_FA_features_alphabet.jpg')
corrplot(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], method="color", title="Residual data - FA",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], order = "alphabet"),
      file = "residual_data_FA_feature_order.txt")

jpeg('residual_data_corrplot_MD_features_alphabet.jpg')
corrplot(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], method="color", title="Residual data - MD",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], order = "alphabet"),
      file = "residual_data_MD_feature_order.txt")

jpeg('residual_data_corrplot_Volume_features_alphabet.jpg')
corrplot(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], method="color", title="Residual data - Volume",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], order = "alphabet"),
      file = "residual_data_volume_feature_order.txt")
##################################### PCA ##########################################################################################3
jpeg('residual_data_corrplot_all_features_FPC.jpg')
corrplot(combined_correaltion_residuals, title="Residual data - all", order = "FPC",
         tl.cex = 0.1,tl.srt = 45)
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals, order = "FPC"),
      file = "residual_data_all_feature_order.txt")

jpeg('residual_data_corrplot_FA_features_FPC.jpg')
corrplot(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], method="color", title="Residual data - FA",
         tl.cex = 0.1,tl.srt = 45, order = "FPC")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[FA_features_residuals,FA_features_residuals], order = "FPC"),
      file = "residual_data_FA_feature_order.txt")

jpeg('residual_data_corrplot_MD_features_FPC.jpg')
corrplot(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], method="color", title="Residual data - MD",
         tl.cex = 0.1,tl.srt = 45, order = "FPC")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[MD_features_residuals,MD_features_residuals], order = "FPC"),
      file = "residual_data_MD_feature_order.txt")

jpeg('residual_data_corrplot_Volume_features_FPC.jpg')
corrplot(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], method="color", title="Residual data - Volume",
         tl.cex = 0.1,tl.srt = 45, order = "FPC")  
dev.off()
write(x = corrMatOrder(combined_correaltion_residuals[Volume_features_residuals,Volume_features_residuals], order = "FPC"),
      file = "residual_data_volume_feature_order.txt")

# Log volume plots --------------------------------------------------------

jpeg('standard_data_corrplot_all_features_alphabet.jpg')
corrplot(combined_correaltion_standard, title="standard data - all", order = "alphabet",
         tl.cex = 0.1,tl.srt = 45)
dev.off()
write(x = corrMatOrder(combined_correaltion_standard, order = "alphabet"),
      file = "standard_data_all_feature_order.txt")

jpeg('standard_data_corrplot_FA_features_alphabet.jpg')
corrplot(combined_correaltion_standard[FA_features_standard,FA_features_standard], method="color", title="Standard data - FA",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_standard[FA_features_standard,FA_features_standard], order = "alphabet"),
      file = "Standard_data_FA_feature_order.txt")

jpeg('Standard_data_corrplot_MD_features_alphabet.jpg')
corrplot(combined_correaltion_standard[MD_features_standard,MD_features_standard], method="color", title="Standard data - MD",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_standard[MD_features_standard,MD_features_standard], order = "alphabet"),
      file = "Standard_data_MD_feature_order.txt")

jpeg('Standard_data_corrplot_Volume_features_alphabet.jpg')
corrplot(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], method="color", title="Standard data - Volume",
         tl.cex = 0.1,tl.srt = 45, order = "alphabet")  
dev.off()
write(x = corrMatOrder(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], order = "alphabet"),
      file = "Standard_data_volume_feature_order.txt")
################################## FPC ############################################
jpeg('standard_data_corrplot_all_features_FPC.jpg')
corrplot(combined_correaltion_standard, title="standard data - all", order = "FPC",
         tl.cex = 0.1,tl.srt = 45)
dev.off()
write(x = corrMatOrder(combined_correaltion_standard, order = "FPC"),
      file = "standard_data_all_feature_order.txt")

jpeg('standard_data_corrplot_FA_features_FPC.jpg')
corrplot(combined_correaltion_standard[FA_features_standard,FA_features_standard], method="color", title="Standard data - FA",
         tl.cex = 0.1,tl.srt = 45, order = "FPC")  
dev.off()
write(x = corrMatOrder(combined_correaltion_standard[FA_features_standard,FA_features_standard], order = "FPC"),
      file = "Standard_data_FA_feature_order.txt")

jpeg('Standard_data_corrplot_MD_features_FPC.jpg')
corrplot(combined_correaltion_standard[MD_features_standard,MD_features_standard], method="color", title="Standard data - MD",
         tl.cex = 0.1,tl.srt = 45, order = "FPC")  
dev.off()
write(x = corrMatOrder(combined_correaltion_standard[MD_features_standard,MD_features_standard], order = "FPC"),
      file = "Standard_data_MD_feature_order.txt")

jpeg('residuals_pure_type_volume_features.jpg')
corrplot(pure_type_combined_correaltion_residuals, order = "alphabet",
         tl.cex = 0.5,tl.srt = 45)
dev.off()

write(x = corrMatOrder(combined_correaltion_standard[Volume_features_standard,Volume_features_standard], order = "FPC"),
      file = "Standard_data_volume_feature_order.txt")


# Centers -----------------------------------------------------------------

jpeg('log_residual_centers.jpg')
ggplot(combined_df_long_sorted_residuals_with_means[combined_df_long_sorted_residuals_with_means$bins_for_fdr %in% levels(combined_df_long_sorted_residuals_with_means$bins_for_fdr)[c(1,2,3)],], aes(V1 ,V2, colour = region)) +
  geom_point(aes(shape = bins_for_fdr)) +
  scale_shape_manual(values=c(3, 6))+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ggtitle("Distance between empirical and theretical centers", subtitle = "Shape indicate FDR pv") + 
  xlab("Men - Women") + 
  ylab("Masculine - Feminine")
dev.off()

jpeg('log_volume_centers.jpg')
ggplot(combined_df_long_sorted_standard_with_means[combined_df_long_sorted_standard_with_means$bins_for_fdr %in% levels(combined_df_long_sorted_standard_with_means$bins_for_fdr)[c(1,2,3)],], aes(V1 ,V2, colour = region)) +
  geom_point(aes(shape = bins_for_fdr)) +
  scale_shape_manual(values=c(3, 6, 18))+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ggtitle("Distance between empirical and theretical centers", subtitle = "Shape indicate FDR pv") + 
  xlab("Men - Women") + 
  ylab("Masculine - Feminine")
dev.off()

jpeg('p_vs_q_residuals_plot_color_indicates_cohen_d_shape_indicates_p_equal_q_or_not.jpeg')
ggplot(combined_df_resid_equal_probs_sorted, aes(p,q, colour = Cohen_D)) +
  geom_point(aes(shape = p_equal_q)) +
  scale_shape_manual(values=c(3, 19))+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  ggtitle("Values of p and q", subtitle = "shape is p=q hypothesis") + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_gradient2(midpoint=0, 
                        low="darkblue", 
                        mid="yellow",
                        high="red", space ="Lab" ) +
  coord_fixed()
dev.off()

# Mixture model example

i <- which(mosaic_mixture_model_features$feature_name == 'Volume of grey matter in Lingual Gyrus (left)')
jpeg(sprintf('histogram of %s log volume.jpeg', mosaic_mixture_model_features$feature_name[i]))
plotGenderHistogram(biobank_standardized_data[[mosaic_mixture_model_features$feature_name[i]]],
                    biobank_feature_standard_analysis[[mosaic_mixture_model_features$feature_name[i]]], mosaic_mixture_model_features$feature_name[i])

dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', mosaic_mixture_model_features$feature_name[i]))
plotGenderHistogram(biobank_residuals_data[[mosaic_mixture_model_features$feature_name[i]]],
                    biobank_feature_residual_analysis[[mosaic_mixture_model_features$feature_name[i]]], mosaic_mixture_model_features$feature_name[i])
dev.off()

mosaic_mixture_model_features <- combined_df_resid_equal_probs_sorted[!combined_df_resid_equal_probs_sorted$p_equal_q,]
write.csv(mosaic_mixture_model_features, "non_equal_mixture_model_features_residuals.csv")

# Equal proportions
i <- which(single_mixture_model_volume_features == 'Volume of grey matter in Vermis VIIIa Cerebellum')
jpeg(sprintf('histogram of %s log volume.jpeg', single_mixture_model_features$feature_name[i]))
plotGenderHistogram(biobank_standardized_data[[single_mixture_model_features$feature_name[i]]],
                    biobank_feature_standard_analysis[[single_mixture_model_features$feature_name[i]]], single_mixture_model_features$feature_name[i])
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', single_mixture_model_features$feature_name[i]))
plotGenderHistogram(biobank_residuals_data[[single_mixture_model_features$feature_name[i]]],
                    biobank_feature_residual_analysis[[single_mixture_model_features$feature_name[i]]], single_mixture_model_features$feature_name[i])
dev.off()

# Pure types
i <- which(rownames(pure_types_residual_model_residual_table) == "Volume of grey matter in Postcentral Gyrus (left)")

jpeg(sprintf('histogram of %s log volume.jpeg', rownames(pure_types_residual_model_residual_table)[i]))
plotGenderHistogram(biobank_standardized_data[[rownames(pure_types_residual_model_residual_table)[i]]],
                    biobank_feature_standard_analysis[[rownames(pure_types_residual_model_residual_table)[i]]], rownames(pure_types_residual_model_residual_table)[i])
dev.off()
jpeg(sprintf('histogram of %s residuals.jpeg', rownames(pure_types_residual_model_residual_table)[i]))
plotGenderHistogram(biobank_residuals_data[[rownames(pure_types_residual_model_residual_table)[i]]],
                    biobank_feature_residual_analysis[[rownames(pure_types_residual_model_residual_table)[i]]], rownames(pure_types_residual_model_residual_table)[i])
dev.off()


write.csv(pure_types_residual_data_with_t_test, file = "~/Human_brain_research/data_for_paper/pure_types_features_residual_data.csv", row.names = T)

i <- 1
j <- 2
jpeg(sprintf('scatter_plot_of_%s_vs_%s_volume.jpeg',volume_features_for_plot[i],
             volume_features_for_plot[j]))
inner_join(biobank_standardized_data[[volume_features_for_plot[i]]],
           biobank_standardized_data[[volume_features_for_plot[j]]],
           by = 'eid') %>% 
  select(`Frontal Pole (left)` = value.x,
         `Planum Polare (right)` = value.y,
         sex = sex.x) %>% 
  mutate(sex = factor(sex)) %>% 
  ggplot(aes(`Frontal Pole (left)`, `Planum Polare (right)`,
             color = sex)) + 
  geom_point(shape=1, size = 1) + 
  coord_fixed() 
dev.off()

jpeg(sprintf('scatter_plot_of_%s_vs_%s_residuals.jpeg',volume_features_for_plot[i],volume_features_for_plot[j]))
ggplot(scatter_plot_two_probabilities, aes(responsebility.x, responsebility.y, col = factor(sex))) + 
  geom_point(shape=1, size = 1) + 
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1))  
dev.off()