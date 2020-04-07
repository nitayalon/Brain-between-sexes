corrplot(combined_correaltion_residuals[volume_features_single_mixture_model,volume_features_single_mixture_model], 
         order = "alphabet",tl.cex = 0.5,tl.srt = 90,tl.col="black", method = 'number')

men_cor_residuals_vector <- 
  men_cor_residuals[upper.tri(men_cor_residuals)] %>% 
  as.vector()

women_cor_residuals_vector <- 
  women_cor_residuals[upper.tri(women_cor_residuals)] %>% 
  as.vector()
both_genders_residuals_correlation_vector_df = data.frame(index = seq(1,length(sort(men_cor_residuals_vector))),
                                                          men_residual_correlation = sort(men_cor_residuals_vector),
                                                          women_residual_correlation = sort(women_cor_residuals_vector))

ggplot(both_genders_residuals_correlation_vector_df, aes(x = index)) + 
  geom_line(aes(y = men_residual_correlation, color = 'men')) + 
  geom_line(aes(y = women_residual_correlation, color = 'women')) + 
  ggtitle("Men and women sorted residual correlation") + 
  xlab("Index") + 
  ylab('Correlation')

plot(sort(men_cor_residuals_vector),col = 'red', type= 'l')
lines(sort(women_cor_residuals_vector),col = 'blue')

plot(sort(men_cor_residuals_vector) - sort(women_cor_residuals_vector),col = 'red', type= 'l')

plot(sort(men_cor_residuals_vector) - lag(sort(men_cor_residuals_vector)),col = 'red', type= 'l',
     ylim = c(0,0.05))
lines(sort(women_cor_residuals_vector) - lag(sort(women_cor_residuals_vector)),col = 'blue')

