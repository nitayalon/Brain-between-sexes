plot_scatter_of_two_features <- function(data, col1, col2) {
  inner_join(biobank_standardized_data[[col1]],
             biobank_standardized_data[[col2]],
             by = 'eid') %>%
    select(col1 == value.x,
           col2 == value.y,
           sex == sex.x) %>% 
    mutate(sex = factor(sex)) %>% 
    ggplot(aes(col1, col2,
               color = sex)) + 
    geom_point(shape=1, size = 1) + 
    coord_fixed() 
}


plot_scatter_of_two_features(biobank_standardized_data,volume_features_for_plot[i],
                             volume_features_for_plot[j])
