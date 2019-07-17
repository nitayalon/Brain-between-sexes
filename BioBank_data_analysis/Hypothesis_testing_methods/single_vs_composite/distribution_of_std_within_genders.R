computingStandardDeviationWithInGroups <- function(list_of_features)
{
  n <- length(list_of_features)
  men_std <- c()
  women_std <- c()
  for(i in 1:n)
  {
    df <- data.frame(feature = full_relevant_data[list_of_features[i]],
                     "Sex" = full_relevant_data$Sex)
    names(df) = c("Feature","Sex")
    std_per_gender <- 
      df %>% 
      group_by(Sex) %>% 
      filter(!is.na(Feature)) %>% 
      summarise(std = sd(Feature))
    men_std[i] <- std_per_gender$std[1]
    women_std[i] <- std_per_gender$std[2]
  }
  return(list(men_std = men_std, women_std = women_std))
}