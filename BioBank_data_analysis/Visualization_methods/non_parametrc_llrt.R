#' Nonparametric Log Likelihhod computation
nonParametricLLRT <- function(feature_name, include_sex_densities = T)
{
  if(!(feature_name %in% names(results.list)))
  {
    return(NULL)
  }
  brain_feature <- results.list[[feature_name]]
  
  grid <- seq(-4, 4, by = 0.01)
  men_density <- logConDens(brain_feature$feature_residuals$value[brain_feature$feature_residuals$sex == 0], xgrid = grid)
  women_density <- logConDens(brain_feature$feature_residuals$value[brain_feature$feature_residuals$sex == 1], xgrid = grid)
  if(include_sex_densities)
  {
    plot(log(men_density$w / women_density$w), main = feature_name)
    plot(men_density)    
    plot(women_density)    
  }
  plot(log(men_density$w / women_density$w), main = feature_name)
}