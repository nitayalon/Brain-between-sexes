#' Nonparametric Log Likelihhod computation
nonParametricLLRT <- function(feature_name, 
                              trim_value = 6,
                              include_sex_densities = T)
{
  if(!(feature_name %in% names(results.list)))
  {
    return(NULL)
  }
  brain_feature <- results.list[[feature_name]]
  grid <- seq(-4, 6, by = 0.01)
  
  men_resid <- brain_feature$feature_residuals$value[brain_feature$feature_residuals$sex == 1]
  women_resid <- brain_feature$feature_residuals$value[brain_feature$feature_residuals$sex == 0]
  
  men_resid_trimmed <- men_resid[men_resid > (-1 * trim_value) & men_resid < trim_value]
  women_resid_trimmed <- women_resid[women_resid > (-1 * trim_value) & women_resid < trim_value]
  
  men_density <- logConDens(sort(men_resid_trimmed) ,smoothed = T, xs = grid)
  women_density <- logConDens(sort(women_resid_trimmed) ,smoothed = T, xs = grid)
  
  if(include_sex_densities)
  {
    # plot(log((men_density$phi * men_density$w) /
    #            (women_density$phi * women_density$w)), main = feature_name)
    # plot(women_density$xs, log(men_density$f.smoothed / women_density$f.smoothed), 
    #      main = feature_name, ylab = "LLRT")
    return(list(men_density = men_density,
                women_density = women_density))
  }
  xlim_ind <- c(length(women_density$x) > length(men_density$x),
                length(women_density$x) < length(men_density$x))
  xlim_df <- list(women = women_density$x,
                  men = men_density$x)
  plot(xlim_df[xlim_ind][[1]],
    log((men_density$phi * men_density$w) /
             (women_density$phi * women_density$w)), 
    main = sprintf("Log convave llrt, %s",feature_name))
}