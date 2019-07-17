validateMoments <- function(feature_data)
{
  third_moments <- moment(feature_data,3,center = T,na.rm = T)
  forth_moments <- moment(feature_data,4,center = T,na.rm = T)
  return(c(third_moments,forth_moments))
}
