## KS Test for brain feature - BioBank data
applyKSTestForMixtureFeature <- function(feature_data,gender_data) {
  
  men_data <- feature_data[gender_data == 1]
  women_data <- feature_data[gender_data == 0]
  
  men_disribution <- applyKSTestPerFeature(men_data)
  women_disribution <- applyKSTestPerFeature(women_data)
  
  ks_test_for_gender <- list(
    men = men_disribution,
    women = women_disribution
  )
    
  return(ks_test_for_gender)
}