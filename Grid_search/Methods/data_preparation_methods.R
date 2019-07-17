computeGroupData <- function(feature_data_by_gender){
  return(list(
    average = mean(feature_data_by_gender),
    var = var(feature_data_by_gender)
  ))
}

splitDataByGender <- function(data){
  men_data <- data[data[,1] == 1,]
  women_data <- data[data[,1] == 2,]
  return(list(
    men_data = men_data,
    women_data = women_data
  ))
}

