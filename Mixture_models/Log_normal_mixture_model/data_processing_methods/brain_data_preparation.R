# This function prepars the data by adding gender data (if missing) and removing missing data
brainDataPreparation <- function(file_name)
{
  dat <- addGenderData(file_name)
  clean_data <- removeMissingData(dat)
  return(clean_data)
}