# Removing missing data for EM

removeMissingData <- function(dat)
{
  no_NA_data <- dat[complete.cases(dat),]
  zero_rows = apply(no_NA_data, 1, function(row) all(row !=0 ))
  no_zero_data <- no_NA_data[zero_rows,]
  return(no_zero_data)
}