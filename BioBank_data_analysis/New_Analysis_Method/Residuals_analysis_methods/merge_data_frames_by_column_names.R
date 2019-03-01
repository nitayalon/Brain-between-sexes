#' Join two data frames by column names
#' @param df1 data frame with column names
#' @param df2 data frame with column names
#' @return df3 data.frame with same names as df1 and nrow = nrow(df1) + nrow(df2)
mergeDataFramesByColumnNames <- function(df1, df2)
{
  stopifnot(names(df1) == names(df2))
  total_rows <- nrow(df1) + nrow(df2)
  ncols = length(names(df1))
  place_holder <- matrix(rep(NA, total_rows * ncols), nrow = total_rows, ncol = ncols) %>% 
    data.frame()
  names(place_holder) <- names(df1)
  for(col_name in names(place_holder))
  {
    place_holder[,col_name] <- c(df1[,col_name], df2[,col_name])
  }
  return(place_holder)
}
