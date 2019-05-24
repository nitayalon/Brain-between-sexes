#' discretize the probablities
ConvertProbabilityToBool <- function(x)
{
  disc <- sapply(x, function(y){ifelse(y > 0.5, 1, -1)})
  disc
}

#' Create 2*2 table:
ComputeTwoByTwoTable <- function(x,y)
{
  tbl <- table(x,y)
  if(dim(tbl)[1] == 2 && dim(tbl)[2] == 2)
  {
   if(all(tbl > 5))
   {
     return(list(data = tbl,
                 valid = TRUE))
   }
    else
   {
     return(list(data = tbl,
                 valid = FALSE)) 
   }
  }
   return(NULL)
}

ComputeChiSquareSign <- function(two_by_two_table)
{
  return(two_by_two_table[1,1] * two_by_two_table[2,2] - 
         two_by_two_table[1,2] * two_by_two_table[2,1])
}
