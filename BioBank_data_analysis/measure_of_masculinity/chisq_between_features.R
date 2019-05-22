#' discretize the probablities
ConvertProbabilityToBool <- function(x)
{
  disc <- sapply(x, function(y){ifelse(y > 0.5, 1, -1)})
  disc
}

men_feature_data <- pblapply(men_feature_data[,-1],
                                  ConvertProbabilityToBool) 

women_feature_data_disc <- pblapply(women_feature_data[,-1],
                                  ConvertProbabilityToBool) 
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

### Create valid data set:

chisq_men <- matrix(NA,length(men_feature_data_disc),length(men_feature_data_disc))
not_valid_for_chisq_list <- c()

for(i in 1:(length(men_feature_data_disc)-1))
{
  for(j in (i+1):length(men_feature_data_disc))
  {
    print(c(i,j))
    data_for_chisq <- ComputeTwoByTwoTable(men_feature_data_disc[[i]],
                                       men_feature_data_disc[[j]])
    if(!is.null(data_for_chisq$valid) && data_for_chisq$valid)
    {
      chi_square_statistic <- (chisq.test(data_for_chisq$data))$statistic
      chi_square_sign <- ComputeChiSquareSign(data_for_chisq$data)
      chisq_men[i,j] <- sqrt(chi_square_statistic) * sign(chi_square_sign)
      next
    }
    not_valid_for_chisq_list <- rbind(not_valid_for_chisq_list,
                                       c(names(men_feature_data_disc)[i],
                                         names(men_feature_data_disc)[j]))
  }
}

hist(chisq_men[upper.tri(chisq_men,diag = F)],breaks = 100)
save(x = chisq_men,
     file = "men_chisq_test.RData")

chisq_women <- matrix(NA,length(men_feature_data_disc),length(men_feature_data_disc))

for(i in 1:(length(women_feature_data_disc)-1))
{
  for(j in (i+1):length(women_feature_data_disc))
  {
    print(c(i,j))
    chisq_per_feature <- ComputeChiSquareStatistic(women_feature_data_disc[[i]],
                                       women_feature_data_disc[[j]])
    chisq_women[i,j] <- chisq_per_feature$statistic
  }
}
save(x = chisq_women,
     file = "women_chisq_test.RData")
hist(chisq_women[upper.tri(chisq_women,diag = F)], breaks = 100)

