require(gdata)
require(magrittr)

# Reading xlsx files from directory ---------------------------------------
target.dir <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Data/Full_Data/Unified_Data/"

# Gender data
dictionary <- read.csv(paste0(target.dir,"/DataRelease_2014-04-22.csv"),
                       header = T, stringsAsFactors = F)
names(dictionary)[names(dictionary) %>% grep("Sex",.)]

# Select only the GSP files
GSP.files <- list.files(target.dir)[
  list.files(target.dir) %>% 
    grep("GSP",.)]

setwd("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/Log_normal_mixture_model/EM_Results/")

mainFuncForThisSession <- function(file_name)
{
  dat <- addGenderData(file_name)
  clean_data <- removeMissingData(dat)
  log_like_results <- mainFunctionComputeLikelihoodRatioOverFile(clean_data)
  # csv_file_name <- paste0(strsplit(file_name,"[.]")[[1]][1],".csv")
  # write.csv(log_like_results$EM_results, csv_file_name)
  # csv_raw_data_name <- paste0(strsplit(file_name,"[.]")[[1]][1],"_raw_data",".csv")
  # write.csv(log_like_results$EM_Data, csv_raw_data_name)
  return(list(EM_results = log_like_results$EM_results,
              EM_Data = log_like_results$EM_Data))
}

for(file_name in GSP.files) 
{
  print(file_name)
  mainFuncForThisSession(file_name)
}
### Only for EM validation
file.name.for.testing <- GSP.files[1]
res <- mainFuncForThisSession(file.name.for.testing)
men <- res$EM_Data$mem
women <- res$EM_Data$women
MLE <- res$EM_results$.
names(MLE) <- row.names(res$EM_results)
validation.res <- validateMLE(men,women,MLE[c("theta_mas","theta_fem","p","q","sigma2")],
                              epsilon = 0.2)
validation.res
