## This function plots a histogram of a feature
library(ggplot2)
histogramIfFeatureData <- function(file_name,with_log = T)
{
  results_directory <- 
    "~/mastersdegree/Thesis/DAPHNA_JOEL/Results/EM_Results/EM_mixture_model_new/"
  table_name <- paste(results_directory,paste0(file_name,".csv"),sep = "")
  brain_file <- read.csv(table_name, header = T)
  feature_data_hist <- ggplot(brain_file, 
                              aes(standard_data.val, fill = factor(standard_data.sex))) + 
    geom_histogram(binwidth = 0.1) + 
    ggtitle(paste0("Histogram of ",file_name),
            subtitle = paste0("log transformation = ",as.character(with_log)))
  return(feature_data_hist)
}