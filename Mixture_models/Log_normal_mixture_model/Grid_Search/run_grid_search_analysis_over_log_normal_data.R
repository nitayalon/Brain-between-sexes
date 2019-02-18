runGridSearchAnalysisOverLogNormalData <- function(all_data = F,export_plots = F,apply_log = F,
                                                   ...)
{
  # Get the data
  target_dir <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Data/Full_Data/Unified_Data/"
  
  # Gender data
  dictionary <- read.csv(paste0(target_dir,"/DataRelease_2014-04-22.csv"),
                         header = T, stringsAsFactors = F)
  names(dictionary)[names(dictionary) %>% grep("Sex",.)]
  
  # Select only the GSP files
  GSP_files <- list.files(target_dir)[
    list.files(target_dir) %>% 
      grep("GSP",.)]
  
  # Prepare the data
  source("../data_processing_methods/remove_list.R")
  
  # Set results directory
  results_directory <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/Log_normal_mixture_model/Grid_Search/grid_search_results/"
  
  if(all_data)
  {
    print(NULL)
  }
  else
  {
    file_name_for_testing <- GSP_files[1]
  }
  grid_search_results <- runGridSearchForLogNormalMixtureModel(file_name_for_testing,apply_log)
  results_file_name <- paste(strsplit(file_name_for_testing,"[.]")[[1]][1],as.character(apply_log),sep= "_")
  save(grid_search_results,file = paste(results_directory,results_file_name,sep = ""))
  if(export_plots)
  {
    create3Dplots(grid_search_results$full_data_results$grid_search_results,c("theta_mas","theta_fem"))
  }
}
