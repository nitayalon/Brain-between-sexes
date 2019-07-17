# Main EM computation for brain features
mainFunctionForGenderMixtureModel <- function(all_data = F,export_plots = F, 
                                              file_number = 1,
                                                   ...)
{
  # Get the data
  target_dir <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Data/Full_Data/Unified_Data/"
  
  # Select only the GSP files
  GSP_files <- list.files(target_dir)[
    list.files(target_dir) %>% 
      grep("GSP",.)]
  
  # Set results directory
  results_directory <- 
    "~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/Log_normal_mixture_model/EM_Results/"
  
  if(all_data)
  {
    print(NULL)
  }
  else
  {
    file_name_for_testing <- 
      tryCatch(
        {
          GSP_files[file_number]
        },
        error=function(cond)
        {
          message("Cannot read file in location")
          message(file_number)
          message("returning defualt file")
          return(GSP_files[1])
        }
      )
  }
  
  # Create brain file class for the seleted file
  brain_data <- new("brainFile", 
                    name = file_name_for_testing,
                    target_dir = "~/mastersdegree/Thesis/DAPHNA_JOEL/Data/Full_Data/Unified_Data/")
  
  grid_search_results <- runEMForLogNormalMixtureModel(brain_data, results_directory)
  results_file_name <- paste(strsplit(file_name_for_testing,"[.]")[[1]][1],sep= "_")
  save(grid_search_results,file = paste(results_directory,results_file_name,sep = ""))
  if(export_plots)
  {
    create3Dplots(grid_search_results$full_data_results$grid_search_results,c("theta_mas","theta_fem"))
  }
}
