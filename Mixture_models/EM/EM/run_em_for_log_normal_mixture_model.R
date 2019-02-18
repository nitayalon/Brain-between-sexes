runEMForLogNormalMixtureModel <- function(brain_file_class,where_to_save = NULL)
{
  stopifnot(class(brain_file_class) == "brainFile")
  brain_file_data_with_dict <- loadBrainDictionaryFile(brain_file_class)
  brain_file_data_with_data <- loadBrainDataFile(brain_file_data_with_dict)
  brain_class_file_name <- getBrainFileName(brain_file_data_with_data)
  save(brain_file_data_with_data,file = paste(where_to_save,
                               paste0(strsplit(brain_class_file_name,"[.]")[[1]][1],
                               "Brain_file_class",sep = "_"),sep = ""))
  final_res <- EMForGenderMixtureModel(brain_file_data_with_data)
  return(final_res)
}


