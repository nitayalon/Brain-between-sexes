runGridSearchForLogNormalMixtureModel <- function(file_name_for_testing,apply_log = F)
{
  proc_data <- brainDataPreparation(file_name_for_testing)
  final_res <- logNormalMixtureModelComputationOverRealData(proc_data,F,apply_log)
  return(final_res)
}


