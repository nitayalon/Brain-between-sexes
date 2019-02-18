linux.path <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Grid_search/Methods/"
windows.path <- "~/MastersDegree/Thesis/DAPHNA_JOEL/Grid_search/Methods/"
file.names <- c("compute_llk_under_null_hypothesis.R", "compute_mle_for_feature.R",
                "compute_parameters_for_features.R", "data_preparation_methods.R",
                "find_mle_from_grid_search_results.R", "grid_search_methods.R",
                "validate_grid_search_results.R", "EDA_of_mock_data.R")

for(file in file.names){
  source(file.path(linux.path, file)) 
}

linux.path <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Grid_search/MockData/"
file.names <- c("sample_mock_data.R","compute_MLE_for_mock_data.R",
                "compute_MLE_and_llkr_ratio_for_mock_data.R",
                "normalizing_data.R", "compute_loglikelihood_over_grid_mock_data.R")
for(file in file.names){
  source(file.path(linux.path, file)) 
}

linux.path <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Grid_search/MockData/Simulations/Using_KLD_to_estimate_power/"
file.names <- c("simple_KLD.R")
for(file in file.names){
  source(file.path(linux.path, file)) 
}

source("~/mastersdegree/Thesis/DAPHNA_JOEL/Grid_search/grid_search_for_two_theta.R")
