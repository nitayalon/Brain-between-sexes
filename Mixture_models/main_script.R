library(data.table)
library(ggplot2)
source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/source_script.R")
setwd("~/mastersdegree/Thesis/DAPHNA_JOEL/Results/EM_Results")

file.list <- c(
  "data_GSP_thickness.csv",
  "data_GSP_volume_divide_power.csv",
  "data_GSP_volume.csv",
  "data_GSP_VBM.csv")

results <- data.frame()
for(file.name in file.list){
  res <- mainFunctionGridSearch(file.name)
  results <- rbind(results, res)
}

write.csv(results,
          file.path("~/mastersdegree/Thesis/DAPHNA_JOEL/Results/EM_Results",
                      "full_data_results.csv"))

data <- read.csv("/home/nitay/mastersdegree/Thesis/DAPHNA_JOEL/Data/data_GSP_volume.csv")
mle.per.feature <- mainFunctionGridSearch("data_GSP_volume.csv")
p <- ggplot(mle.per.feature, aes(feature_list, llk_ratio)) + geom_point()
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

