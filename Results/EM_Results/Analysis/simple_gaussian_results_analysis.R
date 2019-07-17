dat <- read.csv("~/MastersDegree/Thesis/DAPHNA_JOEL/Results/EM_Results/full_data_results.csv")
library(ggplot2)
p1 <- ggplot(dat, aes(llk_ratio)) + geom_density()
p1
p2 <- ggplot(dat, aes(llk_ratio)) + geom_histogram()
p2
#### extreme pvalue 
pValue <- 0.05
interesting <- dat[1 - pexp(dat$llk_ratio,1) < pValue,]
interesting <- dat[dat$llk_ratio > 2,]


#####
GSP.volume.data <- read.csv("~/MastersDegree/Thesis/DAPHNA_JOEL/Data/data_GSP_volume.csv")
GSP.interesting.features <- GSP.volume.data[names(GSP.volume.data)
                                            %in% 
                                              interesting$feature_list[
                                                interesting$file_name == "data_GSP_volume.csv"]]

GSP.interesting.features <- cbind(Gender = GSP.volume.data$Gender, GSP.interesting.features)
names(GSP.interesting.features)
p3 <- ggplot(GSP.interesting.features, aes(X.41, colour = Gender)) + geom_density()
p3 <- ggplot(GSP.interesting.features, aes(X.41)) + geom_density() + facet_wrap(~Gender)
interesting[interesting$file_name == "data_GSP_volume.csv",]


