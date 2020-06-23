all_data = rbind(men_responsebilities_matrix, women_responsebilities_matrix)
dim(all_data)
all_data_correlation = cor(all_data[,-1])
all_data_correlation_eigen = eigen(all_data_correlation)
plot(all_data_correlation_eigen$values)
abline(h = 1, col = 'red')
sum(all_data_correlation_eigen$values > 1)
first_comp = as.matrix(all_data[,-1]) %*% all_data_correlation_eigen$vectors[,1]
second_comp = as.matrix(all_data[,-1]) %*% all_data_correlation_eigen$vectors[,2]
third_comp = as.matrix(all_data[,-1]) %*% all_data_correlation_eigen$vectors[,3]
mean(first_comp[1:7365])
sd(first_comp[1:7365])
mean(first_comp[7366:length(first_comp)])
sd(first_comp[7366:length(first_comp)])

vec = c()

for(i in 1:20){
  first_comp = as.matrix(all_data[,-1]) %*% all_data_correlation_eigen$vectors[,i]
  print(sprintf('Feature number %s',i))
  num = mean(first_comp[1:7365]) - mean(first_comp[7366:length(first_comp)])
  denom =  sqrt(mean(var(first_comp[1:7365]), var(first_comp[7366:length(first_comp)])))
  vec[i] = (num / denom)
}

plot(vec, cex.axis = 1.5, cex = 1.5)
sencod_pca_features = data.frame(lower = names(all_data)[-1][which(order(all_data_correlation_eigen$vectors[,2]) %in% seq(1,10))]
,upper = names(all_data)[-1][which(order(all_data_correlation_eigen$vectors[,2]) %in% seq(length(all_data_correlation_eigen$vectors[,2]) - 9,
                                                                                 length(all_data_correlation_eigen$vectors[,2])))])
write.csv(sencod_pca_features, file = '../data_for_paper/Tables/pca_second_component.csv')

which(sencod_pca_features$lower %in% q2_q4_features_names)
which(sencod_pca_features$lower %in% q1_q3_features_names)
which(sencod_pca_features$upper %in% q2_q4_features_names)
which(sencod_pca_features$upper %in% q1_q3_features_names)
sencod_pca_features$upper[which(sencod_pca_features$upper %in% p_equals_q$feature)]

men_projection_matrix = cbind(first_comp[1:7365],second_comp[1:7365],third_comp[1:7365])
cor(men_projection_matrix)
sd(men_projection_matrix[,1])
sd(men_projection_matrix[,2])
sd(men_projection_matrix[,3])
women_projection_matrix = cbind(first_comp[7366:length(first_comp)],second_comp[7366:length(first_comp)],third_comp[7366:length(first_comp)])
cor(women_projection_matrix)
sd(women_projection_matrix[,1])
sd(women_projection_matrix[,2])
sd(women_projection_matrix[,3])


plot(sort(all_data_correlation_eigen$vectors[,2]))
pca_all_data = prcomp(all_data_correlation)
summary(pca_all_data)
str(pca_all_data)
