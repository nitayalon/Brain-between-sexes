compute_number_of_outliers = function(f)
{
  f_sd = sd(f$value)
  f_mean = mean(f$value)
  counter = f$value > (f_mean + 4 * f_sd) | f$value < (f_mean - 4 * f_sd)
  return(sum(counter))
}

# names(biobank_standardized_data) == names(biobank_residuals_data)
# hist(biobank_standardized_data[[1]]$value)
log_volume_outliers = sapply(biobank_standardized_data, function(x){compute_number_of_outliers(x)})
residuals_outliers = sapply(biobank_residuals_data, function(x){compute_number_of_outliers(x)})
summary(log_volume_outliers)
sort(log_volume_outliers)
summary(residuals_outliers)

which.max(log_volume_outliers)
compute_number_of_outliers(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`)

summary(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
sd(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
hist(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
length(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)

hist(biobank_residuals_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
hist(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
