## KS Test for brain feature - BioBank data
applyFullKSTestPerFeature <- function(feature_data,
                                      model = c("Log_Normal","Normal")) {
  
  alpha = 0.05
  distribution_model = match.arg(model)
  feature_data_transformed <- 
    switch (distribution_model,
            Log_Normal = log(
              unique(
                na.omit(
                  prepareDataForLogTransformation(feature_data)
                  )
                )
              ),
            Normal = unique(
              na.omit(
                feature_data
                )
            )
    )
  h0_mean <- mean(feature_data_transformed)
  h0_standard_diviation <- sd(feature_data_transformed)
  ks_test <- ks.test(
    feature_data_transformed,"pnorm",h0_mean,h0_standard_diviation)
  ks_results <- c(ks_test$statistic, ks_test$p.value)
}