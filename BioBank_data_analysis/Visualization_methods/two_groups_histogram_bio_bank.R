# Create a histogram of two groups
bioBankHistogramBySex <- function(feature_data,
                                  feature_name)
{
  feature_data %<>% na.omit()
  feature_data$Sex %<>% factor()
  ggplot(feature_data, aes_string(x=feature_name, fill="Sex")) +
    geom_histogram(binwidth=.005, alpha=.5, position="identity") + 
    ggtitle(paste0("Histogram of scaled log gender data ",feature_name))
}