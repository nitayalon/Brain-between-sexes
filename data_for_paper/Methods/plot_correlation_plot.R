plotCorrelationPlot <- function(correlation_matrix_data,region_name,tl_cex = 1){
  index <- grep(region_name,sort(correlation_matrix_data$legend$feature_name))
  corrplot(correlation_matrix_data$correlation_matrix[index,index], 
           tl.cex = tl_cex,tl.srt = 45,cl.pos="r")
}