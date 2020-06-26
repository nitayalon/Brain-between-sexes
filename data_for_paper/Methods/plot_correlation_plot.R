plotCorrelationPlot <- function(correlation_matrix_data, region_name,tl_cex = 1, type = 'circle'){
  index <- grep(region_name,sort(correlation_matrix_data$legend$feature_name))
  corrplot(correlation_matrix_data$correlation_matrix[index,index], 
           tl.cex = tl_cex,tl.srt = 45,cl.pos="n", mar = c(0.2,0.2,0.2,0.2), method = type,
           number.cex=0.55,col = c("black"))
}