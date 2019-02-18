library(plotly)
library(dplyr)
create3DPlots <- function(grid_data_frame,parameter_of_interest = c("theta_mas","theta_fem"),trim_prop = 0.2) 
{
  # INPUT - A data.frame object with at least 3 columns, one of them called llk
  # OUTPUT - A 3D plot of two parameters vs llk
  
  # Inpout validation
  stopifnot(is.data.frame(grid_data_frame))
  if(length(parameter_of_interest) != 2)
    {
    warning("You must provide 2 parameters of interest")
    return(NULL)
    }
  
  # Making the plot
  parameter_of_interest[3] <- "llk"
  where_to_trim <- quantile(grid_data_frame$llk,trim_prop)
  plot_matrix <- grid_data_frame %>% filter(llk > where_to_trim)
  
  full_plot <- plot_ly(plot_matrix, x = ~theta_mas, y = ~theta_fem, z = ~llk, color = ~llk) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Theta_mas'),
                        yaxis = list(title = 'Theta_fem'),
                        zaxis = list(title = 'log likelihood')))
  return(full_plot)
}