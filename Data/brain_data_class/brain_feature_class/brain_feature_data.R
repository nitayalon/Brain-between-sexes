# Basic brain feature class

brainFeatureData <- setClass(
  
  # Set class name
  "brainFeatureData",
  
  # Set class properties
  slots = c(
    name = "character",
    value = "data.frame",
    logged_value = "data.frame",
    scaled_value = "data.frame",
    scaled_log_value = "data.frame"
  ),
  
  # Constructor
  prototype=list(
    name = " ",
    value = data.frame(),
    logged_value = data.frame(),
    scaled_value = data.frame(),
    scaled_log_value = data.frame()
  ),
  validity = checkBrainFeatureDataValidity
)




