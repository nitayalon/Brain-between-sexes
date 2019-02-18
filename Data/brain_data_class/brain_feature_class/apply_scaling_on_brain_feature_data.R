# Load financial data from file
library(tools)

# Set
setGeneric(name = "applyScalingOnBrainFeatureData",
           def = function(theObject)
           {
             standardGeneric("applyScalingOnBrainFeatureData")
           }
)

setMethod(f = "applyScalingOnBrainFeatureData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            if(is.null(theObject@logged_value) || is.null(theObject@value))
            {
              cat("No data to scale, make sure you've uploaded the data","\n")
              return(theObject)
            }
            bio_sex <- factor(theObject@value[,1])
            theObject@scaled_value <- data.frame(bio_sex = bio_sex,
                                                 value = scale(theObject@value[,2]))
            theObject@scaled_log_value <- data.frame(bio_sex = bio_sex,
                                                     value = scale(theObject@logged_value[,2]))
            checkBrainFeatureDataValidity(theObject)
            return(theObject)
          }  
)

# Get

setGeneric(name = "getScaledBrainData",
           def = function(theObject)
           {
             standardGeneric("getScaledBrainData")
           }
)

setMethod(f = "getScaledBrainData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            
            return(theObject@scaled_value)
          }  
)

setGeneric(name = "getScaledLogBrainData",
           def = function(theObject)
           {
             standardGeneric("getScaledLogBrainData")
           }
)

setMethod(f = "getScaledLogBrainData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            
            return(theObject@scaled_log_value)
          }  
)
