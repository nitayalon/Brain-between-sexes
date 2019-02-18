# Load financial data from file
library(tools)

# Set
setGeneric(name = "applyLogTransformationOnBrainFeatureData",
           def = function(theObject)
           {
             standardGeneric("applyLogTransformationOnBrainFeatureData")
           }
)

setMethod(f = "applyLogTransformationOnBrainFeatureData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            if(!("value" %in% slotNames(theObject)))
            {
              cat("Sorry, no brain data to transform","\n")
              return(theObject)
            }
            if(any(theObject@value < 0))
            {
              cat("Negative values, cannot apply log transformation","\n")
              return(theObject)
            }
            theObject@logged_value <- log(theObject@value)
            checkBrainFeatureDataValidity(theObject)
            return(theObject)
          }  
)

# Get

setGeneric(name = "getLoggedBrainData",
           def = function(theObject)
           {
             standardGeneric("getLoggedBrainData")
           }
)

setMethod(f = "getLoggedBrainData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            return(theObject@logged_value)
          }  
)

