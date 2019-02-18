# Apply EM on brain feature data
library(tools)

# Set
setGeneric(name = "applyEMAlgorithmOnBrainData",
           def = function(theObject)
           {
             standardGeneric("applyEMAlgorithmOnBrainData")
           }
)

setMethod(f = "applyEMAlgorithmOnBrainData", 
          signature = "EM", 
          definition = function(theObject)
          {
            if(is.null(theObject@data_file))
            {
              cat("Data file slot is empty","\n")
              return(theObject)
            }
            theObject@EM_results <- fullMLEAndLlrtForFeature(theObject@data_file)
            checkEMValidity(theObject)
            return(theObject)
          }  
)


# Get
setGeneric(name = "getEMResults",
           def = function(theObject)
           {
             standardGeneric("getEMResults")
           }
)

setMethod(f = "getEMResults", 
          signature = "EM", 
          definition = function(theObject)
          {
            return(theObject@EM_results$EM_results)
          }  
)

setGeneric(name = "getLlrt",
           def = function(theObject)
           {
             standardGeneric("getLlrt")
           }
)

setMethod(f = "getLlrt", 
          signature = "EM", 
          definition = function(theObject)
          {
            
            return(theObject@EM_results$llrt)
          }  
)
