# Apply EM on brain feature data
library(tools)

# Set
setGeneric(name = "setEMName",
           def = function(theObject,feature_name,logged)
           {
             standardGeneric("setEMName")
           }
)

setMethod(f = "setEMName", 
          signature = "EM", 
          definition = function(theObject,feature_name,logged)
          {
            class_name <- paste(feature_name, as.character(logged), sep = "_")
            theObject@name <- class_name
            checkEMValidity(theObject)
            return(theObject)
          }  
)

# Get
setGeneric(name = "getEMName",
           def = function(theObject)
           {
             standardGeneric("getEMName")
           }
)

setMethod(f = "getEMName", 
          signature = "EM", 
          definition = function(theObject)
          {
            return(theObject@name)
          }  
)
