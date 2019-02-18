# Apply EM on brain feature data
library(tools)

# Set
setGeneric(name = "loadBrainDataForEM",
           def = function(theObject,data_file)
           {
             standardGeneric("loadBrainDataForEM")
           }
)

setMethod(f = "loadBrainDataForEM", 
          signature = "EM", 
          definition = function(theObject,data_file)
          {
            if(class(data_file) != "data.frame")
            {
              cat("data file isn't a data.frame", "\n")
              cat(class(data_file), "\n")
              return(theObject)
            }
            theObject@data_file <- data_file
            checkEMValidity(theObject)
            return(theObject)
          }  
)

# Get
setGeneric(name = "getEMDataFile",
           def = function(theObject)
           {
             standardGeneric("getEMDataFile")
           }
)

setMethod(f = "getEMDataFile", 
          signature = "EM", 
          definition = function(theObject)
          {
            return(theObject@data_file)
          }  
)
