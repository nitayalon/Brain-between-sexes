# Load financial data from file
library(tools)

# Set
setGeneric(name = "loadBrainFeatureData",
           def = function(theObject, brain_file_class, feature_name)
           {
             standardGeneric("loadBrainFeatureData")
           }
)

setMethod(f = "loadBrainFeatureData", 
          signature = "brainFeatureData", 
          definition = function(theObject, brain_file_class, feature_name)
          {
            # Validate brain class instance
            if(!(class(brain_file_class) == "brainFile"))
            {
              cat("Can load feature data only from brain class","\n")
              return(NULL)
            }
            if(!(feature_name %in% names(getRawBrainFileData(brain_file_class))))
            {
              cat("The feature you're looking for isn't in the data","\n")
              return(NULL)
            }
            theObject@name <- feature_name
            feature_data <- subset(getRawBrainFileData(brain_file_class),
                                   select = c("bio_sex",feature_name))
            theObject@value <- feature_data
            checkBrainFeatureDataValidity(theObject)
            return(theObject)
          }  
)

# Get

setGeneric(name = "getRawBrainFeatureData",
           def = function(theObject)
           {
             standardGeneric("getRawBrainFeatureData")
           }
)

setMethod(f = "getRawBrainFeatureData", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            return(raw_data@value)
          }  
)

setGeneric(name = "getBrainFeatureName",
           def = function(theObject)
           {
             standardGeneric("getBrainFeatureName")
           }
)

setMethod(f = "getBrainFeatureName", 
          signature = "brainFeatureData", 
          definition = function(theObject)
          {
            return(theObject@name)
          }  
)