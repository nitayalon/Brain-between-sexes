# Load brain dictionary file
library(tools)

# Set
setGeneric(name = "loadBrainDictionaryFile",
           def = function(theObject)
           {
             standardGeneric("loadBrainDictionaryFile")
           }
)

setMethod(f = "loadBrainDictionaryFile", 
          signature = "brainFile", 
          definition = function(theObject)
          {
            data_dir <- "~/mastersdegree/Thesis/DAPHNA_JOEL/Data/Full_Data/Unified_Data/"
            dictionary <- read.csv(paste0(data_dir,"/DataRelease_2014-04-22.csv"),
                                   header = T, stringsAsFactors = F)
            theObject@dictionary <- dictionary
            return(theObject)
          }  
)

# Get

setGeneric(name = "getBrainDataDictionary",
           def = function(theObject)
           {
             standardGeneric("getBrainDataDictionary")
           }
)

setMethod(f = "getBrainDataDictionary", 
          signature = "brainFile", 
          definition = function(theObject)
          {
            return(theObject@dictionary)
          }  
)