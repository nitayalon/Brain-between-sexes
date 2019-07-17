# Load full brain data from excel file
library(tools)
require(gdata)
# Set
setGeneric(name = "loadBrainDataFile",
           def = function(theObject)
           {
             standardGeneric("loadBrainDataFile")
           }
)

setMethod(f = "loadBrainDataFile", 
          signature = "brainFile", 
          definition = function(theObject)
          {
            path_to_file <- paste0(theObject@target_dir,theObject@name)
            if(!(file.exists(path_to_file)))
            {
              cat(path_to_file, "\n")
              cat("No file found","\n")
              return(NULL)
            }
            
            data_file <- read.xls(path_to_file,sheet = 1,header = T)
            
            if(tolower(names(data_file)) %>% grep("sex",.) ||
               names(data_file) %>% grep("bio_sex",.))
            {
              final_data <- data_file
            }
            else
            {
              gender_data <- theObject@dictionary[,c("Subject_ID","Sex")]  
              full_data <- merge(gender_data,data_file)
              final_data <- full_data
            }
            
            # Sometimes instead of bio sex there's a Sex column
            names(final_data) <- tolower(names(final_data))
            if("sex" %in% names(final_data))
            {
              levels(final_data$sex) <- c("M","F")
              final_data$bio_sex <- as.numeric(final_data$sex)
              final_data <- subset(final_data, select = -c(sex))
            }
            
            theObject@brain_file <- final_data
            checkBrainDataFileValidity(theObject)
            return(theObject)
          }  
)

# Get

setGeneric(name = "getRawBrainFileData",
           def = function(theObject)
           {
             standardGeneric("getRawBrainFileData")
           }
)

setMethod(f = "getRawBrainFileData", 
          signature = "brainFile", 
          definition = function(theObject)
          {
            return(theObject@brain_file)
          }  
)

setGeneric(name = "getBrainFileName",
           def = function(theObject)
           {
             standardGeneric("getBrainFileName")
           }
)

setMethod(f = "getBrainFileName", 
          signature = "brainFile", 
          definition = function(theObject)
          {
            return(theObject@name)
          }  
)