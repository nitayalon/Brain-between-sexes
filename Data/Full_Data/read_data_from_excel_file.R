# install.packages("gdata")
require(gdata)
require(magrittr)
data.dir <- "~/Human_brain_research/Data/Full_Data/Unified_Data/"

# Gender data
dictionary <- read.csv(paste0(data.dir,"/DataRelease_2014-04-22.csv"),
                       header = T, stringsAsFactors = F)

# Select only the GSP files
GSP.files <- list.files(data.dir)[
  list.files(data.dir) %>% 
    grep("GSP",.)]

# Add gender data (if missing)
addGenderData <- function(file_name){
  data_file <- read.xls(paste(data.dir,file_name,sep = "/"),
                        sheet = 1,header = T)
  if(tolower(names(data_file)) %>% grep("sex",.) ||
     names(data_file) %>% grep("bio_sex",.))
  {
    return(data_file)
  }
  else
  {
    gender_data <- dictionary[,c("Subject_ID","Sex")]  
    full_data <- merge(gender_data,data_file)
    return(full_data)
  }
}


