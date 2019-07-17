## Main class for 

EM <- setClass(
  
  # Set class name
  "EM",
  
  # Set class properties
  slots = c(
    name = "character",
    data_file = "data.frame",
    EM_results = "list"
  ),
  
  # Constructor
  prototype=list(
    name = c(""),
    data_file = data.frame(),
    EM_results = list()
  ),
  
  validity = checkEMValidity
  
)


