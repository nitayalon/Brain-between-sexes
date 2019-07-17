brainFile <- setClass(
  
  # Set class name
  "brainFile",
  
  # Set class properties
  slots = c(
    name = "character",
    target_dir = "character",
    destination_dir = "character",
    dictionary = "data.frame",
    brain_file = "data.frame"
  ),
  
  # Constructor
  prototype=list(
    name = " ",
    target_dir = " ",
    destination_dir = " ",
    dictionary = data.frame(),
    brain_file = data.frame()
  ),
  
  validity = checkBrainDataFileValidity
  
)


