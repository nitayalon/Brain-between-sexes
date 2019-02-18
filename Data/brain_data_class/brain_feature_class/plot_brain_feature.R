# Load financial data from file
library(ggplot2)

# Set
setGeneric(name = "plotBrainFeature",
           def = function(theObject,scaled, n_bins = 50)
           {
             standardGeneric("plotBrainFeature")
           }
)

setMethod(f = "plotBrainFeature", 
          signature = "brainFeatureData", 
          definition = function(theObject, scaled, n_bins = 50)
          {
            theObject@value$bio_sex <- factor(theObject@value$bio_sex)
            theObject@logged_value$bio_sex <- factor(theObject@value$bio_sex)
            theObject@scaled_value$bio_sex <- factor(theObject@value$bio_sex)
            theObject@scaled_log_value$bio_sex <- factor(theObject@value$bio_sex)
            if(!scaled)
            {
              non_log <- ggplot(theObject@value, aes_string(x = theObject@name,
                                                     fill = "bio_sex")) + 
                geom_histogram(bins = n_bins)
              logged <- ggplot(theObject@logged_value, aes_string(theObject@name,
                                                           fill = "bio_sex")) + 
                geom_histogram(bins = n_bins)
            }
            else
            {
              non_log <- ggplot(theObject@scaled_value, aes_string("value", 
                                                            fill = "bio_sex")) + 
                geom_histogram(bins = n_bins)
              logged <- ggplot(theObject@scaled_log_value, aes_string("value",
                                                           fill = "bio_sex")) + 
                geom_histogram(bins = n_bins)
            }
            return(list(natural = non_log,
                        logged = logged))
          }  
)

