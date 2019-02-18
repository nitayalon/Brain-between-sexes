library(dplyr)
library(tidyr)
library(ggplot2)

# Load the BioBank data:
bio.bank.data <- read.csv("../Data/Biobank/ukb24562.csv", 
                          header = T, stringsAsFactors = F)


# Clean data --------------------------------------------------------------
relevant_region <- c(seq(45,92)
                     ,seq(141,167)
                     ,seq(93,140)
                     ,seq(177,194)
                     ,seq(195,333))
relevant_data <- bio.bank.data[,relevant_region]
# Adding sex,DOB,Ethnicity
demographic_data <- bio.bank.data[,c(2,3,seq(13,15))]
names(demographic_data) <- c("Sex","Year_Of_Birth",
                             "Ethnic_BG_1",
                             "Ethnic_BG_2",
                             "Ethnic_BG_3")
# Join
full_relevant_data <- cbind(demographic_data, relevant_data)
# Removing missing data
sapply(full_relevant_data, function(x){sum(is.na(x))})
# Some statistics on the demographic data 
full_relevant_data$Sex %>% table() %>% prop.table()
full_relevant_data$Ethnic_BG_1 %>% table() %>% prop.table()
qplot(full_relevant_data$Ethnic_BG_1, geom = "histogram")
