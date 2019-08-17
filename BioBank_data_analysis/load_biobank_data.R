library(dplyr)
library(tidyr)
library(ggplot2)

# Load the BioBank data:
bio.bank.data <- read.csv("~/Human_brain_research/Data/Biobank/ukb24562.csv",
                          header = T, stringsAsFactors = F)


# Clean data --------------------------------------------------------------
relevant_region <- c(seq(45,92)
                     ,seq(93,140)
                     ,seq(141,167)
                     ,seq(177,194)
                     ,seq(195,333))
relevant_data <- bio.bank.data[,relevant_region]
# Adding features names
data_set_columns_from_csv <- read.csv("Data/Biobank/convertcsv.csv",header = T,
                                      stringsAsFactors = F)
data_set_columns_from_csv$UDI_new <- paste0("X",data_set_columns_from_csv$UDI) %>% 
  gsub("-", ".", .)

data_set_columns <- data_set_columns_from_csv %>% 
  filter(UDI_new %in% names(relevant_data)) %>% 
  select(UDI_new,Description)

all(names(relevant_data) == data_set_columns$UDI_new[order(names(relevant_data))])
# Adding sex,DOB,Ethnicity
demographic_data <- bio.bank.data[,c(2,3,seq(13,15))]
names(demographic_data) <- c("Sex","Year_Of_Birth",
                             "Ethnic_BG_1",
                             "Ethnic_BG_2",
                             "Ethnic_BG_3")
names(relevant_data) = data_set_columns$Description
# Join
full_relevant_data <- cbind(demographic_data, relevant_data)
# Removing missing data
sapply(full_relevant_data, function(x){sum(is.na(x))})
# Some statistics on the demographic data 
full_relevant_data$Sex %>% table() %>% prop.table()
