#' In this script we apply linear regression over the bioBank data to
#' "integrate out" the brain volume effect on the distribution of the 
#' features

library(dplyr)
library(ggplot2)
library(Jmisc)

source("~/Documents/Human_brain_research/DAPHNA_JOEL/Mixture_models/source_script.R")
total.brain.volum.data <- bio.bank.data %>% select("X25010.2.0","X25004.2.0")

# Apply LM on a single feature:
feature.location <- sample(6:dim(full_relevant_data)[2], 1, F)

data.for.lm <- tibble(y = full_relevant_data[,feature.location],
                      x1 = total.brain.volum.data[,1],
                      x2 = total.brain.volum.data[,2])

data.for.lm.log.scale <- 
  data.for.lm %>% 
  filter(y > 0) %>% 
  mutate(log_y = log(y), log_x1 = log(x1), log_x2 = log(x2))

lm.for.first.feature <- lm(log_y ~ log_x1 + log_x2, data = data.for.lm.log.scale)

residuals.first.feature <- tibble(sex = full_relevant_data$Sex[!is.na(full_relevant_data[,feature.location]) & 
                                                                 full_relevant_data[,feature.location] > 0],
                                  value = lm.for.first.feature$residuals)
ggplot(residuals.first.feature, aes(x = value, fill = factor(sex))) + 
  geom_histogram(bins = 150)

residuals.first.feature <-
  residuals.first.feature %>% 
  mutate(z_score = (value - mean(value)) / sd(value)) 
ggplot(residuals.first.feature, aes(x = z_score, fill = factor(sex))) + 
  geom_histogram(bins = 150)

residuals.first.feature <-
  residuals.first.feature %>% 
  mutate(trimmed_z_score = ifelse(abs(z_score) > 3, sign(z_score) * 3,z_score)) 
ggplot(residuals.first.feature, aes(x = trimmed_z_score, fill = factor(sex))) + 
  geom_histogram(bins = 150)

### Testing the function:
feature.name <- names(full_relevant_data)[feature.location]
test.residuals.function <- applyLinearModelOverBrainFeature(feature.name)

ggplot(residuals.first.feature, 
       aes(x = trimmed_z_score, fill = factor(sex))) + 
  geom_histogram(bins = 150)

ggplot(test.residuals.function, 
       aes(x = trimmed_z_score, fill = factor(sex))) + 
  geom_histogram(bins = 150)

# Now the data is scaled and logged
simple.vs.compostie.llrt <- 
  simpleDistributionVsCompositeDistribution(
  residuals.first.feature$trimmed_z_score,
  residuals.first.feature$sex,
  "L")


t.test(residuals.first.feature$trimmed_z_score[residuals.first.feature$sex == 0],
       residuals.first.feature$trimmed_z_score[residuals.first.feature$sex == 1])

pure.vs.mixed <- 
  pureTypeMixtureVsCompositeMixture(residuals.first.feature$trimmed_z_score,
                                    residuals.first.feature$sex,
                                    distribution_model = "L",
                                    data_needs_preparation = F)

all.hypothesis.test <- 
  applyHypothesisOverResiduals(test.residuals.function)

list.of.names <- names(full_relevant_data)
full.function.test <- fullBrainFeatureAnalysis(feature.name,list.of.names)
full.function.test$hypothesis_results$simple_vs_compostie_llrt
analyzeFullBrainFeature(full.function.test,plot = T)
