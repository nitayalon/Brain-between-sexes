library(effsize)
log_feature_em = lapply(log_feature_data_for_em, function(x){applyHypothesisOverResiduals(x$data_for_em, including_equal_mixture_model = F)})
sapply(log_feature_em, function(x){x$pure_types_vs_mixture_model_llr})
