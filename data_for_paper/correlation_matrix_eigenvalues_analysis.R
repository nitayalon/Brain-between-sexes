q2_q4_eigenvalues <- rowMeans(cbind(q2_q4_mixture_model_men_correlation_eigenvalues$values,q2_q4_mixture_model_women_correlation_eigenvalues$values))
q1_q3_eigenvalues <- rowMeans(cbind(q1_q3_mixture_model_men_correlation_eigenvalues$values,q1_q3_mixture_model_women_correlation_eigenvalues$values))
single_mixture_eigenvalues <- rowMeans(cbind(single_mixture_model_women_correlation_eigenvalues$values,
                                             single_mixture_model_men_correlation_eigenvalues$values))
sum(q2_q4_eigenvalues>1) / length(q2_q4_eigenvalues)
sum(q1_q3_eigenvalues>1) / length(q1_q3_eigenvalues)
sum(single_mixture_eigenvalues>1) / length(single_mixture_eigenvalues)

q2_q4_eigenvalues <- rowMeans(cbind(q2_q4_mixture_model_men_correlation_eigenvalues_mean_fa$values,q2_q4_mixture_model_women_correlation_eigenvalues_mean_fa$values))
q1_q3_eigenvalues <- rowMeans(cbind(q1_q3_mixture_model_men_correlation_eigenvalues_mean_fa$values,q1_q3_mixture_model_women_correlation_eigenvalues_mean_fa$values))
single_mixture_eigenvalues <- rowMeans(cbind(single_mixture_model_women_correlation_eigenvalues_mean_fa$values,
                                             single_mixture_model_men_correlation_eigenvalues_mean_fa$values))
sum(q2_q4_eigenvalues>1) / length(q2_q4_eigenvalues)
sum(q1_q3_eigenvalues>1) / length(q1_q3_eigenvalues)
sum(single_mixture_eigenvalues>1) / length(single_mixture_eigenvalues)
