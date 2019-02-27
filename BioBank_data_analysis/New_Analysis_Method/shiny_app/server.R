library(shiny)
library(magrittr)

shinyServer(function(input, output, session) {
  
  data_path <- "~/Documents/Human_brain_research/DAPHNA_JOEL/BioBank_data_analysis/New_Analysis_Method/Residuals_analysis_methods/Results"
  load(sprintf("%s/New_analysis_method_results.RData", data_path))
  bio_bank_columns <- names(results.list)
  updateSelectInput(session,"columns",label="label",choices=bio_bank_columns)
  
  dat <- reactive({
    feature_data <- results.list[[input$columns]]
    feature_data
  })
  
  output$groupsHistogram <-renderPlot({
    feature_data <- dat()
    plot_data <- feature_data$feature_residuals
    em_data <- feature_data$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters
    ggplot(plot_data, aes(x=trimmed_z_score, fill=factor(sex))) +
      geom_histogram(aes(y=..density..),
                     bins = input$n_bins, 
                     alpha=.8, 
                     position="identity") +
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$p),
                    colour = "blue", lwd = 1.2) + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = (1 - em_data$p)),
                    colour = "blue", lwd = 1.2,linetype = "dashed") + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$q),
                    colour = "red", lwd = 1.0) + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = (1 - em_data$q)),
                    colour = "red", lwd = 1.0,linetype = "dashed") + 
      ggtitle(paste0("Histogram of gender data"))
  }
  ,height = 400,width = 600
  )
  
  output$pValueSummary <- renderTable({
    statistical_analysis_results <- dat()
    summary_of_analysis <- summarizeFullBrainFeature(statistical_analysis_results)
    data.frame(p_value_simple_vs_composite = summary_of_analysis$p_value_simple_vs_composite,
               p_value_pure_vs_composite = summary_of_analysis$p_value_pure_vs_composite
    )
  },
  caption = "P - values summary",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$EmSummaryAlternative <- renderTable({
    full_analysis_results <- dat()
    em_results_for_feature <- full_analysis_results$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters
    p <- em_results_for_feature$p
    q <- em_results_for_feature$q
    men_mean <- em_results_for_feature$mu_1
    women_mean <- em_results_for_feature$mu_2
    sigma_2_men <- em_results_for_feature$sigma_2_men
    sigma_2_women <- em_results_for_feature$sigma_2_women
    data.frame(p = p,
               q = q,
               mu_1 = mu_1,
               mu_2 = mu_2,
               sigma_2_men = sigma_2_men,
               sigma_2_women = sigma_2_women
    )
  },
  caption = "EM results - mixture model results",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$EmSummaryNull <- renderTable({
    full_analysis_results <- dat()
    em_results_for_feature <- full_analysis_results$hypothesis_results$pure_type_vs_mixed_gender_em_results$null_hypothesis
    men_mean <- em_results_for_feature$men_mean
    women_mean <- em_results_for_feature$women_mean
    sigma_2 <- em_results_for_feature$null_var
    data.frame(men_mean = men_mean,
               women_mean = women_mean,
               sigma_2 = sigma_2
    )
  },
  caption = "EM results - pure type model",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )

})
