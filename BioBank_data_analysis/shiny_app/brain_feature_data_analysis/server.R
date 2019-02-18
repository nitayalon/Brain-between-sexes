library(shiny)
library(ggplot2)
library(magrittr)

shinyServer(function(input, output, session) {
  
  load("~/mastersdegree/Thesis/DAPHNA_JOEL/BioBank_data_analysis/shiny_app/BioBank_data_for_analysis.RData")
  bio_bank_columns <- names(full_relevant_data)[-c(1:5)]
  updateSelectInput(session,"columns",label="label",choices=bio_bank_columns)
  
  load("~/mastersdegree/Thesis/DAPHNA_JOEL/BioBank_data_analysis/shiny_app/EM_results_pure_type_vs_mixture_model.RData")
  load("~/mastersdegree/Thesis/DAPHNA_JOEL/BioBank_data_analysis/shiny_app/Cohens_D_and_T_test.RData")
  
  dat <- reactive({
    feature_data <- full_relevant_data[input$columns]
    feature_data$Sex <- full_relevant_data$Sex
    feature_data <-  na.omit(feature_data)
    feature_data
  })
  
  em_results <- reactive({
    feature_em_results <- composite_hypothesis_result[[input$columns]]
    feature_em_results
  })
  
  statistical_test <- reactive({
    statistical_test_results <- t_test_and_cohens_d_by_gender[[input$columns]]
    statistical_test_results
  })
  
  output$EmSummaryAlternative <- renderTable({
    em_results_for_feature <- em_results()
    p <- em_results_for_feature$alternative_hypothesis$m_parameters$p
    q <- em_results_for_feature$alternative_hypothesis$m_parameters$q
    mu_1 <- em_results_for_feature$alternative_hypothesis$m_parameters$mu_1
    mu_2 <- em_results_for_feature$alternative_hypothesis$m_parameters$mu_2
    sigma_2 <- em_results_for_feature$alternative_hypothesis$m_parameters$sigma_2
    llrt <- em_results_for_feature$llrt
    data.frame(p = p,
               q = q,
               mu_1 = mu_1,
               mu_2 = mu_2,
               sigma_2 = sigma_2,
               llrt = llrt
    )
  },
  caption = "EM results - mixture model",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$EmSummaryNull <- renderTable({
    em_results_for_feature <- em_results()
    men_mean <- em_results_for_feature$null_hypothesis$men_mean
    women_mean <- em_results_for_feature$null_hypothesis$women_mean
    sigma_2 <- em_results_for_feature$null_hypothesis$null_var
    data.frame(men_mean = men_mean,
               women_mean = women_mean,
               sigma_2 = sigma_2
    )
  },
  caption = "EM results - pure type model",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$groupsHistogram <-renderPlot({
    em_results_for_feature <- em_results()
    p <- em_results_for_feature$alternative_hypothesis$m_parameters$p
    q <- em_results_for_feature$alternative_hypothesis$m_parameters$q
    mu_1 <- em_results_for_feature$alternative_hypothesis$m_parameters$mu_1
    mu_2 <- em_results_for_feature$alternative_hypothesis$m_parameters$mu_2
    sigma_2 <- em_results_for_feature$alternative_hypothesis$m_parameters$sigma_2
    
    feature_data <- dat()
    feature_data$Sex <- factor(feature_data$Sex)
    if(input$transform_data_for_analysis)
    {
      feature_data <- removeZeroData(feature_data,input$columns)
      feature_data[input$columns][,1] <-
        feature_data[input$columns][,1] %>%
        logNormalDataPreparation()
    }
    ggplot(feature_data, aes_string(x=input$columns, fill="Sex")) +
      geom_histogram(aes(y=..density..),
                     bins = input$n_bins, 
                     alpha=.8, 
                     position="identity") +
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = mu_1, sigma = sqrt(sigma_2), lambda = p),
                    colour = "blue", lwd = 1.2) + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = mu_2, sigma = sqrt(sigma_2) , lambda = 1 - p),
                    colour = "blue", lwd = 1.2) + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = mu_1, sigma = sqrt(sigma_2), lambda = q),
                    colour = "red", lwd = 1.0) + 
      stat_function(geom = "line", 
                    fun = plot_mix_comps,
                    args = list(mu = mu_2, sigma = sqrt(sigma_2) , lambda = 1 - q),
                    colour = "red", lwd = 1.0) + 
      ggtitle(paste0("Histogram of gender data ",input$columns))
  }
  ,height = 400,width = 600
  )
  
  output$StatSummary <- renderTable({
    statistical_test_for_feature <- statistical_test()
    t_test_value <- statistical_test_for_feature$t_test$statistic
    t_test_p_value <- statistical_test_for_feature$t_test$p.value
    t_test_conf_int <- statistical_test_for_feature$t_test$conf.int
    cohens_d_value <- statistical_test_for_feature$cohens_d$estimate
    cohens_d_conf_int <- statistical_test_for_feature$cohens_d$conf.int
    data.frame(t_test_value = t_test_value,
               t_test_p_value = t_test_p_value,
               cohens_d_value = cohens_d_value
    )
  },
  caption = "T test and Cohen's D statistics",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$TconfidenceInterval <- renderTable({
    statistical_test_for_feature <- statistical_test()
    t_test_conf_int <- statistical_test_for_feature$t_test$conf.int
    data.frame(lower = t_test_conf_int[1],
               upper = t_test_conf_int[2]
    )
  },
  caption = "Confidnece interval T-Test",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  output$CohensconfidenceInterval <- renderTable({
    statistical_test_for_feature <- statistical_test()
    cohens_d_conf_int <- statistical_test_for_feature$cohens_d$conf.int
    data.frame(lower = cohens_d_conf_int[1],
               upper = cohens_d_conf_int[2]
    )
  },
  caption = "Confidnece interval Cohen's D",
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
})
