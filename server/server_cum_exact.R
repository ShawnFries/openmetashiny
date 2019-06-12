#############################
##    effect_exact_cum_exact_norm      ##
#############################

# TODO: Split UI into 3 tabs like for server_exact_meta_exact_analysis (may be in UI page)
# TODO: Combine exact and normal methods in one tab (use rma.glmm for exact) and give options like for server_exact_meta_exact_analysis
# TODO: Fix for 2 proportions/2 means

dataModal2_exact_cum <- function(failed=F) {
  modalDialog(
    selectInput("type_exact_cum", "Type of data", c("One proportion", "Two proportions", "Diagnostic"), switch(input$dataType,
                                                                                                               "proportion" = "One proportion",
                                                                                                               "proportions" = "Two proportions",
                                                                                                               "diagnostic" = "Diagnostic"
    )
               ),
    conditionalPanel(
      condition="input.type_exact_cum_cum == 'Proportion'",
      selectInput("metric1_exact_cum", "Metric", c(raw_exact_proportion="PR", arcsine="PAS", logit="PLO"))
    ),
    conditionalPanel(
      condition="input.type_exact_cum_cum == 'Mean'",
      selectInput("metric2_exact_cum", "Metric", c(`MN - raw mean`="MN", 
                                             `MNLN - log transformed mean`="MNLN", 
                                             `CVLN - log transformed coefficient of variation`="CVLN",
                                             `SDLN - log transformed standard deviation`="SDLN"
                                            )
                 )
    ),
    conditionalPanel(
      condition="input.type_exact_cum_cum == 'Two proportions (2X2)'",
      selectInput("metric3_exact_cum", "Metric", c("RR", "OR", "RD", "AS", "PETO"))
    ),
    conditionalPanel(
      condition="input.type_exact_cum_cum == 'Two means'",
      selectInput("metric4_exact_cum", "Metric", c("MD", "SMD", "SMDH", "ROM"))
    ),conditionalPanel(
      condition="input.type_exact_cum_cum == 'Regression Coefficient'",
      selectInput("metric5_exact_cum",
                  "Metric", 
                  c(`COR - raw correlation coefficient`="COR",
                    `UCOR - unbiased raw correlation coefficient`="UCOR",
                    `ZCOR - Fisher's r-to-z transformed correlation coefficient`="ZCOR"
                  )
      )
    ),
    conditionalPanel(
      condition="input.type_exact_cum_cum == 'Diagnostic'",
      selectInput("metric6_exact_cum",
                  "Metric", 
                  c(`RR - log risk ratio`="RR", 
                    `OR - log odds ratio`="OR",
                    `RD - risk difference`="RD",
                    `AS - arcsine square root transformed risk difference`="AS",
                    `PETO - log odds ratio estimated with Peto's method`="PETO",
                    `PBIT - probit transformed risk difference`="PBIT",
                    `OR2DN - Transformed odds ratio for normal distributions`="OR2DN",
                    `OR2DL - Transformed odds ratio for logistic distributions`="OR2DL"
                  )
      )
    ),
    conditionalPanel(
      condition="input.metric4_exact_cum == 'MD'",
      checkboxInput("use_homoscedasticity_exact_cum",
                    "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
      )
    ),
    conditionalPanel(
      condition="input.metric4_exact_cum == 'SMD' || input.metric5_exact_cum == 'UCOR'",
      checkboxInput("variance_is_exact_approximate_exact_cum",
                    "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used", 
                    T
      )
    ),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("okexact_cum_escalc", "OK")                                             ####oknorm_exact_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_cum_exact, {
  showModal(dataModal2_exact_cum())
})


observeEvent(input$okexact_cum_escalc, {                         ####okexact_escalc
  if (!is.null(hot$data) & input$type_exact_cum == "One proportion") {
    vals$dataescalc_exact_cum <- tryCatch({
      rma.glmm(
        measure="PLO",
        xi=if (!is.null(hot$data$count)) count
        else if (!is.null(hot$data$counts)) counts
        else if (!is.null(hot$data$xi)) xi
        else if (!is.null(hot$data$x)) x
        else if (!is.null(hot$data$x_i)) x_i
        else if (!is.null(hot$data$x_is)) x_is
        else xis,
        
        ni=if (!is.null(hot$data$ni)) ni
        else if (!is.null(hot$data$nis)) nis
        else if (!is.null(hot$data$n_i)) n_i
        else if (!is.null(hot$data$n_is)) n_is
        else if (!is.null(hot$data$n)) n
        else ns,
        
        data=hot$data)},
      error=function(err) {
        #error handler picks up where error was generated
        print(paste("ERROR:  There must be at least one column named \"count\" or \"xi\" and one named \"ni\""))
      }
    )#ends tryCatch
    removeModal()
    
  } else if(!is.null(hot$data) & input$type_exact_cum == "Two proportions") {
    vals$dataescalc_exact_cum <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      rma.glmm(measure="OR",
               ai=ai,
               n1i=n1i,
               ci=ci,
               n2i=n2i,
               data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"ci\", \"n1i\", and \"n2i\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_exact_cum == "Diagnostic") {
    vals$dataescalc_exact_cum <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      rma.glmm(measure="OR",
               ai=ai,
               bi=bi,
               ci=ci,
               di=di,
               data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"bi\", \"ci\", and \"di\"")
      }
    )#ends tryCatch
    removeModal()
  } else {
    showModal(dataModal2_exact_cum(failed=T))
  }
  
  output$escalcdat_exact_cum <- renderTable({
    if (!is.null(hot$data)) {
      hot$data
    }
  })
})


#################################
##         oknorm_exact_res          ##
#################################

observeEvent(input$okexact_cum_res, {
  conflevel <- as.numeric(as.character(input$conflevel2_exact_cum))
  cc <- as.numeric(as.character(input$cc2_exact_cum))  # continuity correction

  res <- vals$dataescalc_exact_cum

  #####################NEEDS TO BE GENERALIZED############################
  output$forest_cum_exact <- renderPlot({
    conflevel <- as.numeric(as.character(input$conflevel2_exact_cum))
    
    forest(cumul(res, order(hot$data$year)), refline=NA, digits=input$digits2_exact_cum, level=conflevel)
  })

  output$msummary_cum_exact <- renderPrint({
    print(res)
  })

})


##########################
##     dynamic UI       ##
##########################

output$rand_exact_cum_estimation <- renderUI({
  # if(input$type=="."){
  #   NULL
  # }else

  if (input$fixed_exact_cum_exact == "FE") {                                                   ####fixed_exact_norm in ui_exact_meta_exact_norm.R
    selectInput("fixed_exact_cum_exact_est",
                "Estimation method",
                c("Inverse-variance")
               )
  } else if (input$fixed_exact_cum_exact == "RE"){                                             ####fixed_exact_norm in ui_exact_meta_exact_norm.R
    selectInput("rand_exact_cum_est",
                "Estimation method",
                c(`DerSimonian Laird`="DL",
                  `Hedges`="HE",
                  `Hunter-Schmidt`="HS",
                  `Sidik-Jonkman`="SJ",
                  `Maximum likelihood`="ML",
                  `Restricted ML`="REML",
                  `Empirical Bayes`="EB",
                  `Paule-Mandel`="PM",
                  `Generalized Q-statistic`="GENQ"
                ),
                "REML"
               )
  }
})