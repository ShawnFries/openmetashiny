#############################
##    effect_cum_norm      ##
#############################

# TODO: Split UI into 3 tabs like for server_meta_analysis (may be in UI page)
# TODO: Combine exact and normal methods in one tab (use rma.glmm for exact) and give options like for server_meta_analysis
# TODO: Fix for 2 proportions/2 means

dataModal2_cum <- function(failed=F) {
  modalDialog(
    selectInput("type_cum", "Type of data", c("Proportion", "Mean", "Two proportions (2X2)", "Two means"), switch(dataType$type,
                                                                                                                  "proportion" = "Proportion",
                                                                                                                  "mean" = "Mean",
                                                                                                                  "proportions" = "Two proportions (2X2)",
                                                                                                                  "means" = "Two means",
                                                                                                                  "regression coefficient" = "Regression coefficient",
                                                                                                                  "generic effect size" = "Generic effect size",
                                                                                                                  "diagnostic" = "Diagnostic"
                                                                                                                 )
               ),
    conditionalPanel(
      condition="input.type_cum == 'Proportion'",
      selectInput("metric1_cum", "Metric", c(raw_proportion="PR", arcsine="PAS", logit="PLO"))
    ),
    conditionalPanel(
      condition="input.type_cum == 'Mean'",
      selectInput("metric2_cum", "Metric", c(`MN - raw mean`="MN", 
                                             `MNLN - log transformed mean`="MNLN", 
                                             `CVLN - log transformed coefficient of variation`="CVLN",
                                             `SDLN - log transformed standard deviation`="SDLN"
                                            )
                 )
    ),
    conditionalPanel(
      condition="input.type_cum == 'Two proportions (2X2)'",
      selectInput("metric3_cum", "Metric", c("RR", "OR", "RD", "AS", "PETO"))
    ),
    conditionalPanel(
      condition="input.type_cum == 'Two means'",
      selectInput("metric4_cum", "Metric", c("MD", "SMD", "SMDH", "ROM"))
    ),conditionalPanel(
      condition="input.type_cum == 'Regression Coefficient'",
      selectInput("metric5_cum",
                  "Metric", 
                  c(`COR - raw correlation coefficient`="COR",
                    `UCOR - unbiased raw correlation coefficient`="UCOR",
                    `ZCOR - Fisher's r-to-z transformed correlation coefficient`="ZCOR"
                  )
      )
    ),
    conditionalPanel(
      condition="input.type_cum == 'Diagnostic'",
      selectInput("metric6_cum",
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
      condition="input.metric4_cum == 'MD'",
      checkboxInput("use_homoscedasticity_cum",
                    "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
      )
    ),
    conditionalPanel(
      condition="input.metric4_cum == 'SMD' || input.metric5_cum == 'UCOR'",
      checkboxInput("variance_is_approximate_cum",
                    "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used", 
                    T
      )
    ),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("oknorm_cum_escalc", "OK")                                             ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_cum_norm, {
  showModal(dataModal2_cum())
})


observeEvent(input$oknorm_cum_escalc, {                                                              ####oknorm_cum_escalc
  if (!is.null(hot$data) & input$type_cum == "Proportion") {
    vals$dataescalc_cum <- tryCatch({
      escalc(
        measure=input$metric1_cum,
        
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
    
  } else if (!is.null(hot$data) & input$type_cum == "Mean") {
    vals$dataescalc_cum <- tryCatch({
      escalc(measure=input$metric2_cum,
             
             mi=if (!is.null(hot$data$mi)) mi
             else if (!is.null(hot$data$m)) m
             else if (!is.null(hot$data$m_i)) m_i
             else if (!is.null(hot$data$m_is)) m_is
             else if (!is.null(hot$data$mis)) mis
             else if (!is.null(hot$data$u)) u
             else if (!is.null(hot$data$ui)) ui
             else if (!is.null(hot$data$u_i)) u_i
             else if (!is.null(hot$data$u_is)) u_is
             else if (!is.null(hot$data$uis)) uis
             else if (!is.null(hot$data$mu)) mu
             else if (!is.null(hot$data$mui)) mui
             else if (!is.null(hot$data$mu_i)) mu_i
             else if (!is.null(hot$data$mu_is)) mu_is
             else muis,
             
             sdi=if (!is.null(hot$data$sdi)) sdi
             else if (!is.null(hot$data$sd)) sd
             else if (!is.null(hot$data$sd_i)) sd_i
             else if (!is.null(hot$data$sd_is)) sd_is
             else if (!is.null(hot$data$sdis)) sdis
             else if (!is.null(hot$data$sigma)) sigma
             else if (!is.null(hot$data$sigmai)) sigmai
             else if (!is.null(hot$data$sigma_i)) sigma_i
             else if (!is.null(hot$data$sigma_is)) sigma_is
             else sigmais,
             
             ni=if (!is.null(hot$data$ni)) ni
             else if (!is.null(hot$data$nis)) nis
             else if (!is.null(hot$data$n_i)) n_i
             else if (!is.null(hot$data$n_is)) n_is
             else if (!is.null(hot$data$n)) n
             else ns,
             
             data=hot$data
      )
    },
    error=function(err) {
      print("ERROR:  There must be at least one column each named \"mi\", \"sdi\" and \"ni\"")
    }
    )#ends tryCatch
    removeModal()
    
  } else if(!is.null(hot$data) & input$type_cum == "Two proportions (2X2)") {
    vals$dataescalc_cum <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3_cum,
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
    
  } else if (!is.null(hot$data) & input$type_cum=="Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc_cum <- tryCatch({
      escalc(measure=input$metric4_cum, 
             m1i=if (!is.null(hot$data$m1i)) hot$data$m1i
             else if (!is.null(hot$data$m1)) hot$data$m1
             else if (!is.null(hot$data$m1_i)) hot$data$m1_i
             else if (!is.null(hot$data$m1_is)) hot$data$m1_is
             else if (!is.null(hot$data$m1is)) hot$data$m1is
             else if (!is.null(hot$data$u1)) hot$data$u1
             else if (!is.null(hot$data$u1i)) hot$data$u1i
             else if (!is.null(hot$data$u1_i)) hot$data$u1_i
             else if (!is.null(hot$data$u1_is)) hot$data$u_is
             else if (!is.null(hot$data$u1is)) hot$data$uis
             else if (!is.null(hot$data$mu1)) hot$data$mu1
             else if (!is.null(hot$data$mu1i)) hot$data$mu1i
             else if (!is.null(hot$data$mu1_i)) hot$data$mu1_i
             else if (!is.null(hot$data$mu1_is)) hot$data$mu1_is
             else hot$data$mu1is,
             sd1i=hot$data$sd1i,
             n1i=hot$data$n1i,
             m2i=if (!is.null(hot$data$m2i)) hot$data$m2i
             else if (!is.null(hot$data$m2)) hot$data$m2
             else if (!is.null(hot$data$m2_i)) hot$data$m2_i
             else if (!is.null(hot$data$m2_is)) hot$data$m2_is
             else if (!is.null(hot$data$m2is)) hot$data$m2is
             else if (!is.null(hot$data$u2)) hot$data$u2
             else if (!is.null(hot$data$u2i)) hot$data$u2i
             else if (!is.null(hot$data$u2_i)) hot$data$u2_i
             else if (!is.null(hot$data$u2_is)) hot$data$u_is
             else if (!is.null(hot$data$u2is)) hot$data$uis
             else if (!is.null(hot$data$mu2)) hot$data$mu2
             else if (!is.null(hot$data$mu2i)) hot$data$mu2i
             else if (!is.null(hot$data$mu2_i)) hot$data$mu2_i
             else if (!is.null(hot$data$mu2_is)) hot$data$mu2_is
             else hot$data$mu2is,
             sd2i=sd2i,
             n2i=n2i,
             
             # LS is the default (large-sample approximation if using SMD). UB is unbiased (only an option for measure == "SMD")
             vtype=if (input$metric4_cum == "SMD" & !input$variance_is_approximate_cum) "UB"
             else if (input$metric4_cum == "MD" & input$use_homoscedasticity_cum) "HO"
             else "LS",
             data=hot$data)
    },
    error=function(err){
      print("ERROR:  There must be at least one column each named \"m1i\", \"m2i\", \"sd1i\", \"sd2i\", \"n1i\", and \"n2i\"")
    }
    )#ends tryCatch
    removeModal()
    
  } else{
    showModal(dataModal2_cum(failed=T))
  }
  
  
  output$escalcdat_cum <- renderTable({
    if (!is.null(vals$dataescalc_cum)) {
      vals$dataescalc_cum
    }
  })
})

#################################
##         oknorm_res          ##
#################################

observeEvent(input$oknorm_cum_res, {
  conflevel <- as.numeric(as.character(input$conflevel_cum))
  cc <- as.numeric(as.character(input$cc_cum))  # continuity correction

  res <- rma(yi,
             vi,
             method=if (input$fixed_cum_norm == "RE") input$rand_cum_est else "FE",
             data=vals$dataescalc_cum,
             weighted=F,
             add=cc,
             to=input$addto_cum,
             digits=input$digits_cum,
             level=conflevel,
             slab=paste(if (!is.null(hot$data$author)) hot$data$author
                        else if (!is.null(hot$data$authors)) hot$data$authors,
                        
                        if (!is.null(hot$data$year)) hot$data$year
                        else if (!is.null(hot$data$years)) hot$data$years,
                        
                        sep=", "
             )
            )

  #####################NEEDS TO BE GENERALIZED############################
  output$forest_cum_norm <- renderPlot({
    conflevel <- as.numeric(as.character(input$conflevel_cum))
    
    forest(cumul(res, order(hot$data$year)), refline=NA, digits=input$digits_cum, level=conflevel, atransf=if (input$atransf_cum != "none") get(paste0("transf.", input$atransf_cum)))
  })

  output$msummary_cum_norm <- renderPrint({
    print(res)
  })

})


##########################
##     dynamic UI       ##
##########################

output$rand_cum_estimation <- renderUI({
  # if(input$type=="."){
  #   NULL
  # }else

  if (input$fixed_cum_norm == "FE") {                                                   ####fixed_norm in ui_meta_norm.R
    selectInput("fixed_cum_est",
                "Estimation method",
                c("Inverse-variance")
               )
  } else if (input$fixed_cum_norm == "RE"){                                             ####fixed_norm in ui_meta_norm.R
    selectInput("rand_cum_est",
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