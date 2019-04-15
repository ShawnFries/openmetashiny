#############################
##    effect_cum_norm      ##
#############################

# TODO: Split UI into 3 tabs like for server_meta_analysis (may be in UI page)
# TODO: Combine exact and normal methods in one tab (use rma.glmm for exact) and give options like for server_meta_analysis
# TODO: Fix for 2 proportions/2 means

dataModal2_cum <- function(failed=F) {
  modalDialog(
    selectInput("type_cum", "Type of data", c("Proportion", "Mean", "Two proportions (2X2)", "Two means"), switch(input$dataType,
                                                                                                                  "proportion" = "Proportion",
                                                                                                                  "mean" = "Mean",
                                                                                                                  "proportions" = "Two proportions (2X2)",
                                                                                                                  "means" = "Two means"
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
    hot$dataescalc_cum <- tryCatch({
      cum_data <- hot$data
      cum_data$xi <- if (!is.null(cum_data$count)) cumfunc(cum_data$count)
      else if (!is.null(cum_data$counts)) cumfunc(cum_data$counts)
      else if (!is.null(cum_data$xi)) cumfunc(cum_data$xi)
      else if (!is.null(cum_data$x)) cumfunc(cum_data$x)
      else if (!is.null(cum_data$x_i)) cumfunc(cum_data$x_i)
      else if (!is.null(cum_data$x_is)) cumfunc(cum_data$x_is)
      else cumfunc(cum_data$xis)
      cum_data$ni <- cumfunc(cum_data$ni)
      escalc(measure=input$metric1_cum,
             xi=cum_data$xi,
             ni=cum_data$ni,
             data=cum_data
            )
      },
      error=function(err) {
        #error handler picks up where error was generated
        print(paste("ERROR:  ", err))
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_cum == "Mean") {
    hot$dataescalc_cum <- tryCatch({
      cum_data <- hot$data
      
             cum_data$mi <- if (!is.null(cum_data$mi)) cumfunc(cum_data$mi)
             else if (!is.null(cum_data$m)) cumfunc(cum_data$m)
             else if (!is.null(cum_data$m_i)) cumfunc(cum_data$m_i)
             else if (!is.null(cum_data$m_is)) cumfunc(cum_data$m_is)
             else if (!is.null(cum_data$mis)) cumfunc(cum_data$mis)
             else if (!is.null(cum_data$u)) cumfunc(cum_data$u)
             else if (!is.null(cum_data$ui)) cumfunc(cum_data$ui)
             else if (!is.null(cum_data$u_i)) cumfunc(cum_data$u_i)
             else if (!is.null(cum_data$u_is)) cumfunc(cum_data$u_is)
             else if (!is.null(cum_data$uis)) cumfunc(cum_data$uis)
             else if (!is.null(cum_data$mu)) cumfunc(cum_data$mu)
             else if (!is.null(cum_data$mui)) cumfunc(cum_data$mui)
             else if (!is.null(cum_data$mu_i)) cumfunc(cum_data$mu_i)
             else if (!is.null(cum_data$mu_is)) cumfunc(cum_data$mu_is)
             else cumfunc(cum_data$muis)
             
             cum_data$sdi <- if (!is.null(cum_data$sdi)) cumfunc(cum_data$sdi)
             else if (!is.null(cum_data$sd)) cumfunc(cum_data$sd)
             else if (!is.null(cum_data$sd_i)) cumfunc(cum_data$sd_i)
             else if (!is.null(cum_data$sd_is)) cumfunc(cum_data$sd_is)
             else if (!is.null(cum_data$sdis)) cumfunc(cum_data$sdis)
             else if (!is.null(cum_data$sigma)) cumfunc(cum_data$sigma)
             else if (!is.null(cum_data$sigmai)) cumfunc(cum_data$sigmai)
             else if (!is.null(cum_data$sigma_i)) cumfunc(cum_data$sigma_i)
             else if (!is.null(cum_data$sigma_is)) cumfunc(cum_data$sigma_is)
             else cumfunc(cum_data$sigmais)
             
             cum_data$ni <- if (!is.null(cum_data$ni)) cumfunc(cum_data$ni)
             else if (!is.null(cum_data$nis)) cumfunc(cum_data$nis)
             else if (!is.null(cum_data$n_i)) cumfunc(cum_data$n_i)
             else if (!is.null(cum_data$n_is)) cumfunc(cum_data$n_is)
             else if (!is.null(cum_data$n)) cumfunc(cum_data$n)
             else cumfunc(cum_data$ns)
             escalc(measure=input$metric2_cum, mi=mi, sdi=sdi, ni=ni, data=cum_data)
    },
    error=function(err) {
      print("ERROR:  There must be at least one column each named \"mi\", \"sdi\" and \"ni\"")
    }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_cum == "Two means") {
    cum_data <- hot$data
    hot$dataescalc_cum <- tryCatch({
      escalc(measure=input$metric2_cum, m1i=cumfunc(m1i), sd1i=cumfunc(sd1i), n1i=cumfunc(n1i), m2i=cumfunc(m2i), sd2i=cumfunc(sd2i), n2i=cumfunc(n2i), data=cum_data)
      },
      error=function(err) {
        print(paste("ERROR:  ", err))
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_cum == "Two proportions (2X2)") {
    cum_data <- hot$data
    hot$dataescalc_cum <- tryCatch({
        escalc(measure=input$metric3_cum, ai=cumfunc(ai), n1i=cumfunc(n1i), ci=cumfunc(ci), n2i=cumfunc(n2i), data=cum_data)
      },
      error=function(err) {
        print(paste("ERROR:  ", err))
      }
    )#ends tryCatch
    removeModal()
  } else {
    showModal(dataModal2_cum(failed=T))
  }
  
  output$escalcdat_cum <- renderTable({
    if (!is.null(hot$dataescalc_cum)) {
      hot$dataescalc_cum
    }
  })
})

#################################
##         oknorm_res          ##
#################################

observeEvent(input$oknorm_cum_res, {
  conflevel <- as.numeric(as.character(input$conflevel_cum))
  cc <- as.numeric(as.character(input$cc_cum))  # continuity correction

  res <- if (input$fixed_cum_norm == "FE") {
    rma(yi, vi, method=input$fixed_cum_norm, data=hot$dataescalc_cum, weighted=F, add=cc, to=input$addto_cum, digits=input$digits_cum, level=conflevel)
  } else if (input$fixed_cum_norm == "RE") {
    rma(yi, vi, method=input$rand_cum_est, data=hot$dataescalc_cum, weighted=F, add=cc, to=input$addto_cum, digits=input$digits_cum, level=conflevel)
  }

  #####################NEEDS TO BE GENERALIZED############################
  output$forest_cum_norm <- renderPlot({
    conflevel <- as.numeric(as.character(input$conflevel_cum))
    
    forest(res, refline=NA, digits=input$digits_cum, level=conflevel)
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
                c(DerSimonian_Laird="DL", Maximum_likelihood="ML", Restricted_ML="REML"),
                "REML"
               )
  }
})