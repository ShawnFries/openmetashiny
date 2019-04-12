#############################
##    effect_cum_norm      ##
#############################

dataModal2_cum <- function(failed=F) {
  modalDialog(
    selectInput("type_cum", "Type of data", c("Proportion", "Mean", "Two proportions (2X2)"), switch(input$dataType,
                                                                                                     "proportion" = "Proportion",
                                                                                                     "mean" = "Mean",
                                                                                                     "proportions" = "Two proportions (2X2)"#,
                                                                                                     #"means" = "Two means"
                                                                                                    )
               ),
    conditionalPanel(
      condition="input.type_cum == 'Proportion'",
      selectInput("metric1_cum", "Metric", c(raw_proportion="PR", arcsine="PAS", logit="PLO"))
    ),
    conditionalPanel(
      condition="input.type_cum == 'Mean'",
      selectInput("metric2_cum", "Metric", c("MD", "SMD", "SMDH", "ROM"))
    ),
    conditionalPanel(
      condition="input.type_cum == 'Two proportions (2X2)'",
      selectInput("metric3_cum", "Metric", c("RR", "OR", "RD", "AS", "PETO"))
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
      escalc(measure=input$metric2_cum, 
             m1i=hot$data$m1i, sd1i=hot$data$sd1i, n1i=hot$data$n1i,
             m2i=hot$data$m2i, sd2i=hot$data$sd2i, n2i=hot$data$n2i, 
             data=hot$data)
      },
      error=function(err) {
        print(paste("ERROR:  ", err))
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_cum == "Two proportions (2X2)") {
    hot$dataescalc_cum <- tryCatch({
      escalc(measure=input$metric3_cum, ai=hot$data$ai, n1i=hot$data$n1i, ci=hot$data$ci, n2i=hot$data$n2i, data=hot$data)
      },
      error=function(err) {
        print(paste("ERROR:  ", err))
      }
    )#ends tryCatch
    removeModal()
  } else {
    showModal(dataModal(failed=T))
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
    if (input$metric1_cum == "PLO") {
      forest(res, transf=transf.ilogit, targs=list(ni=cum_data$ni), refline=NA, digits=input$digits_cum, level=conflevel)
    } else if (input$metric1_cum == "PAS") {
      forest(res, transf=transf.isqrt, targs=list(ni=cum_data$ni), refline=NA, digits=input$digits_cum, level=conflevel)
    } else if (input$metric1_cum == "PR") {
      forest(res, refline=NA, digits=input$digits_cum, level=conflevel)
    }
  })

  output$msummary_cum_norm <- renderPrint({
    print(res)
  })

})


##########################
##     dynamic UI       ##
##########################

output$rand_estimation <- renderUI({
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
                c(DerSimonian_Laird="DL", Maximum_likelihood="ML", Restricted_ML="REML")
               )
  }
})