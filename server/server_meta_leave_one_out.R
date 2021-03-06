#############################
##    effect_leave_norm      ##
#############################

# TODO: Split UI into 3 tabs like for server_meta_analysis (may be in UI page)
# TODO: Combine exact and normal methods in one tab (use rma.glmm for exact) and give options like for server_meta_analysis
# TODO: Fix for 2 proportions/2 means

dataModal2_leave <- function(failed=F) {
  modalDialog(
    selectInput("type_leave", "Type of data", c("Proportion", "Mean", "Two proportions (2X2)", "Two means"), switch(input$dataType,
                                                                                                                  "proportion" = "Proportion",
                                                                                                                  "mean" = "Mean",
                                                                                                                  "proportions" = "Two proportions (2X2)",
                                                                                                                  "means" = "Two means"
    )
    ),
    conditionalPanel(
      condition="input.type_leave == 'Proportion'",
      selectInput("metric1_leave", "Metric", c(raw_proportion="PR", arcsine="PAS", logit="PLO"))
    ),
    conditionalPanel(
      condition="input.type_leave == 'Mean'",
      selectInput("metric2_leave", "Metric", c(`MN - raw mean`="MN", 
                                             `MNLN - log transformed mean`="MNLN", 
                                             `CVLN - log transformed coefficient of variation`="CVLN",
                                             `SDLN - log transformed standard deviation`="SDLN"
      )
      )
    ),
    conditionalPanel(
      condition="input.type_leave == 'Two proportions (2X2)'",
      selectInput("metric3_leave", "Metric", c("RR", "OR", "RD", "AS", "PETO"))
    ),
    conditionalPanel(
      condition="input.type_leave == 'Two means'",
      selectInput("metric4_leave", "Metric", c("MD", "SMD", "SMDH", "ROM"))
    ),
    footer=tagList(
      modalButton("Cancel"),
      actionButton("oknorm_leave_escalc", "OK")                                             ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_leave_norm, {
  showModal(dataModal2_leave())
})


observeEvent(input$oknorm_leave_escalc, {                                                              ####oknorm_leave_escalc
  if (!is.null(hot$data) & input$type_leave == "Proportion") {
    vals$dataescalc_leave <- tryCatch({
      escalc(
        measure=input$metric1_leave,
        
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
    
  } else if (!is.null(hot$data) & input$type_leave == "Mean") {
    vals$dataescalc_leave <- tryCatch({
      escalc(measure=input$metric2_leave,
             
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
    
  } else if(!is.null(hot$data) & input$type_leave == "Two proportions (2X2)") {
    vals$dataescalc_leave <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3_leave,
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
    
  } else if (!is.null(hot$data) & input$type_leave=="Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc_leave <- tryCatch({
      escalc(measure=input$metric4_leave, 
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
             vtype=if (input$metric4_leave == "SMD" & !input$variance_is_approximate_leave) "UB"
             else if (input$metric4_leave == "MD" & input$use_homoscedasticity_leave) "HO"
             else "LS",
             data=hot$data)
    },
    error=function(err){
      print("ERROR:  There must be at least one column each named \"m1i\", \"m2i\", \"sd1i\", \"sd2i\", \"n1i\", and \"n2i\"")
    }
    )#ends tryCatch
    removeModal()
    
  } else{
    showModal(dataModal(failed=T))
  }
  
  
  output$escalcdat_leave <- renderTable({
    if (!is.null(vals$dataescalc_leave)) {
      vals$dataescalc_leave
    }
  })
})

#################################
##         oknorm_res          ##
#################################

observeEvent(input$oknorm_leave_res, {
  conflevel <- as.numeric(as.character(input$conflevel_leave))
  cc <- as.numeric(as.character(input$cc_leave))  # continuity correction
  
  res <- rma(yi,
             vi,
             method=if (input$fixed_leave_norm == "RE") input$rand_leave_est else "FE",
             data=vals$dataescalc_leave,
             add=cc,
             to=input$addto_leave,
             digits=input$digits_leave,
             level=conflevel,
             slab=paste(if (!is.null(hot$data$author)) hot$data$author
                        else if (!is.null(hot$data$authors)) hot$data$authors,
                        
                        if (!is.null(hot$data$year)) hot$data$year
                        else if (!is.null(hot$data$years)) hot$data$years,
                        
                        sep=", "
             )
  )
  
  #####################NEEDS TO BE GENERALIZED############################
  output$plot_leave_norm <- renderPlot({
    plot(influence(res))
  })
  
  output$msummary_leave_norm <- renderPrint({
    print(leave1out(res))
  })
  
})


##########################
##     dynamic UI       ##
##########################

output$rand_leave_estimation <- renderUI({
  # if(input$type=="."){
  #   NULL
  # }else
  
  if (!is.null(input$fixed_leave_norm) && input$fixed_leave_norm == "FE") {                                                   ####fixed_norm in ui_meta_norm.R
    selectInput("fixed_leave_est",
                "Estimation method",
                c("Inverse-variance")
    )
  } else if (!is.null(input$fixed_leave_norm) && input$fixed_leave_norm == "RE"){                                             ####fixed_norm in ui_meta_norm.R
    selectInput("rand_leave_est",
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