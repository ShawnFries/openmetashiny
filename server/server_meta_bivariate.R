#############################
##       effect_norm       ##
#############################
#TODO: Support regression coefficient in forest/escalc/rma calculations! (And the other data types e.g. generic effect size, diagnostic i.e. TP/FP...)
# (Is part of backcalc)
# TODO: Truncate confidence intervals for proportions in metafor plots to 0 to 1
dataModal2_bivariate <- function(failed=F) {
  modalDialog(
    selectInput("type_bivariate", "Type of data", c("Two proportions", "Two means", "Event counts", "Diagnostic (2x2 data)", "Sensitivity and Specificity", "Positive and Negative Predictive Value"), switch(dataType$type,
                                                                                                                                              "proportions" = "Two proportions",
                                                                                                                                              "means" = "Two means",
                                                                                                                                              "event counts" = "Event counts",
                                                                                                                                              "diagnostic" = "Diagnostic (2x2 data)",
                                                                                                                                              "sens and spec" = "Sensitivity and Specificity",
                                                                                                                                              "predictive value" = "Positive and Negative Predictive Value"
    )
    ),
    conditionalPanel(
      condition="input.type_bivariate == 'Two proportions' || input.type_bivariate == 'Diagnostic (2x2 data)' || input.type_bivariate == 'Sensitivity and Specificity' || input.type_bivariate == 'Positive and Negative Predictive Value'",
      selectInput("metric1_bivariate",
                  "Metric",
                  c(`PR - raw proportion`="PR",
                    `PLN - natural logarithm transformed proportion`="PLN",
                    `PLO - logit transformed proportion`="PLO",
                    `PAS - arcsine square root (angular) transformed proportion`="PAS",
                    `PFT - Freeman-Tukey double arcsine transformed proportion`="PFT"
                   )#,
                  #TODO: Below is incorrect, we would actually want to reverse the transformation in this case (manually back-transform then feed through metafor?)
                   #if (exists("min_proportion") && (min_proportion < 0 || max_proportion > 1)) {
                   # "PLO"
                 # }
                 )
    ),
    conditionalPanel(
      condition="input.type_bivariate == 'Two means'",
      selectInput("metric2_bivariate",
                  "Metric", 
                  c(`MN - raw mean`="MN", 
                    `MNLN - log transformed mean`="MNLN", 
                    `CVLN - log transformed coefficient of variation`="CVLN",
                    `SDLN - log transformed standard deviation`="SDLN"
                   )
                  )
    ), conditionalPanel(
  condition="input.type_bivariate == 'Event counts'",
  selectInput("metric_event_counts_bivariate",
              "Metric", 
              c(`IR - raw incidence rate`="IR",
                `IRLN - unbiased raw correlation coefficient`="IRLN",
                `IRS - Fisher's r-to-z transformed correlation coefficient`="IRS",
                `IRFT - Freeman-Tukey transformed incidence rate`="IRFT"
               )
  )
),
    
    footer=tagList(
      modalButton("Cancel"),
      actionButton("oknorm_escalc_bivariate", "OK")                                     ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_norm_bivariate, {
  showModal(dataModal2_bivariate())
})

observeEvent(input$oknorm_escalc_bivariate, {                         ####oknorm_escalc
  if (!is.null(hot$data) & input$type_bivariate == "Two proportions") {
    vals$dataescalc_bivariate <- tryCatch({
      group_1_variances <- escalc(measure=input$metric1_bivariate,
             xi=ai,
             ni=n1i,
             data=hot$data)
      group_1_variances$group_ <- 'group_1'
      group_2_variances <- escalc(measure=input$metric1_bivariate,
                  xi=ci,
                  ni=n2i,
                  data=hot$data)
      group_2_variances$group_ <- 'group_2'
      rbind(group_1_variances, group_2_variances)},
      error=function(err) {
        #error handler picks up where error was generated
        print(paste("ERROR:  There must be at least one column named \"count\" or \"xi\" and one named \"ni\""))
      }
    )#ends tryCatch
    removeModal()
  
  } else if (!is.null(hot$data) & input$type_bivariate == "Two means") {
    vals$dataescalc_bivariate <- tryCatch({
      group_1_variances <- escalc(measure=input$metric2_bivariate,
                                  mi=m1i,
                                  sdi=sd1i,
                                  ni=n1i,
                                  data=hot$data)
      group_1_variances$group_ <- 'mean_1'
      group_2_variances <- escalc(measure=input$metric2_bivariate,
                                  mi=m2i,
                                  sdi=sd2i,
                                  ni=n2i,
                                  data=hot$data)
      group_2_variances$group_ <- 'mean_2'
      rbind(group_1_variances, group_2_variances)},
      error=function(err) {
        print("ERROR:  There must be at least one column each named \"mi\", \"sdi\" and \"ni\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_bivariate == "Diagnostic (2x2 data)") {
    vals$dataescalc_bivariate <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      group_1_variances <- escalc(measure=input$metric1_bivariate,
                                  xi=ai,
                                  mi=bi,  # In this case mi means the individuals who do not experience the event of interest
                                  data=hot$data)
      group_1_variances$group_ <- 'group_1'
      group_2_variances <- escalc(measure=input$metric1_bivariate,
                                  xi=ci,
                                  mi=di,
                                  data=hot$data)
      group_2_variances$group_ <- 'group_2'
      rbind(group_1_variances, group_2_variances)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"bi\", \"ci\", and \"di\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type_bivariate == "Sensitivity and Specificity") {
    vals$dataescalc_bivariate <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      group_1_variances <- escalc(measure=input$metric1_bivariate,
                                  xi=sensitivity * true_positive,
                                  mi=(1 - sensitivity) * true_positive,
                                  data=hot$data)
      group_1_variances$group_ <- 'sensitivity'
      group_2_variances <- escalc(measure=input$metric1_bivariate,
                                  mi=(1 - specificity) * true_negative,
                                  xi=specificity * true_negative,
                                  data=hot$data)
      group_2_variances$group_ <- 'specificity'
      rbind(group_1_variances, group_2_variances)},
      error=function(err){
        print(err)
        print("ERROR:  There must be at least one column each named \"sensitivity\", \"specificity\", \"true_positive\", and \"true_negative\"")
      }
    )#ends tryCatch
    removeModal()
    #TODO: fix this...
  } else if (!is.null(hot$data) & input$type_bivariate == "Positive and Negative Predictive Value") {
    vals$dataescalc_bivariate <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      group_1_variances <- escalc(measure=input$metric1_bivariate,
                                  xi=positive_predictive_value * measured_positive,
                                  mi=(1 - positive_predictive_value) * measured_positive,
                                  data=hot$data)
      group_1_variances$group_ <- 'positive_predictive_value'
      group_2_variances <- escalc(measure=input$metric1_bivariate,
                                  mi=(1 - negative_predictive_value) * measured_negative,
                                  xi=negative_predictive_value * measured_negative,
                                  data=hot$data)
      group_2_variances$group_ <- 'negative_predictive_value'
      rbind(group_1_variances, group_2_variances)},
      error=function(err){
        print(err)
        print("ERROR:  There must be at least one column each named \"positive_predicted_value\", \"negative_predictive_value\", \"measured_positive\", and \"measured_negative\"")
      }
    )#ends tryCatch
    removeModal()
    #TODO: fix this...
  } else if (!is.null(hot$data) & input$type == "Event count") {
    vals$dataescalc_bivariate <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_event_count,
             xi=xi,
             ti=ti,
             data=hot$data)},
      error=function(err) {
        print("ERROR:  There must be at least one column each named \"xi\" and \"ti\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Event counts") {
    vals$dataescalc_bivariate <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_event_counts,
             x1i=x1i,
             t1i=t1i,
             x2i=x2i,
             t2i=t2i,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"x1i\", \"t1i\", \"x2i\", and \"t2i\"")
      }
    )#ends tryCatch
    removeModal()
  }
  
  output$escalcdat_bivariate <- renderTable({
    if (!is.null(vals$dataescalc_bivariate)) {
      vals$dataescalc_bivariate
    }
  })
})


#################################
##         oknorm_res          ##
#################################

res_bivariate <- eventReactive(input$oknorm_res_bivariate, {
  conflevel <- as.numeric(as.character(input$conflevel_bivariate))
  cc <- as.numeric(as.character(input$cc_bivariate))
  
    tryCatch({
    rma.mv(yi,
        vi,
        mods = ~ group_ - 1,
        random = ~ group_ | study,
        method=input$est_bivariate,
        data=vals$dataescalc_bivariate,
        level=conflevel,
        struct="UN",  # To allow correlation among the 2 variables
        digits=input$digits_bivariate
       )
    },
    error=function(err) {
      print(paste("ERROR:  ", err))
    }
    )
}
)

observeEvent(input$oknorm_res_bivariate, {
# cc<-as.numeric(as.character(input$cc))
# 
# res<-if(input$fixed_norm=="FE"){
#   rma(yi, vi, method=input$fixed_norm, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }else if(input$fixed_norm=="RE"){
#   rma(yi, vi, method=input$rand_est, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }
res_bivariate <- res_bivariate()

#####################NEEDS TO BE GENERALIZED############################

output$forest_norm_bivariate <- renderPlot({
  conflevel <- as.numeric(as.character(input$conflevel_bivariate))
  #print(as.name(input$atransf))
  ##display forest plot
  if (input$type_bivariate == "Two proportions" && input$metric1_bivariate == "PR") {
    forest(res_bivariate, refline=NA, level=conflevel, digits=input$digits_bivariate, mlab=vals$dataescalc_bivariate$specificity, slab=paste(if (!is.null(vals$dataescalc_bivariate$author)) vals$dataescalc_bivariate$author
                                                                           else if (!is.null(vals$dataescalc_bivariate$authors)) vals$dataescalc_bivariate$authors
                                                                           else if (!is.null(vals$dataescalc_bivariate$study.name)) vals$dataescalc_bivariate$study.name
                                                                           else if (!is.null(vals$dataescalc_bivariate$study.names)) vals$dataescalc_bivariate$study.names
                                                                           else if (!is.null(res_bivariate[["study name"]])) res_bivariate[["study name"]]
                                                                           else if (!is.null(res_bivariate[["study names"]])) res_bivariate[["study names"]]
                                                                           else if (!is.null(vals$dataescalc_bivariate$study)) vals$dataescalc_bivariate$study
                                                                           else if (!is.null(vals$dataescalc_bivariate$studies)) vals$dataescalc_bivariate$studies
                                                                           else "NA",
                                                                           if (!is.null(vals$dataescalc_bivariate$year)) vals$dataescalc_bivariate$year
                                                                           else if (!is.null(vals$dataescalc_bivariate$years)) vals$dataescalc_bivariate$years
                                                                           else "NA",
                                                                           vals$dataescalc_bivariate$group_,
                                                                           sep=", "
                                                                          ), transf=if (input$atransf_bivariate != "none") get(paste0("transf.", input$atransf_bivariate)),
         # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
         alim=c(0, 1),
         clim=c(0, 1)
  )
  } else {
    forest(res_bivariate, refline=NA, level=conflevel, digits=input$digits_bivariate, slab=paste(if (!is.null(vals$dataescalc_bivariate$author)) vals$dataescalc_bivariate$author
                                                                             else if (!is.null(vals$dataescalc_bivariate$authors)) vals$dataescalc_bivariate$authors
                                                                             else if (!is.null(vals$dataescalc_bivariate$study.name)) vals$dataescalc_bivariate$study.name
                                                                             else if (!is.null(vals$dataescalc_bivariate$study.names)) vals$dataescalc_bivariate$study.names
                                                                             else if (!is.null(res_bivariate[["study name"]])) res_bivariate[["study name"]]
                                                                             else if (!is.null(res_bivariate[["study names"]])) res_bivariate[["study names"]]
                                                                             else if (!is.null(vals$dataescalc_bivariate$study)) vals$dataescalc_bivariate$study
                                                                             else if (!is.null(vals$dataescalc_bivariate$studies)) vals$dataescalc_bivariate$studies
                                                                             else "NA",
                                                                             if (!is.null(vals$dataescalc_bivariate$year)) vals$dataescalc_bivariate$year
                                                                             else if (!is.null(vals$dataescalc_bivariate$years)) vals$dataescalc_bivariate$years
                                                                             else "NA",
                                                                             vals$dataescalc_bivariate$group_,
                                                                             sep=", "
    ), transf=if (input$atransf_bivariate != "none") get(paste0("transf.", input$atransf_bivariate))
    # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
    )
  }
  text(-16, 26, "Study and Author", pos=4)

  })

output$msummary_norm_bivariate <- renderPrint({
  if (input$type_bivariate %in% c("Two proportions", "Diagnostic (2x2 data")) {
    np_first <- min(hot$data$ai)
    np_second <- min(hot$data$ci)
    if (input$type_bivariate == "Two proportions") {
      binomial_variance_first <- min(hot$data$ai * (1 - hot$data$ai / hot$data$n1i))
      binomial_variance_second <- min(hot$data$ci * (1 - hot$data$ci / hot$data$n2i))
    } else {
      binomial_variance_first <- min(hot$data$ai * (1 - hot$data$ai / (hot$data$ai + hot$data$bi)))
      binomial_variance_second <- min(hot$data$ci * (1 - hot$data$ci / (hot$data$ci + hot$data$di)))
    }
    if (np_first < 10 || np_second < 10 || binomial_variance_first < 10 || binomial_variance_second < 10) {
      print(
        'Caution: At least one of your studies uses a very small sample size and/or a proportion close to 0 or 1. We recommend using the dropdown above to select "exact likelihood" instead.'
      )
    } 
  }
  print(res_bivariate)
  print("Study weights (percent; model is fit using inverse-variance approach)")
  print(round(weights(res_bivariate), 2))
  print("Confidence intervals for residual heterogeneity")
  print(confint(res_bivariate))
})

})

#################################
##         save_fplot          ##
#################################
dataModal3_bivariate <- function(failed=F) {
  modalDialog(
    
    textInput("fplot_path_bivariate", "Type a path to save your forest plot:",
                "~/plot1.png"),
    textInput("fplot_w_bivariate", "Width of forest plot:", "8"),
    textInput("fplot_h_bivariate", "Height of forest plot:", "6"),
    selectInput("fplot_unit_bivariate", "Unit of saved plot dimensions", c(`pixels`="px", `inches`="in", "cm", "mm"), "in"),
    textInput("fplot_resolution_bivariate", "Resolution of forest plot:", "210"),
    
    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot_bivariate", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot_bivariate, {
  showModal(dataModal3_bivariate())
})

observeEvent(input$ok_save_fplot_bivariate, {
  conflevel<-as.numeric(as.character(input$conflevel_bivariate))
  
  res_bivariate <- res_bivariate()
  
  ##save a png of the plot
  png(filename=input$fplot_path_bivariate, width=as.numeric(input$fplot_w_bivariate), height=as.numeric(input$fplot_h_bivariate), units=input$fplot_unit_bivariate, res=as.numeric(input$fplot_resolution_bivariate))

  if (input$type_bivariate == "One proportion" && input$metric1_bivariate == "PR") {
    forest(res_bivariate, refline=NA, digits=input$digits_bivariate, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                             else if (!is.null(hot$data$authors)) hot$data$authors
                                                                             else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                             else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                             else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                             else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                             else if (!is.null(hot$data$study)) hot$data$study
                                                                             else if (!is.null(hot$data$studies)) hot$data$studies
                                                                             else "NA",
                                                                             if (!is.null(hot$data$year)) hot$data$year
                                                                             else if (!is.null(hot$data$years)) hot$data$years
                                                                             else "NA",
                                                                             sep=", "
                                                                            ), transf=if (input$atransf_bivariate != "none") get(paste0("transf.", input$atransf_bivariate)),
           # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
           alim=c(0, 1),
           clim=c(0, 1)
          )
  } else {
    forest(res_bivariate, refline=NA, digits=input$digits_bivariate, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                             else if (!is.null(hot$data$authors)) hot$data$authors
                                                                             else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                             else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                             else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                             else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                             else if (!is.null(hot$data$study)) hot$data$study
                                                                             else if (!is.null(hot$data$studies)) hot$data$studies
                                                                             else "NA",
                                                                             if (!is.null(hot$data$year)) hot$data$year
                                                                             else if (!is.null(hot$data$years)) hot$data$years
                                                                             else "NA",
                                                                             
                                                                             sep=", "
    ), transf=if (input$atransf_bivariate != "none") get(paste0("transf.", input$atransf_bivariate))
    )
  }
  # Doesn't seem to be appearing? troubleshoot..
  # TODO: Fix below line so label appears
  text(-16, 26, "Study and Author", pos=4)
  dev.off()
  
  removeModal()
})


##########################
##     dynamic UI       ##
##########################
observe({
  fixed_norm <- input$fixed_norm_bivariate
  
  updateSelectInput(session,
                           "est_bivariate",
                           "Estimation method (Ignored if data contains a column called 'weights', unless using Peto/MH method)",
                           
                           if (!is.null(fixed_norm) && fixed_norm == "RE") c(
                                                     `Maximum likelihood`="ML",
                                                     `Restricted ML`="REML"
                                                    )
                           else c(`Inverse-variance`="FE"),
                           
                           if (!is.null(fixed_norm) && fixed_norm == "RE") "REML"
                          )
})
