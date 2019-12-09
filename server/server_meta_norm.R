#############################
##       effect_norm       ##
#############################
#TODO: Support regression coefficient in forest/escalc/rma calculations! (And the other data types e.g. generic effect size, diagnostic i.e. TP/FP...)
# (Is part of backcalc)
# TODO: Truncate confidence intervals for proportions in metafor plots to 0 to 1
dataModal2 <- function(failed=F) {
  modalDialog(
    selectInput("type", "Type of data", c("One proportion", "One mean", "Event count", "Two proportions", "Two means", "Event counts", "Regression coefficient", "Cronbach α", "Generic effect size", "Raw mean difference", "Diagnostic (2x2 data)", "Sensitivity and Specificity", "Positive and Negative Predictive Value"), switch(dataType$type,
                                                                                                                                              "proportion" = "One proportion",
                                                                                                                                              "mean" = "One mean",
                                                                                                                                              "event count" = "Event count",
                                                                                                                                              "proportions" = "Two proportions",
                                                                                                                                              "means" = "Two means",
                                                                                                                                              "event counts" = "Event counts",
                                                                                                                                              "regression coefficient" = "Regression coefficient",
                                                                                                                                              "cronbach alpha" = "Cronbach α",
                                                                                                                                              "generic effect size" = "Generic effect size",
                                                                                                                                              "mean difference" = "Raw mean difference",
                                                                                                                                              "diagnostic" = "Diagnostic (2x2 data)",
                                                                                                                                              "sens and spec" = "Sensitivity and Specificity",
                                                                                                                                              "predictive value" = "Positive and Negative Predictive Value"
    )
    ),
    conditionalPanel(
      condition="input.type == 'One proportion'",
      selectInput("metric1",
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
      condition="input.type == 'One mean'",
      selectInput("metric2",
                  "Metric", 
                  c(`MN - raw mean`="MN", 
                    `MNLN - log transformed mean`="MNLN", 
                    `CVLN - log transformed coefficient of variation`="CVLN",
                    `SDLN - log transformed standard deviation`="SDLN"
                   )
                  )
    ), conditionalPanel(
      condition="input.type == 'Two proportions' || input.type == 'Diagnostic (2x2 data)' || input.type == 'Sensitivity and Specificity' || input.type == 'Positive and Negative Predictive Value'",
      selectInput("model_type",
                  "Select the model type to fit 2x2 data to",
                  c("Linear Mixed Effects"="rma",
                    "Peto's Method"="peto",
                    "Mantel-Haenszel Method"="mh"
                   )
      )
    ),
    conditionalPanel(
      condition="(input.type == 'Two proportions' || input.type == 'Diagnostic (2x2 data)' || input.type == 'Sensitivity and Specificity' || input.type == 'Positive and Negative Predictive Value') && input.model_type != 'mh' && input.model_type != 'peto'",
      selectInput("metric3",
                  "Metric", 
                  c(`RR - log risk ratio`="RR", 
                    `OR - log odds ratio`="OR",
                    `RD - risk difference`="RD",
                    `AS - arcsine square-root transformed risk difference`="AS",
                    `PETO - log odds ratio estimated with Peto's method`="PETO",
                    `PBIT - probit transformed risk difference`="PBIT",
                    `OR2DN - Transformed odds ratio for normal distributions`="OR2DN",
                    `OR2DL - Transformed odds ratio for logistic distributions`="OR2DL",
                    `PHI - phi coefficient`='PHI',
                    `YUQ - Yule's Q`="YUQ",
                    `YUY - Yule's Y`="YUY",
                    `RTET - tetrachoric correlation`="RTET"
                   )
                 )
    ), conditionalPanel(
      condition="(input.type == 'Two proportions' || input.type == 'Diagnostic (2x2 data)' || input.type == 'Sensitivity and Specificity' || input.type == 'Positive and Negative Predictive Value') && input.model_type == 'mh'",
      selectInput("metric_mh",
                  "Metric", 
                  c(`RR - relative risk`="RR", 
                    `OR - odds ratio`="OR",
                    `RD - risk difference`="RD"
                  )
      )
    ),
    conditionalPanel(
      condition="input.type == 'Two means' || input.type == 'Raw mean difference'",
      selectInput("metric4",
                  "Metric",
                  c(`MD - raw mean difference`="MD",
                    `SMD - standardized mean difference`="SMD",
                    `SMDH - standardized mean difference with heteroscedastic population variances in the two groups`="SMDH",
                    `ROM - log transformed ratio of means`="ROM"
                   )
                 )
),
#TODO: Add mean change (over time, requires correlation coefficient ri, m1i, m2i, sd1i, sd2i)
conditionalPanel(
  condition="input.type == 'Regression coefficient'",
  selectInput("metric5",
              "Metric", 
              c(`COR - raw correlation coefficient`="COR",
                `UCOR - unbiased raw correlation coefficient`="UCOR",
                `ZCOR - Fisher's r-to-z transformed correlation coefficient`="ZCOR"
              )
  )
), conditionalPanel(
  condition="input.type == 'Event count'",
  selectInput("metric_event_count",
              "Metric", 
              c(`IR - raw incidence rate`="IR",
                `IRLN - unbiased raw correlation coefficient`="IRLN",
                `IRS - Fisher's r-to-z transformed correlation coefficient`="IRS",
                `IRFT - Freeman-Tukey transformed incidence rate`="IRFT"
               )
  )
), conditionalPanel(
  condition="input.type == 'Event counts'",
  selectInput("metric_event_counts",
              "Metric", 
              c(`IRR - log incidence rate ratio`="IRR",
                `IRD - incidence rate difference`="IRD",
                `IRSD - square root transformed incidence rate difference`="IRSD"
               )
  )
), conditionalPanel(
  condition="input.type == 'Cronbach α'",
  selectInput("metric_cronbach_alpha",
              "Metric",
              c(`ARAW - raw alpha values`="ARAW",
                `AHW - transformed alpha values (modified Hakstian and Whalen)`="AHW",
                `ABT - transformed alpha values (modified Bonett)`="ABT"
               )
  )
), conditionalPanel(
  condition="input.metric4 == 'MD'",
  checkboxInput("use_homoscedasticity",
                "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
  )
),
      conditionalPanel(
        condition="input.metric4 == 'SMD' || input.metric5 == 'UCOR'",
        checkboxInput("variance_is_approximate",
                      "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used", 
                      T
                     )
      ),
    
    footer=tagList(
      modalButton("Cancel"),
      actionButton("oknorm_escalc", "OK")                                     ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_norm, {
  showModal(dataModal2())
})

observeEvent(input$oknorm_escalc, {                         ####oknorm_escalc
  if (!is.null(hot$data) & input$type == "One proportion") {
    vals$dataescalc <- tryCatch({
      escalc(
        measure=input$metric1,
        
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
  
  } else if (!is.null(hot$data) & input$type == "One mean") {
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric2,
             
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
    
  } else if(!is.null(hot$data) & input$type == "Two proportions") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3,
             ai=ai,
             n1i=n1i,
             ci=ci,
             n2i=n2i,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"ci\", \"n1i\", and \"n2i\"")
      }
    ) #ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type=="Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric4, 
             m1i=if (!is.null(hot$data$m1i)) m1i
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
             else hot$data$mu1is
             ,
             sd1i=sd1i,
             n1i=n1i,
             m2i=if (!is.null(hot$data$m2i)) m2i
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
             else hot$data$mu2is
                  ,
             sd2i=sd2i,
             n2i=n2i,
             
             # LS is the default (large-sample approximation if using SMD). UB is unbiased (only an option for measure == "SMD")
             vtype=if (input$metric4 == "SMD" & !input$variance_is_approximate) "UB"
                   else if (input$metric4 == "MD" & input$use_homoscedasticity) "HO"
                   else "LS"
                   ,
             data=hot$data)
      },
      error=function(err){
        print("ERROR:  There must be at least one column each named \"m1i\", \"m2i\", \"sd1i\", \"sd2i\", \"n1i\", and \"n2i\"")
        print(vals$dataescalc)
        print(err)
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type == "Regression coefficient") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric5,
             ri=ri,
             ni=ni,
             data=hot$data)},
      error=function(err){ 
        print("ERROR:  There must be at least one column each named \"ri\" and \"ni\"")
      }
    ) #ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Generic effect size") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      if ((!is.null(hot$data$sei))) {
        escalc(measure="GEN",
               yi=yi,
               sei=sei,
               data=hot$data)
      } else {
        escalc(measure="GEN",
               yi=yi,
               vi=vi,
               data=hot$data)
      }
      },
      error=function(err){
        print("ERROR:  There must be at least one column named \"yi\" and either \"vi\" or \"sei\"")
      })
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Raw mean difference") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      if ((!is.null(hot$data$sei))) {
        escalc(measure=input$metric4,
               yi=yi,
               sei=sei,
               data=hot$data)
      } else {
        escalc(measure=input$metric4,
               yi=yi,
               vi=vi,
               data=hot$data)
      }
    },
    error=function(err){
      print("ERROR:  There must be at least one column named \"yi\" and either \"vi\" or \"sei\"")
    })
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Diagnostic (2x2 data)") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3,
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
  } else if (!is.null(hot$data) & input$type == "Sensitivity and Specificity") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3,
             ai=sensitivity * true_positive,
             bi=(1 - specificity) * true_negative,
             ci=(1 - sensitivity) * true_positive,
             di=specificity * true_negative,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"sensitivity\", \"specificity\", \"true_positive\", and \"true_negative\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Positive and Negative Predictive Value") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3,
             ai=positive_predictive_value * measured_positive,
             bi=(1 - positive_predictive_value) * measured_positive,
             ci=(1 - negative_predictive_value) * measured_negative,
             di=negative_predictive_value * measured_negative,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"positive_predicted_value\", \"negative_predictive_value\", \"measured_positive\", and \"measured_negative\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type == "Event count") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
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
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
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
  } else if (!is.null(hot$data) & input$type == "Cronbach α") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_cronbach_alpha,
             ai=ai,
             mi=mi,
             ni=ni,
             data=hot$data)},
      error=function(err) {
        print("ERROR:  There must be at least one column each named \"ai\" (for observed alpha values), \"mi\" (for number of items/replications/parts of the measurement instrument),
              and \"ni\" (for sample size)")
      }
    )#ends tryCatch
    removeModal()
  } else {
    showModal(dataModal(failed=T))
  }
  
  output$escalcdat <- renderTable({
    if (!is.null(vals$dataescalc)) {
      vals$dataescalc
    }
  })
})


#################################
##         oknorm_res          ##
#################################

res <- eventReactive(input$oknorm_res, {
  conflevel <- as.numeric(as.character(input$conflevel))
  cc <- as.numeric(as.character(input$cc))
  print(input$fixed_norm)
  print(!(input$type %in% c("Two proportions", "Diagnostic (2x2 data)", "Sensitivity and Specificity", "Positive and Negative Predictive Value")) || input$model_type != "rma")
  
    tryCatch({
    if (!(input$type %in% c("Two proportions", "Diagnostic (2x2 data)", "Sensitivity and Specificity", "Positive and Negative Predictive Value") && input$model_type != "rma")) {
    rma(yi,
        vi,
        weights=if (!is.null(hot$data$weights)) hot$data$weights,
        method=if (input$fixed_norm == "RE") input$est else "FE",
        data=vals$dataescalc,
        add=cc,
        to=input$addto,
        level=conflevel,
        digits=input$digits
       )
    } else {
      switch(input$model_type,
             "mh"=switch(input$type, "Two proportions"=rma.mh(ai, n1i=n1i, ci=ci, n2i=n2i,
                               data=vals$dataescalc,
                               measure=input$metric_mh,
                               add=cc,
                               to=input$addto,
                               level=conflevel,
                               digits=input$digits
             ), "Diagnostic (2x2 data)"=rma.mh(ai, bi=bi, ci=ci, di=di,
                                               data=vals$dataescalc,
                                               measure=input$metric_mh,
                                               add=cc,
                                               to=input$addto,
                                               level=conflevel,
                                               digits=input$digits
             ), "Sensitivity and Specificity"
             =rma.mh(ai=sensitivity * true_positive,
                    bi=(1 - specificity) * true_negative,
                    ci=(1 - sensitivity) * true_positive,
                    di=specificity * true_negative,
                       data=vals$dataescalc,
                       measure=input$metric_mh,
                       add=cc,
                       to=input$addto,
                       level=conflevel,
                       digits=input$digits
             ), "Positive and Negative Predictive Value"=rma.mh(ai=positive_predictive_value * measured_positive,
                                                                bi=(1 - positive_predictive_value) * measured_positive,
                                                                ci=(1 - negative_predictive_value) * measured_negative,
                                                                di=negative_predictive_value * measured_negative,
                                                                data=vals$dataescalc,
                                                                measure=input$metric_mh,
                                                                add=cc,
                                                                to=input$addto,
                                                                level=conflevel,
                                                                digits=input$digits
             )),
             switch(input$type, "Two proportions"=rma.peto(ai, n1i=n1i, ci=ci, n2i=n2i,
                                                         data=vals$dataescalc,
                                                         add=cc,
                                                         to=input$addto,
                                                         level=conflevel,
                                                         digits=input$digits
             ), "Diagnostic (2x2 data)"=rma.peto(ai, bi=bi, ci=ci, di=di,
                                               data=vals$dataescalc,
                                               add=cc,
                                               to=input$addto,
                                               level=conflevel,
                                               digits=input$digits
             ), "Sensitivity and Specificity"# Last case (sensitivity and specificity)
             =rma.peto(ai=sensitivity * true_positive,
                      bi=(1 - specificity) * true_negative,
                      ci=(1 - sensitivity) * true_positive,
                      di=specificity * true_negative,
                    data=vals$dataescalc,
                    add=cc,
                    to=input$addto,
                    level=conflevel,
                    digits=input$digits
             ), "Positive and Negative Predictive Value"=rma.peto(ai=positive_predictive_value * measured_positive,
                                                                  bi=(1 - positive_predictive_value) * measured_positive,
                                                                  ci=(1 - negative_predictive_value) * measured_negative,
                                                                  di=negative_predictive_value * measured_negative,
                                                                  data=vals$dataescalc,
                                                                  add=cc,
                                                                  to=input$addto,
                                                                  level=conflevel,
                                                                  digits=input$digits
             )))
    }
    },
    error=function(err) {
      print(paste("ERROR:  ", err))
    }
    )
}
)

observeEvent(input$oknorm_res, {
# cc<-as.numeric(as.character(input$cc))
# 
# res<-if(input$fixed_norm=="FE"){
#   rma(yi, vi, method=input$fixed_norm, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }else if(input$fixed_norm=="RE"){
#   rma(yi, vi, method=input$rand_est, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }
res <- res()

#####################NEEDS TO BE GENERALIZED############################

output$forest_norm <- renderPlot({
  conflevel <- as.numeric(as.character(input$conflevel))
  #print(as.name(input$atransf))
  ##display forest plot
  par(mar=c(5,4,0,2)) # Remove whitespace above plot
  if (input$type == "One proportion" && input$metric1 == "PR") {
    forest(res, refline=NA, level=conflevel, digits=input$digits, slab=paste(if (!is.null(hot$data$author)) hot$data$author
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
                                                                          ), transf=if (input$atransf != "none") get(paste0("transf.", input$atransf)),
         # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
         alim=c(0, 1),
         clim=c(0, 1)
         #If large number of studies, make point estimates and other graph attributes larger/easier to see
        # psize=if(nrow(hot$data) > 20) 1.2 else 1, 
        # cex=if(nrow(hot$data) > 20) 0.4 else 1, 
        # efac=if(nrow(hot$data) > 20) c(0, 0.5) else 1
         
  )
  } else {
    forest(res, refline=NA, level=conflevel, digits=input$digits, slab=paste(if (!is.null(hot$data$author)) hot$data$author
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
    ), transf=if (input$atransf != "none") get(paste0("transf.", input$atransf))
    #If large number of studies, make point estimates and other graph attributes larger/easier to see
    #psize=if(nrow(hot$data) > 20) 2 else 1, 
    #cex=if(nrow(hot$data) > 20) 1.2 else 1, 
    #efac=if(nrow(hot$data) > 20) c(0, 0.2) else 1
    # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
    )
  }
 # text(-16, 26, "Study and Author", pos=4)

  })

output$msummary_norm <- renderPrint({
  if (input$type %in% c("Two proportions", "Diagnostic (2x2 data")) {
    np_first <- min(hot$data$ai)
    np_second <- min(hot$data$ci)
    if (input$type == "Two proportions") {
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
  print(res)
  print("Study weights (percent; model is fit using inverse-variance approach)")
  print(round(weights(res), 2))
  try({
    confint(res)
    print("Confidence intervals for residual heterogeneity")
    print(confint(res))
  })
})

})

#################################
##         save_fplot          ##
#################################
dataModal3 <- function(failed=F) {
  modalDialog(
    
    textInput("fplot_path", "Type a path to save your forest plot:",
                "~/plot1.png"),
    textInput("fplot_w", "Width of forest plot:", "8"),
    textInput("fplot_h", "Height of forest plot:", "6"),
    selectInput("fplot_unit", "Unit of saved plot dimensions", c(`pixels`="px", `inches`="in", "cm", "mm"), "in"),
    textInput("fplot_resolution", "Resolution of forest plot:", "210"),
    
    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot, {
  showModal(dataModal3())
})

observeEvent(input$ok_save_fplot, {
  conflevel<-as.numeric(as.character(input$conflevel))
  
  res <- res()
  
  ##save a png of the plot
  png(filename=input$fplot_path, width=as.numeric(input$fplot_w), height=as.numeric(input$fplot_h), units=input$fplot_unit, res=as.numeric(input$fplot_resolution))

  if (input$type == "One proportion" && input$metric1 == "PR") {
    forest(res, refline=NA, digits=input$digits, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
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
                                                                            ), transf=if (input$atransf != "none") get(paste0("transf.", input$atransf)),
           # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
           alim=c(0, 1),
           clim=c(0, 1)
          )
  } else {
    forest(res, refline=NA, digits=input$digits, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
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
    ), transf=if (input$atransf != "none") get(paste0("transf.", input$atransf))
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
  fixed_norm <- input$fixed_norm
  
  updateSelectInput(session,
                           "est",
                           "Estimation method (Ignored if data contains a column called 'weights', unless using Peto/MH method)",
                           
                           if (!is.null(fixed_norm) && fixed_norm == "RE") c(`DerSimonian Laird`="DL",
                                                     `Hedges`="HE",
                                                     `Hunter-Schmidt`="HS",
                                                     `Sidik-Jonkman`="SJ",
                                                     `Maximum likelihood`="ML",
                                                     `Restricted ML`="REML",
                                                     `Empirical Bayes`="EB",
                                                     `Paule-Mandel`="PM",
                                                     `Generalized Q-statistic`="GENQ"
                                                    )
                           else c(`Inverse-variance`="FE"),
                           
                           if (!is.null(fixed_norm) && fixed_norm == "RE") "REML"
                          )
})
