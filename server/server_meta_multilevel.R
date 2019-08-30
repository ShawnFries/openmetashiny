#############################
##       effect_norm       ##
#############################
#TODO: Support regression coefficient in forest/escalc/rma calculations! (And the other data types e.g. generic effect size, diagnostic i.e. TP/FP...)
# (Is part of backcalc)
# TODO: Truncate confidence intervals for proportions in metafor plots to 0 to 1
dataModal2_multilevel <- function(failed=F) {
  modalDialog(
    selectInput("type_multilevel", "Type of data", c("One proportion", "One mean", "Event count", "Two proportions", "Two means", "Event counts", "Regression coefficient", "Cronbach α", "Generic effect size", "Raw mean difference", "Diagnostic"), switch(dataType$type,
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
                                                                                                                                              "diagnostic" = "Diagnostic"
    )
    ),
    selectInput("grouping_variables_random_effects", "Outer hierarchical variables - Random effects within each group, groups are assumed independent)", colnames(hot$data), multiple=T),
    selectizeInput("inner_variables_random_effects",
                   "Inner hierarchical variables - Correlated random effects within each above group depending on value of inner variable (applied in same order, first here to first above etc)",
                   colnames(hot$data), multiple=T, options=list(maxItems=2)
                   #TODO: Need to have option to select same option twice (e.g. CS, CS)
                  ),
    #TODO: Add correlation matrix? (argument R in rma.mv, e.g. for phylogenetic meta-analysis; see package shinyMatrix)
    selectizeInput("inner_variance_structure",
                "Inner variable variance structure (set 2 values in order if using 2 inner variables above)",
                c("Compound symmetry"="CS",
                  "Heteroscedastic compound symmetry"="HCS",
                  "Unstructured variance-covariance matrix"="UN",
                  "Identity (compound symmetry with ρ = 0)"="ID",
                  "Diagonal (heteroscedastic compound symmetry with ρ = 0)"="DIAG",
                  "Autoregressive (autoregression among random effects, e.g. for time series data set inner variable to points in time)"="AR",
                  "Heteroscedastic autoregressive structure"="HAR"
                 ),
                multiple=T,
                options=list(maxItems=2)
               ),
    # Phylogenetic meta-analysis; really probably need to pass in species correlation matrices? Then modify other argument... check rma.mv documentation
   # checkboxInput("is_phylogenetic", "Perform a phylogenetic meta-analysis? (i.e. force the correlation of effects ", colnames(hot$data), multiple=T),
    conditionalPanel(
      condition="input.type_multilevel == 'One proportion'",
      selectInput("metric1_multilevel",
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
      condition="input.type_multilevel == 'One mean'",
      selectInput("metric2_multilevel",
                  "Metric", 
                  c(`MN - raw mean`="MN", 
                    `MNLN - log transformed mean`="MNLN", 
                    `CVLN - log transformed coefficient of variation`="CVLN",
                    `SDLN - log transformed standard deviation`="SDLN"
                   )
                  )
    ),
    conditionalPanel(
      condition="input.type_multilevel == 'Two proportions' || input.type_multilevel == 'Diagnostic'",
      selectInput("metric3_multilevel",
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
    ),
    conditionalPanel(
      condition="input.type_multilevel == 'Two means' || input.type_multilevel == 'Raw mean difference'",
      selectInput("metric4_multilevel",
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
  condition="input.type_multilevel == 'Regression coefficient'",
  selectInput("metric5_multilevel",
              "Metric", 
              c(`COR - raw correlation coefficient`="COR",
                `UCOR - unbiased raw correlation coefficient`="UCOR",
                `ZCOR - Fisher's r-to-z transformed correlation coefficient`="ZCOR"
              )
  )
), conditionalPanel(
  condition="input.type_multilevel == 'Event count'",
  selectInput("metric_event_count_multilevel",
              "Metric", 
              c(`IR - raw incidence rate`="IR",
                `IRLN - unbiased raw correlation coefficient`="IRLN",
                `IRS - Fisher's r-to-z transformed correlation coefficient`="IRS",
                `IRFT - Freeman-Tukey transformed incidence rate`="IRFT"
               )
  )
), conditionalPanel(
  condition="input.type_multilevel == 'Event counts'",
  selectInput("metric_event_counts_multilevel",
              "Metric", 
              c(`IRR - log incidence rate ratio`="IRR",
                `IRD - incidence rate difference`="IRD",
                `IRSD - square root transformed incidence rate difference`="IRSD"
               )
  )
), conditionalPanel(
  condition="input.type_multilevel == 'Cronbach α'",
  selectInput("metric_cronbach_alpha_multilevel",
              "Metric",
              c(`ARAW - raw alpha values`="ARAW",
                `AHW - transformed alpha values (modified Hakstian and Whalen)`="AHW",
                `ABT - transformed alpha values (modified Bonett)`="ABT"
               )
  )
), conditionalPanel(
  condition="input.metric4_multilevel == 'MD'",
  checkboxInput("use_homoscedasticity_multilevel",
                "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
  )
),
      conditionalPanel(
        condition="input.metric4_multilevel == 'SMD' || input.metric5_multilevel == 'UCOR'",
        checkboxInput("variance_is_approximate_multilevel",
                      "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used", 
                      T
                     )
      ),
    
    footer=tagList(
      modalButton("Cancel"),
      actionButton("okmultilevel_escalc", "OK")                                     ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_multilevel, {
  showModal(dataModal2_multilevel())
})

observeEvent(input$okmultilevel_escalc, {                         ####oknorm_escalc
  if (!is.null(hot$data) & input$type_multilevel == "One proportion") {
    vals$dataescalc <- tryCatch({
      escalc(
        measure=input$metric1_multilevel,
        
        xi=if (!is.null(hot$data$count)) count
        else if (!is.null(hot$data$counts)) counts
        else if (!is.null(hot$data$xi)) xi
        else if (!is.null(hot$data$x)) x
        else if (!is.null(hot$data$x_i)) x_i
        else if (!is.null(hot$data$x_is)) x_is
        else xis,
        add=as.numeric(as.character(input$cc_multilevel)),
        to=input$addto_multilevel,
        
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
  
  } else if (!is.null(hot$data) & input$type_multilevel == "One mean") {
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric2_multilevel,
             
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
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             
             data=hot$data
             )
      },
      error=function(err) {
        print("ERROR:  There must be at least one column each named \"mi\", \"sdi\" and \"ni\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if(!is.null(hot$data) & input$type_multilevel == "Two proportions") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3_multilevel,
             ai=ai,
             n1i=n1i,
             ci=ci,
             n2i=n2i,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"ci\", \"n1i\", and \"n2i\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_multilevel == "Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric4_multilevel, 
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
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             
             # LS is the default (large-sample approximation if using SMD). UB is unbiased (only an option for measure == "SMD")
             vtype=if (input$metric4_multilevel == "SMD" & !input$variance_is_approximate_multilevel) "UB"
                   else if (input$metric4_multilevel == "MD" & input$use_homoscedasticity_multilevel) "HO"
                   else "LS",
             data=hot$data)
      },
      error=function(err){ 
        print("ERROR:  There must be at least one column each named \"m1i\", \"m2i\", \"sd1i\", \"sd2i\", \"n1i\", and \"n2i\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_multilevel == "Regression coefficient") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric5_multilevel,
             ri=ri,
             ni=ni,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             data=hot$data)},
      error=function(err){ 
        print("ERROR:  There must be at least one column each named \"ri\" and \"ni\"")
      }
    ) #ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Generic effect size") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      if ((!is.null(hot$data$sei))) {
        escalc(measure="GEN",
               yi=yi,
               sei=sei,
               add=as.numeric(as.character(input$cc_multilevel)),
               to=input$addto_multilevel,
               data=hot$data)
      } else {
        escalc(measure="GEN",
               yi=yi,
               vi=vi,
               add=as.numeric(as.character(input$cc_multilevel)),
               to=input$addto_multilevel,
               data=hot$data)
      }
      },
      error=function(err){
        print("ERROR:  There must be at least one column named \"yi\" and either \"vi\" or \"sei\"")
      })
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Raw mean difference") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      if ((!is.null(hot$data$sei))) {
        escalc(measure=input$metric4_multilevel,
               yi=yi,
               sei=sei,
               add=as.numeric(as.character(input$cc_multilevel)),
               to=input$addto_multilevel,
               data=hot$data)
      } else {
        escalc(measure=input$metric4_multilevel,
               yi=yi,
               vi=vi,
               add=as.numeric(as.character(input$cc_multilevel)),
               to=input$addto_multilevel,
               data=hot$data)
      }
    },
    error=function(err){
      print("ERROR:  There must be at least one column named \"yi\" and either \"vi\" or \"sei\"")
    })
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Diagnostic") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3_multilevel,
             ai=ai,
             bi=bi,
             ci=ci,
             di=di,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"bi\", \"ci\", and \"di\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Event count") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_event_count_multilevel,
             xi=xi,
             ti=ti,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"xi\" and \"ti\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Event counts") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_event_counts_multilevel,
             x1i=x1i,
             t1i=t1i,
             x2i=x2i,
             t2i=t2i,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"x1i\", \"t1i\", \"x2i\", and \"t2i\"")
      }
    )#ends tryCatch
    removeModal()
  } else if (!is.null(hot$data) & input$type_multilevel == "Cronbach α") {
    vals$dataescalc <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric_cronbach_alpha_multilevel,
             ai=ai,
             mi=mi,
             ti=ti,
             add=as.numeric(as.character(input$cc_multilevel)),
             to=input$addto_multilevel,
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
  
  output$escalcdat_multilevel <- renderTable({
    if (!is.null(vals$dataescalc)) {
      vals$dataescalc
    }
  })
})


#################################
##         oknorm_res          ##
#################################

res_multilevel <- eventReactive(input$okmultilevel_res, {
  conflevel <- as.numeric(as.character(input$conflevel_multilevel))
  cc <- as.numeric(as.character(input$cc_multilevel))
  inner_variable_length <- length(input$inner_variables_random_effects)
  
    tryCatch({
    rma.mv(yi,
        vi,
        W=if (!is.null(hot$data$weights)) hot$data$weights,
        method=if (input$fixed_multilevel == "RE") input$est_multilevel,
        data=vals$dataescalc,
        random=if (!is.null(input$inner_variables_random_effects) && !is.null(input$grouping_variables_random_effects)) {
                #print(2)
                 inner_variable_length <- length(input$inner_variables_random_effects)
                 #print(inner_variable_length)
                 # Don't recycle length of inner variables, just use them once then do no correlated inner effect for any excess outer grouping variables in order
                 mapply(function(x, y, z) reformulate(paste(ifelse(z > inner_variable_length, "1", y), "|", x)),
                        input$grouping_variables_random_effects,
                        input$inner_variables_random_effects,
                        seq_along(input$grouping_variables_random_effects),
                        SIMPLIFY=F
                       )
               } else if (!is.null(input$grouping_variables_random_effects)) lapply(input$grouping_variables_random_effects, function(x) reformulate(paste("1 |", x)))
               ,
        struct=ifelse(is.null(input$inner_variance_structure), "CS", input$inner_variance_structure),
        level=conflevel,
        test="t",
        digits=input$digits_multilevel
       )
    },
    error=function(err) {
      print(paste("ERROR:  ", err))
    }
    )
}
)

observeEvent(input$okmultilevel_res, {
# cc<-as.numeric(as.character(input$cc))
# 
# res<-if(input$fixed_norm=="FE"){
#   rma(yi, vi, method=input$fixed_norm, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }else if(input$fixed_norm=="RE"){
#   rma(yi, vi, method=input$rand_est, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }
res_multilevel <- res_multilevel()

#####################NEEDS TO BE GENERALIZED############################

output$forest_multilevel <- renderPlot({
  conflevel <- as.numeric(as.character(input$conflevel))
  #print(as.name(input$atransf))
  ##display forest plot
  if (input$type_multilevel == "One proportion" && input$metric1_multilevel == "PR") {
    forest(res_multilevel, refline=NA, level=conflevel, digits=input$digits, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                                        else if (!is.null(hot$data$authors)) hot$data$authors
                                                                                        else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                                        else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                                        else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                                        else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                                        else if (!is.null(hot$data$study)) hot$data$study
                                                                                        else if (!is.null(hot$data$studies)) hot$data$studies,

                                                                           if (!is.null(hot$data$year)) hot$data$year
                                                                           else if (!is.null(hot$data$years)) hot$data$years,
                                                                           sep=", "
                                                                          ), atransf=if (input$atransf != "none") get(paste0("transf.", input$atransf)),
         # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
         alim=c(0, 1),
         clim=c(0, 1)
  )
  } else {
    forest(res_multilevel, refline=NA, level=conflevel, digits=input$digits, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                                        else if (!is.null(hot$data$authors)) hot$data$authors
                                                                                        else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                                        else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                                        else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                                        else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                                        else if (!is.null(hot$data$study)) hot$data$study
                                                                                        else if (!is.null(hot$data$studies)) hot$data$studies,
                                                                             
                                                                             if (!is.null(hot$data$year)) hot$data$year
                                                                             else if (!is.null(hot$data$years)) hot$data$years,
                                                                             sep=", "
    ), atransf=if (input$atransf != "none") get(paste0("transf.", input$atransf))
    # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
    )
  }

  })

output$msummary_multilevel <- renderPrint({
  if (input$type_multilevel %in% c("Two proportions", "Diagnostic")) {
    np_first <- min(hot$data$ai)
    np_second <- min(hot$data$ci)
    if (input$type_multilevel == "Two proportions") {
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
  print(res_multilevel)
  print("Study weights (percent; model is fit using inverse-variance approach)")
  print(weights(res_multilevel))
  print("Confidence intervals for residual heterogeneity")
  print(confint(res_multilevel))
})

})

#################################
##         save_fplot          ##
#################################
dataModal3_multilevel <- function(failed=F) {
  modalDialog(
    
    textInput("fplot_path_multilevel", "Type a path to save your forest plot:",
                "~/openmeta/plot1.png"),
    textInput("fplot_w_multilevel", "Width of forest plot:", "8"),
    textInput("fplot_h_multilevel", "Height of forest plot:", "6"),
    selectInput("fplot_unit_multilevel", "Unit of saved plot dimensions", c(`pixels`="px", `inches`="in", "cm", "mm"), "in"),
    textInput("fplot_resolution_multilevel", "Resolution of forest plot:", "210"),
    
    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot_multilevel", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot_multilevel, {
  showModal(dataModal3_multilevel())
})

observeEvent(input$ok_save_fplot_multilevel, {
  conflevel<-as.numeric(as.character(input$conflevel_multilevel))
  
  res_multilevel <- res_multilevel()
  
  ##save a png of the plot
  png(filename=input$fplot_path_multilevel, width=as.numeric(input$fplot_w_multilevel), height=as.numeric(input$fplot_h_multilevel), units=input$fplot_unit_multilevel, res=as.numeric(input$fplot_resolution_multilevel))
  if (input$type_multilevel == "One proportion" && input$metric1_multilevel == "PR") {
    forest(res_multilevel, refline=NA, digits=input$digits_multilevel, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                                                   else if (!is.null(hot$data$authors)) hot$data$authors
                                                                                                   else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                                                   else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                                                   else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                                                   else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                                                   else if (!is.null(hot$data$study)) hot$data$study
                                                                                                   else if (!is.null(hot$data$studies)) hot$data$studies,
                                                                             
                                                                             if (!is.null(hot$data$year)) hot$data$year
                                                                             else if (!is.null(hot$data$years)) hot$data$years,
                                                                             top=0,
                                                                             sep=", "
                                                                            ), atransf=if (input$atransf_multilevel != "none") get(paste0("transf.", input$atransf_multilevel)),
           # If raw proportion (cannot be less than 0 or greater than 1), enforce that limit on x-axis and confidence intervals
           alim=c(0, 1),
           clim=c(0, 1)
          )
  } else {
    forest(res_multilevel, refline=NA, digits=input$digits_multilevel, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                                                   else if (!is.null(hot$data$authors)) hot$data$authors
                                                                                                   else if (!is.null(hot$data$study.name)) hot$data$study.name
                                                                                                   else if (!is.null(hot$data$study.names)) hot$data$study.names
                                                                                                   else if (!is.null(hot$data[["study name"]])) hot$data[["study name"]]
                                                                                                   else if (!is.null(hot$data[["study names"]])) hot$data[["study names"]]
                                                                                                   else if (!is.null(hot$data$study)) hot$data$study
                                                                                                   else if (!is.null(hot$data$studies)) hot$data$studies,
                                                                             top=0,
                                                                             if (!is.null(hot$data$year)) hot$data$year
                                                                             else if (!is.null(hot$data$years)) hot$data$years,
                                                                             
                                                                             sep=", "
    ), atransf=if (input$atransf_multilevel != "none") get(paste0("transf.", input$atransf_multilevel))
    )
  }
  title("Multilevel Meta-Analysis")
  dev.off()
  
  removeModal()
})


##########################
##     dynamic UI       ##
##########################
observe({
  fixed_multilevel <- input$fixed_multilevel
  
  updateSelectInput(session,
                           "est_multilevel",
                           "Estimation method (Ignored if data contains a column called 'weights')",
                           
                           if (!is.null(fixed_multilevel) && fixed_multilevel == "RE") c(
                                                     `Maximum likelihood`="ML",
                                                     `Restricted ML`="REML"
                                                    )
                           else c(`Inverse-variance`="FE"),
                           
                           if (!is.null(fixed_multilevel) && fixed_multilevel == "RE") "REML"
                          )
})
