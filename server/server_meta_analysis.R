#############################
##       effect_norm       ##
#############################

#TODO: Add rma.glmm() for exact calculations as an option
#TODO: (lower priority) Support entering proportions as decimal from 0 to 1 (possibly + sample size)
# (Is part of backcalc)

dataModal2 <- function(failed=F) {
  modalDialog(
    selectInput("type", "Type of data", c("One proportion", "One mean", "Two proportions", "Two means"), switch(input$dataType,
                                                                                                                "proportion" = "One proportion",
                                                                                                                "mean" = "One mean",
                                                                                                                "proportions" = "Two proportions",
                                                                                                                "means" = "Two means"
                                                                                                               )
                ),
    conditionalPanel(
      condition="input.type == 'One proportion'",
      selectInput("metric1",
                  "Metric",
                  c(`PR - raw proportion`="PR", `PAS - arcsine transformed proportion`="PAS", `PLO - logit transformed proportion`="PLO")
                 )
    ),
    conditionalPanel(
      condition="input.type == 'One mean'",
      selectInput("metric2",
                  "Metric", 
                  c(`MN - raw mean`="MN", 
                            `MNLN - log transformed mean`="MNLN", 
                            `CVLN - log transformed coefficient of variation`="CVLN",
                            `SDLN - log transformed standard deviation`="SDLN")
                  )
    ),
    conditionalPanel(
      condition="input.type == 'Two proportions'",
      selectInput("metric3",
                  "Metric", 
                  c(`RR - log risk ratio`="RR", 
                            `OR - log odds ratio`="OR",
                            `RD - risk difference`="RD",
                            `AS - arcsine square root transformed risk difference`="AS",
                            `PETO - log odds ratio estimated with Peto's method`="PETO"
                           )
                 )
    ),
    conditionalPanel(
      condition="input.type == 'Two means'",
      selectInput("metric4",
                  "Metric", 
                  c(`MD - raw mean difference`="MD",
                            `SMD - standardized mean difference`="SMD",
                            `SMDH - standardized mean difference with heteroscedastic population variances in the two groups`="SMDH",
                            `ROM - log transformed ratio of means`="ROM"
                           )
                 )
),
      conditionalPanel(
        condition="input.metric4 == 'MD'",
        checkboxInput("use_homoscedasticity",
                      "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
                     )
        ),
      conditionalPanel(
        condition="input.metric4 == 'SMD'",
        checkboxInput("variance_is_approximate",
                      "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used.", 
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
    vals$dataescalc<-tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3,
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
    
  } else if (!is.null(hot$data) & input$type=="Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric4, 
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
             vtype=if (input$metric4 == "SMD" & !input$variance_is_approximate) "UB"
                   else if (input$metric4 == "MD" & input$use_homoscedasticity) "HO"
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
  
    tryCatch({
    rma(yi,
        vi,
        method=if (input$fixed_norm == "RE") input$est else "FE",
        data=vals$dataescalc,
        weighted=F,
        add=cc,
        to=input$addto,
        level=conflevel,
        digits=input$digits)
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
  
  ##display forest plot
  forest(res, refline=NA, level=conflevel, digits=input$digits, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                           else if (!is.null(hot$data$authors)) hot$data$authors,

                                                                           if (!is.null(hot$data$year)) hot$data$year
                                                                           else if (!is.null(hot$data$years)) hot$data$years,

                                                                           sep=", "
                                                                          )
  )

  })

output$msummary_norm <- renderPrint({
  print(res)
})

})

#################################
##         save_fplot          ##
#################################
dataModal3 <- function(failed=F) {
  modalDialog(
    
    textInput("fplot_path", "Type a path to save your forest plot:",
                "~/openmeta/plot1.png"),
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
  
  res<-res()
  
  ##save a png of the plot
  png(filename=input$fplot_path, width=as.numeric(input$fplot_w), height=as.numeric(input$fplot_h), units=input$fplot_unit, res=as.numeric(input$fplot_resolution))
  
  forest(res, refline=NA, digits=input$digits, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                           else if (!is.null(hot$data$authors)) hot$data$authors,
                                                                           
                                                                           if (!is.null(hot$data$year)) hot$data$year
                                                                           else if (!is.null(hot$data$years)) hot$data$years,
                                                                           
                                                                           sep=", "
                                                                          )
        )
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
                           "Estimation method",
                           
                           if (fixed_norm == "RE") c(`DerSimonian Laird`="DL",
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
                           
                           if (fixed_norm == "RE") "REML"
                          )
})
