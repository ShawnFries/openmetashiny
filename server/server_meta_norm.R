#############################
##       effect_norm       ##
#############################

dataModal2 <- function(failed=F) {
  modalDialog(
    selectInput("type", "Type of data", choices=c("One proportion", "One mean", "Two proportions", "Two means"), selected="One proportion"),
    conditionalPanel(
      condition="input.type == 'One proportion'",
      selectInput("metric1",
                  "Metric", 
                  choices=c(`PR - raw proportion`="PR", `PAS - arcsine transformed proportion`="PAS", `PLO - logit transformed proportion`="PLO"),
                  selected="PR"
                 )
    ),
    conditionalPanel(
      condition="input.type == 'One mean'",
      selectInput("metric2",
                  "Metric", 
                  choices=c(`MN - raw mean`="MN", 
                            `MNLN - log transformed mean`="MNLN", 
                            `CVLN - log transformed coefficient of variation`="CVLN",
                            `SDLN - log transformed standard deviation`="SDLN"),
                  selected="MN")
    ),
    conditionalPanel(
      condition="input.type == 'Two proportions'",
      selectInput("metric3",
                  "Metric", 
                  choices=c(`RR - log risk ratio`="RR", 
                            `OR - log odds ratio`="OR", 
                            `RD - risk difference`="RD", 
                            `AS - arcsine square root transformed risk difference`="AS", 
                            `PETO - log odds ratio estimated with Peto's method`="PETO"
                           ),
                  selected="RR"
                 )
    ),
    conditionalPanel(
      condition="input.type == 'Two means'",
      selectInput("metric4",
                  "Metric", 
                  choices=c(`MD - raw mean difference`="MD", 
                            `SMD - standardized mean difference`="SMD", 
                            `SMDH - standardized mean difference with heteroscedastic population variances in the two groups`="SMDH", 
                            `ROM - log transformed ratio of means`="ROM"
                           ),
                  selected="MD"
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

observeEvent(input$oknorm_escalc, {                                                              ####oknorm_escalc
  if(!is.null(vals$datar) & input$type == "One proportion"){
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric1, xi=output$hot$count, ni=output$hot$ni, data=output$hot)},
      error=function(err) {
        #error handler picks up where error was generated
        print(paste("ERROR:  ", err)
        )
    }
    )#ends tryCatch
    removeModal()
  
  } else if(!is.null(vals$datar) & input$type == "One mean"){
    vals$dataescalc <- tryCatch({
      escalc(measure=input$metric2, 
             mi=vals$datar$mi,
             sdi=vals$datar$sdi, 
             ni=vals$datar$ni, 
             data=vals$datar
             )
      },
      error=function(err) {
        print(paste("ERROR:  ", err))
        }
    )#ends tryCatch
    removeModal()
    
  } else if(!is.null(vals$datar) & input$type == "Two proportions"){
    vals$dataescalc<-tryCatch({
      escalc(measure=input$metric3, ai=vals$datar$ai, n1i=vals$datar$n1i, ci=vals$datar$ci, n2i=vals$datar$n2i, data=vals$datar)},
      error=function(err){
        print(paste("ERROR:  ",err))}
    )#ends tryCatch
    removeModal()
    
  }else if(!is.null(vals$datar) & input$type=="Two means"){
    vals$dataescalc<-tryCatch({
      escalc(measure=input$metric4, 
             m1i=vals$datar$m1i, sd1i=vals$datar$sd1i, n1i=vals$datar$n1i,
             m2i=vals$datar$m2i, sd2i=vals$datar$sd2i, n2i=vals$datar$n2i, 
             data=vals$datar)},
      error=function(err){
        print(paste("ERROR:  ",err))}
    )#ends tryCatch
    removeModal()
    
  }else{
    showModal(dataModal(failed = TRUE))
  }
  
  
  output$escalcdat<-renderTable({
    if(!is.null(vals$dataescalc)){
      vals$dataescalc
    }
  })
})






#################################
##         oknorm_res          ##
#################################

res <- eventReactive(input$oknorm_res, {
  cc <- as.numeric(as.character(input$cc))
  
    tryCatch({
    rma(yi, vi, method=if (input$fixed_norm == "FE") input$fixed_norm else input$rand_est, data=vals$dataescalc, weighted=F, add=cc, to=input$addto)
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
conflevel <- as.numeric(as.character(input$conflevel))
output$forest_norm <- renderPlot({
  conflevel <- as.numeric(as.character(input$conflevel))
  
  ##display forest plot
  if (input$metric1 == "PLO"){
    forest(res, transf=transf.ilogit, targs=list(ni=vals$datar$ni), refline=NA, level=conflevel, digits=input$digits)
  } else if(input$metric1 == "PAS") {
    forest(res, transf=transf.isqrt, targs=list(ni=vals$datar$ni), refline=NA, level=conflevel, digits=input$digits)
  } else if(input$metric1 == "PR") {
    forest(res, refline=NA, level=conflevel, digits=input$digits)
  }

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
    selectInput("fplot_unit", "Unit of saved plot dimensions", choices=c(`pixels`="px", `inches`="in", "cm", "mm"), selected="in"),
    textInput("fplot_resolution", "Resolution of forest plot:", "210"),
    
    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot, {
  showModal(dataModal3())
})

observeEvent(input$ok_save_fplot,{
  conflevel<-as.numeric(as.character(input$conflevel))
  
  res<-res()
  
  ##save a png of the plot
  png(filename=input$fplot_path, width=as.numeric(input$fplot_w), height=as.numeric(input$fplot_h), units=input$fplot_unit, res=as.numeric(input$fplot_resolution))
  
  if(input$metric1 == "PLO") {
    forest(res, transf=transf.ilogit, targs=list(ni=vals$datar$ni), refline=NA, digits=input$digits, level=conflevel)
  } else if(input$metric1 == "PAS") {
    forest(res, transf=transf.isqrt, targs=list(ni=vals$datar$ni), refline=NA, digits=input$digits, level=conflevel)
  } else if(input$metric1 == "PR") {
    forest(res, refline=NA, digits=input$digits, level=conflevel)
  }
  
  dev.off()
  
  removeModal()
})










##########################
##     dynamic UI       ##
##########################

output$rand_estimation <- renderUI({
  # if(input$type=="."){
  #   NULL
  # }else 
  
  if (input$fixed_norm == "FE") {                                                   ####fixed_norm in ui_meta_norm.R
    selectInput("fixed_est", "Estimation method", choices=c("Inverse-variance"), selected="Inverse-variance")
  }else if (input$fixed_norm == "RE") {                                             ####fixed_norm in ui_meta_norm.R
    selectInput("rand_est", "Estimation method", choices=c(`DerSimonian Laird`="DL", `Maximum likelihood`="ML", `Restricted ML`="REML"), selected="REML")
  }
})