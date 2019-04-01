observeEvent(input$okexact_res,{
  
  tryCatch({
    if (input$type2 == "Mean") {
      print(1)
      res <- rma.glmm(mi=vals$datar$mi,
                      #ti=vals$datar$sdi, 
                      ni=vals$datar$ni,
                      method=if (input$fixed_exact == "FE") input$fixed_exact else input$rand_est2,
                      data=vals$datar,
                      measure=input$metric_mean_exact  # TODO: Expand/fix this
                      )
    } else {  # Assumed to be proportion data
      print(2)
      res <- rma.glmm(xi=vals$datar$count,
                      ni=vals$datar$ni,
                      method=if (input$fixed_exact == "FE") input$fixed_exact else input$rand_est2,
                      data=vals$datar,
                      measure=input$metric_prop_exact  # TODO: Expand/fix this
      )
    }
  },
  error=function(err) {
    #error handler picks up where error was generated
    print(paste(input$type2, "ERROR:  ", err))
  }
  )
  # output$forest_norm<-renderPlot({
  #   conflevel<-as.numeric(as.character(input$conflevel2))
  # 
  #     forest(res, alim=c(0,1), refline=NA, digits=input$digits2, level=conflevel)
  # 
  # })
  
  output$msummary_exact<-renderPrint({
    print(res)
  })
  
  # output$temp<-renderDataTable({
  #   vals$datar
  # })
  
})


########################
##       effect2      ##
########################

dataModal5 <- function(failed = FALSE) {
  modalDialog(
    selectInput("type2", "Type of data", 
                choices=c("Proportion", "Mean", "Two proportions (2X2)"),
                selected="Proportion"),
    conditionalPanel(
      condition="input.type2 == 'Proportion'",
      selectInput("metric_prop_exact",
                  "Metric", 
                  choices=c(`PR - raw proportion`="PR", 
                            `PAS - arcsine transformed proportion`="PAS", 
                            `PLO - logit transformed proportion`="PLO"
                  ),
                  selected="PR"
      )
    ),
    conditionalPanel(
      condition="input.type2 == 'Mean'",
      selectInput("metric_mean_exact",
                  "Metric", 
                  choices=c(`MN - raw mean`="MN", 
                            `MNLN - log transformed mean`="MNLN", 
                            `CVLN - log transformed coefficient of variation`="CVLN",
                            `SDLN - log transformed standard deviation`="SDLN"),
                  selected="MN")
    ),
    conditionalPanel(
      condition="input.type2 == 'Two proportions (2X2)'",
      selectInput("metric_two_props_exact",
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
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okexact_data", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_exact, {
  showModal(dataModal5())
})

observeEvent(input$okexact_data,{ 
  output$dataexact<-renderTable({
    if(!is.null(vals$datar)){
      vals$datar
    }
  })
  removeModal()
})


##########################
##     dynamic UI       ##
##########################
output$rand_estimation2 <- renderUI({
  # if(input$type=="."){
  #   NULL
  # }else 
  
  if (input$fixed_exact == "FE") {
    selectInput("fixed_est2", "Estimation method", choices=c("Inverse-variance"), selected="Inverse-variance")
  } else if (input$fixed_exact == "RE") {
    selectInput("rand_est2", "Estimation method", choices=c(Maximum_likelihood="ML"), selected="ML")
  }
})