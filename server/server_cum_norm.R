#############################
##    effect_cum_norm      ##
#############################

dataModal2_cum <- function(failed = FALSE) {
  modalDialog(
    selectInput("type_cum", "Type of data",
                choices=c("Proportion", "Mean", "Two proportions (2X2)"),
                selected="Proportion"),
    conditionalPanel(
      condition="input.type_cum == 'Proportion'",
      selectInput("metric1_cum", "Metric", 
                  choices=c(raw_proportion="PR", arcsine="PAS", logit="PLO"),
                  selected="PR")
    ),
    conditionalPanel(
      condition="input.type_cum == 'Mean'",
      selectInput("metric2_cum", "Metric", 
                  choices=c("MD", "SMD", "SMDH", "ROM"),
                  selected="MD")
    ),
    conditionalPanel(
      condition="input.type_cum == 'Two proportions (2X2)'",
      selectInput("metric3_cum", "Metric", 
                  choices=c("RR", "OR", "RD", "AS", "PETO"),
                  selected="RR")
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("oknorm_cum_escalc", "OK")                                             ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_cum_norm, {
  showModal(dataModal2_cum())
})


observeEvent(input$oknorm_cum_escalc,{                                                              ####oknorm_cum_escalc
  
  if(!is.null(vals$data) & input$type_cum=="Proportion"){
    vals$dataescalc<-tryCatch({
      cum_data<-vals$data
      cum_data$xi<-cumfunc(cum_data$xi)
      cum_data$ni<-cumfunc(cum_data$ni)
      escalc(measure=input$metric1_cum, xi=cum_data$xi, 
             ni=cum_data$ni, data=cum_data)},
      error=function(err){
        #error handler picks up where error was generated
        print(paste("ERROR:  ",err))}
    )#ends tryCatch
    removeModal()
    
  }else if(!is.null(vals$data) & input$type_cum=="Mean"){
    vals$dataescalc_cum<-tryCatch({
      escalc(measure=input$metric2_cum, 
             m1i=vals$data$m1i, sd1i=vals$data$sd1i, n1i=vals$data$n1i,
             m2i=vals$data$m2i, sd2i=vals$data$sd2i, n2i=vals$data$n2i, 
             data=vals$data)},
      error=function(err){
        print(paste("ERROR:  ",err))}
    )#ends tryCatch
    removeModal()
    
  }else if(!is.null(vals$data) & input$type_cum=="Two proportions (2X2)"){
    vals$dataescalc<-tryCatch({
      escalc(measure=input$metric3_cum, 
             ai=vals$data$ai, n1i=vals$data$n1i, 
             ci=vals$data$ci, n2i=vals$data$n2i, 
             data=vals$data)},
      error=function(err){
        print(paste("ERROR:  ",err))}
    )#ends tryCatch
    removeModal()
  }else{
    showModal(dataModal(failed = TRUE))
  }
  
  output$escalcdat_cum<-renderTable({
    if(!is.null(vals$dataescalc)){
      vals$dataescalc
    }
  })
})













# #################################
# ##         oknorm_res          ##
# #################################
# 
# observeEvent(input$oknorm_res,{
#   cc<-as.numeric(as.character(input$cc))
#   
#   res<-if(input$fixed_norm=="FE"){
#     rma(yi, vi, method=input$fixed_norm, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
#   }else if(input$fixed_norm=="RE"){
#     rma(yi, vi, method=input$rand_est, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
#   }
#   
#   #####################NEED TO BE GENERALIZED############################
#   output$forest_norm<-renderPlot({
#     conflevel<-as.numeric(as.character(input$conflevel))
#     if(input$metric1=="PLO"){
#       forest(res, transf=transf.ilogit, targs=list(ni=vals$data$ni), 
#              xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
#     }else if(input$metric1=="PAS"){
#       forest(res, transf=transf.isqrt, targs=list(ni=vals$data$ni), 
#              xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
#     }else if(input$metric1=="PR"){
#       forest(res, xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
#     }
#   })
#   
#   output$msummary_norm<-renderPrint({
#     print(res)
#   })
#   
# })
# 
# 
# 
# 
# 
# 
# ##########################
# ##     dynamic UI       ##
# ##########################
# 
# output$rand_estimation<-renderUI({
#   # if(input$type=="."){
#   #   NULL
#   # }else 
#   
#   if(input$fixed_norm=="FE"){                                                   ####fixed_norm in ui_meta_norm.R
#     selectInput("fixed_est",
#                 "Estimation method",
#                 choices=c("Inverse-variance"),
#                 selected="Inverse-variance")
#   }else if(input$fixed_norm=="RE"){                                             ####fixed_norm in ui_meta_norm.R
#     selectInput("rand_est",
#                 "Estimation method",
#                 choices=c(DerSimonian_Laird="DL", Maximum_likelihood="ML", 
#                           Restricted_ML="REML"),
#                 selected="DL")
#   }
# })