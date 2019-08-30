#############################
##       effect_exact       ##
#############################

#TODO: This only really does anything for 2 proportions/diagnostic with odds ratio. Remove "one proportion"/update errors for normal as appropriate.
# The exact approach uses the hypergeometric-normal model for odds ratios as described in documentation.
# (Is part of backcalc)
dataModal2_exact <- function(failed=F) {
  modalDialog(
    selectInput("type_exact", "Type of data", c("Two proportions", "Diagnostic"), switch(input$dataType,
                                                                                         "diagnostic"="Diagnostic"
                                                                                        )
    ), 
    footer=tagList(
      modalButton("Cancel"),
      actionButton("okexact_escalc", "OK")                                     ####okexact_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_exact, {
  showModal(dataModal2_exact())
})

observeEvent(input$okexact_escalc, {                         ####okexact_escalc
  if(!is.null(hot$data) & input$type_exact == "Two proportions") {
    vals$dataescalc_exact <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      rma.glmm(measure="OR",
             ai=ai,
             n1i=n1i,
             ci=ci,
             n2i=n2i,
             slab=paste(if (!is.null(hot$data$author)) hot$data$author
                        else if (!is.null(hot$data$authors)) hot$data$authors,
                        
                        if (!is.null(hot$data$year)) hot$data$year
                        else if (!is.null(hot$data$years)) hot$data$years,
                        
                        sep=", "
             ),
            # model="CM.EL", # This argument seems to make it take forever to calculate (literally as far as I could tell in limited computing time..) but uses a quasi Newtonian method
            # Default uses iteratively reweighted least squares
             data=hot$data)
      },
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"ci\", \"n1i\", and \"n2i\"")
      }
    )#ends tryCatch
    removeModal()
    
  } else if (!is.null(hot$data) & input$type_exact == "Diagnostic") {
    vals$dataescalc_exact <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      rma.glmm(measure="OR",
             ai=ai,
             bi=bi,
             ci=ci,
             di=di,
             slab=paste(if (!is.null(hot$data$author)) hot$data$author
                        else if (!is.null(hot$data$authors)) hot$data$authors,
                        
                        if (!is.null(hot$data$year)) hot$data$year
                        else if (!is.null(hot$data$years)) hot$data$years,
                        
                        sep=", "
             ),
             model="CM.EL",
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"bi\", \"ci\", and \"di\"")
      }
    )#ends tryCatch
    removeModal()
  } else {
    showModal(dataModal_exact(failed=T))
  }
  
  output$escalcdat_exact <- renderTable({
    if (!is.null(hot$data)) {
      hot$data
    }
  })
})

#################################
##         okexact_res          ##
#################################

res_exact <- eventReactive(input$okexact_res, {
  conflevel <- as.numeric(as.character(input$conflevel_exact))
  cc <- as.numeric(as.character(input$cc_exact))
  
    tryCatch({
      #TODO: Replace escalc above with this for each
      vals$dataescalc_exact
    },
    error=function(err) {
      print(paste("ERROR:  ", err))
    }
    )
}
)

observeEvent(input$okexact_res, {
# cc<-as.numeric(as.character(input$cc))
# 
# res<-if(input$fixed_exact=="FE"){
#   rma(yi, vi, method=input$fixed_exact, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }else if(input$fixed_exact=="RE"){
#   rma(yi, vi, method=input$rand_est, data=vals$dataescalc, weighted=FALSE, add=cc, to=input$addto)
# }
res_exact <- res_exact()

#####################NEEDS TO BE GENERALIZED############################

output$forest_exact <- renderPlot({
  conflevel <- as.numeric(as.character(input$conflevel_exact))
  
  ##display forest plot
  forest(res_exact, refline=NA, level=conflevel, digits=input$digits_exact, slab=paste(if (!is.null(hot$data$author)) hot$data$author
                                                                           else if (!is.null(hot$data$authors)) hot$data$authors,

                                                                           if (!is.null(hot$data$year)) hot$data$year
                                                                           else if (!is.null(hot$data$years)) hot$data$years,

                                                                           sep=", "
                                                                          )
  )

  })

output$msummary_exact <- renderPrint({
  print(res_exact)
})

})

#################################
##         save_fplot          ##
#################################
dataModal3_exact <- function(failed=F) {
  modalDialog(
    
    textInput("fplot_path_exact", "Type a path to save your forest plot:",
                "~/openmeta/plot1.png"),
    textInput("fplot_w_exact", "Width of forest plot:", "8"),
    textInput("fplot_h_exact", "Height of forest plot:", "6"),
    selectInput("fplot_unit_exact", "Unit of saved plot dimensions", c(`pixels`="px", `inches`="in", "cm", "mm"), "in"),
    textInput("fplot_resolution_exact", "Resolution of forest plot:", "210"),
    
    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot_exact", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot_exact, {
  showModal(dataModal3_exact())
})

observeEvent(input$ok_save_fplot_exact, {
  conflevel<-as.numeric(as.character(input$conflevel_exact))
  
  res_exact <- res_exact()
  
  ##save a png of the plot
  png(filename=input$fplot_path_exact, width=as.numeric(input$fplot_w_exact), height=as.numeric(input$fplot_h_exact), units=input$fplot_unit_exact, res=as.numeric(input$fplot_resolution_exact))
  
  forest(res_exact, refline=NA, digits=input$digits_exact, level=conflevel, slab=paste(if (!is.null(hot$data$author)) hot$data$author
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
  fixed_exact <- input$fixed_exact
  
  updateSelectInput(session,
                           "est_exact",
                           "Estimation method",
                           
                           if (!is.null(fixed_exact) && fixed_exact == "RE") c(`DerSimonian Laird`="DL",
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
                           
                           if (!is.null(fixed_exact) && fixed_exact == "RE") "REML"
                          )
})
