#TODO: Re-label accumulated columns etc (including in forest plot and data table) to make clear that it's an accumulated column, not the value for each

output$cum_data <- renderTable({
  
  cum_data <-
  if (!is.null(hot$data)) {
    
    if (input$type_cum_data == "Proportion") {
  
      cbind(hot$data[, !colnames(hot$data) %in% c("xi", "ni", "count")],
            lapply(hot$data[ , colnames(hot$data) %in% c("xi", "ni", "count")], cumfunc)
           )
  
    } else if (input$type_cum_data == "Mean") {
      
      cbind(hot$data[, !colnames(hot$data) %in% c("mi", "sdi", "ni")],
            lapply(hot$data[ , colnames(hot$data) %in% c("mi", "sdi", "ni")], cumfunc)
      )
      
    } else if (input$type_cum_data == "Two proportions (2X2)") {
  
      cbind(hot$data[, !colnames(hot$data) %in% c("ai", "ci", "n1i", "n2i")],
            lapply(hot$data[ , colnames(hot$data) %in% c("ai", "ci", "n1i", "n2i")], cumfunc)
           )
    } else if (input$type_cum_data == "Two means") {
      
      cbind(hot$data[, !colnames(hot$data) %in% c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i")],
            lapply(hot$data[ , colnames(hot$data) %in% c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i")], cumfunc)
      )
    }
  }
  cum_data
  
})

observe({
  
  updateSelectInput(session,
                    "type_cum_data",
                    "Type of data",
                    c("Proportion", "Mean", "Two proportions (2X2)", "Two means"),
                    switch(dataType$type,
                           "mean" = "Mean",
                           "proportions" = "Two proportions (2X2)",
                           "means" = "Two means"
                    )
  )
})