sidebarLayout(
  sidebarPanel(
    selectInput("type_cum_data", "Type of data",                                                    ####type_cum_data
                c("Proportion", "Mean", "Two proportions (2X2)", "Two means")#,
                #switch(dataType,
                    #   "mean" = "Mean",
                     #  "proportions" = "Two proportions (2X2)",
                     #  "means" = "Two means"
                     # )
               )
  ),
  mainPanel(
    #TODO: Think this needs to be moved to server to clean up the error display..
    tryCatch({
      tableOutput("cum_data")
    },
    error=function(err) {
      #error handler picks up where error was generated
      print(paste("ERROR:  ", err))
    }
    )
  )
)