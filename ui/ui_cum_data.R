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
    tableOutput("cum_data")
  )
)