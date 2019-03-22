sidebarLayout(
  sidebarPanel(
    selectInput("type_cum_data", "Type of data",                                                    ####type_cum_data
                choices=c("Proportion", "Mean", "Two proportions (2X2)"),
                selected="Proportion")
  ),
  mainPanel(
    tableOutput("cum_data")
  )
)