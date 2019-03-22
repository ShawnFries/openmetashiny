sidebarLayout(
  sidebarPanel(
    actionButton("effect_exact", "Choose effect measure"),
    selectInput("fixed_exact",                                                            ####fixed_exact in server_meta_exact.R
                "Fixed or random effect",
                choices=c(Fixed_effect="FE", Random_effects="RE"),
                selected="FE"),
    uiOutput("rand_estimation2"),
    sliderInput("digits2", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel2", "Confidence level", value="95"),
    actionButton("okexact_res", "Show results")                                           ####okexact_res in server_meta_exact.R
  ),
  mainPanel(
    fluidRow(
      column(width=6, 
             tableOutput("dataexact")),
      column(width=6, 
             verbatimTextOutput("msummary_exact"))),                                       ####msummaryexact in server_meta_exact.R
    fluidRow(
      plotOutput("forest_exact")
    )
  )
)