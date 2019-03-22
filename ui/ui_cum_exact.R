sidebarLayout(
  sidebarPanel(
    actionButton("effect_cum_exact", "Choose effect measure"),
    selectInput("fixed_cum_exact",                                                            ####fixed_exact in server_meta_exact.R
                "Fixed or random effect",
                choices=c(Fixed_effect="FE", Random_effects="RE"),
                selected="FE"),
    uiOutput("rand_cum_estimation2"),
    sliderInput("digits2_cum", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel2_cum", "Confidence level", value="95"),
    actionButton("okexact_cum_res", "Show results")                                           ####okexact_res in server_meta_exact.R
  ),
  mainPanel(
    fluidRow(
      column(width=6, 
             tableOutput("dataexact_cum")),
      column(width=6, 
             verbatimTextOutput("msummary_cum_exact"))),                                       ####msummaryexact in server_meta_exact.R
    fluidRow(
      plotOutput("forest_cum_exact")
    )
  )
)