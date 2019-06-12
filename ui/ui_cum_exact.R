sidebarLayout(
  sidebarPanel(
    actionButton("effect_cum_exact", "Choose effect measure"),
    selectInput("fixed_cum_exact",                                                            ####fixed_exact in server_meta_exact.R
                "Fixed or random effect",
                choices=c(Fixed_effect="FE", Random_effects="RE")
                ),
    uiOutput("rand_exact_cum_estimation2"),
    sliderInput("digits2_exact_cum", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel2_exact_cum", "Confidence level", value="95"),
    textInput("cc2_exact_cum", "Continuity correction", value="0.5"),
    actionButton("okexact_cum_res", "Show results")                                           ####okexact_res in server_meta_exact.R
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_exact_cum")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_cum_exact")),                                   ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", plotOutput("forest_cum_exact"), actionButton("save_exact_fplot", "Save forest plot"))
           
    )
  )#ends mainPanel
)