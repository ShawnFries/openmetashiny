sidebarLayout(
  sidebarPanel(
    actionButton("effect_cum_norm", "Choose effect measure"),                              ####effect_norm in server_data_csv.R
    selectInput("fixed_cum_norm",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                choices=c(`Fixed effect`="FE", Random_effects="RE"),
                selected="FE"),
    uiOutput("rand_cum_estimation"),
    sliderInput("digits_cum", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel_cum", "Confidence level", value="95"),
    textInput("cc_cum", "Continuity correction", value="0.5"),
    selectInput("addto_cum", "Add continuity correction to", 
                choices=c("all", "only0", "if0all", "none"),
                selected="only0"),
    actionButton("oknorm_cum_res", "Show results")
  ),
  mainPanel(
    fluidRow(
      column(width=6, 
             tableOutput("escalcdat_cum")),
      column(width=6, 
             verbatimTextOutput("msummary_cum_norm"))),                                   ####msummary_norm in server_meta_norm.R
    fluidRow(
      plotOutput("forest_cum_norm")
    )
  )#ends mainPanel
)