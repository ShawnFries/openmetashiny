sidebarLayout(
  sidebarPanel(
    actionButton("effect_cum_norm", "Choose effect measure"),                              ####effect_norm in server_data_csv.R
    selectInput("fixed_cum_norm",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"
               ),
    uiOutput("rand_cum_estimation"),
    sliderInput("digits_cum", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel_cum", "Confidence level", value="95"),
    textInput("cc_cum", "Continuity correction", value="0.5"),
    selectInput("addto_cum", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    actionButton("oknorm_cum_res", "Show results")
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_cum")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_cum_norm")),                                   ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", plotOutput("forest_cum_norm"), actionButton("save_fplot", "Save forest plot"))
           
    )
  )#ends mainPanel
)