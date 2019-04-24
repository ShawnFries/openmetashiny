sidebarLayout(
  sidebarPanel(
    actionButton("effect_leave_norm", "Choose effect measure"),                              ####effect_norm in server_data_csv.R
    selectInput("fixed_leave_norm",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"
    ),
    uiOutput("rand_leave_estimation"),
    sliderInput("digits_leave", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel_leave", "Confidence level", value="95"),
    textInput("cc_leave", "Continuity correction", value="0.5"),
    selectInput("addto_leave", "Add continuity correction to", 
                c("all", "only0", "if0all", "none"),
                "only0"),
    actionButton("oknorm_leave_res", "Show results")
  ),
  mainPanel(
    tabBox(width=12,
      tabPanel("Transformed data", tableOutput("escalcdat_leave")),
      tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_leave_norm")),                                   ####msummary_norm in server_meta_norm.R
      tabPanel("Forest plot", plotOutput("plot_leave_norm"), actionButton("save_fplot", "Save forest plot"))
      
    )
  )#ends mainPanel
)