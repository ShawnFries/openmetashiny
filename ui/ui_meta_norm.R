sidebarLayout(
  sidebarPanel(
    actionButton("effect_norm", "Choose effect measure"),                              ####effect_norm in server_meta_norm.R
    selectInput("fixed_norm",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                choices=c(`Fixed effect`="FE", Random_effects="RE"),
                selected="FE"),
    uiOutput("rand_estimation"),
    sliderInput("digits", "Number of digits to display",
                min=1, max=10, value=3, step=1),
    textInput("conflevel", "Confidence level", value="95"),
    textInput("cc", "Continuity correction", value="0.5"),
    selectInput("addto", "Add continuity correction to", 
                choices=c("all", "only0", "if0all", "none"),
                selected="only0"),
    actionButton("oknorm_res", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_norm")),      ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", plotOutput("forest_norm"),
                    actionButton("save_fplot", "Save forest plot"))                     ####save_fplot in server_meta_norm.R
           )

  )#ends mainPanel
)