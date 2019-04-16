sidebarLayout(
  sidebarPanel(
    actionButton("effect_norm_reg", "Choose effect measure"),                              ####effect_norm in server_meta_norm.R
    selectInput("fixed_norm_reg",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"),
    selectInput("est_reg", "Estimation method", c(`DerSimonian Laird`="DL", `Maximum likelihood`="ML", `Restricted ML`="REML"), "REML"),
    sliderInput("digits_reg", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel_reg", "Confidence level", value="95"),
    textInput("cc_reg", "Continuity correction", value="0.5"),
    selectInput("addto_reg", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    actionButton("oknorm_res_reg", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_reg")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_norm_reg")),      ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", plotOutput("forest_norm_reg"), actionButton("save_fplot_reg", "Save forest plot")                     ####save_fplot in server_meta_norm.R
                   )
    )
  )#ends mainPanel
)