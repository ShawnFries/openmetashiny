sidebarLayout(
  sidebarPanel(
    actionButton("effect_norm", "Choose effect measure"),                              ####effect_norm in server_meta_norm.R
    selectInput("fixed_norm",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"),
    selectInput("est", "Estimation method (Ignored if data contains a column called 'weights' unless using Peto/MH method)", c(`DerSimonian Laird`="DL",
                                              `Hedges`="HE",
                                              `Hunter-Schmidt`="HS",
                                              `Sidik-Jonkman`="SJ",
                                              `Maximum likelihood`="ML",
                                              `Restricted ML`="REML",
                                              `Empirical Bayes`="EB",
                                              `Paule-Mandel`="PM",
                                              `Generalized Q-statistic`="GENQ"
                                             ), "REML"),
    sliderInput("digits", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel", "Confidence level", value="95"),
    textInput("cc", "Continuity correction", value="0.5"),
    selectInput("addto", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    #TODO: Add all functionality from metafor trans
    selectInput("atransf", "X-axis transformation", c("none"="none",
                                                      "logit (log odds, for proportions)"="logit",
                                                      "inverse logit"="ilogit",
                                                      "arcsine square-root (for proportions)"="arcsin",
                                                      "inverse arcsine square-root"="iarcsin",
                                                      "square (x²)"="isqrt"
                                                     )
               ),
    actionButton("oknorm_res", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_norm")),      ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", br(), br(), br(), column(12, align="center", "Meta-Analysis (Normal Approximation)"), plotOutput("forest_norm"),
                    actionButton("save_fplot", "Save forest plot")                     ####save_fplot in server_meta_norm.R
                   )
           )
  )#ends mainPanel
)