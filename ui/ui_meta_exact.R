sidebarLayout(
  sidebarPanel(
    actionButton("effect_exact", "Choose effect measure"),                              ####effect_exact in server_meta_exact.R
    selectInput("fixed_exact",                                                         ####fixed_exact in server_meta_exact.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"),
    selectInput("est_exact", "Estimation method", c(`DerSimonian Laird`="DL",
                                              `Hedges`="HE",
                                              `Hunter-Schmidt`="HS",
                                              `Sidik-Jonkman`="SJ",
                                              `Maximum likelihood`="ML",
                                              `Restricted ML`="REML",
                                              `Empirical Bayes`="EB",
                                              `Paule-Mandel`="PM",
                                              `Generalized Q-statistic`="GENQ"
                                             ), "REML"),
    sliderInput("digits_exact", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel_exact", "Confidence level", value="95"),
    textInput("cc_exact", "Continuity correction", value="0.5"),
    selectInput("addto_exact", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    selectInput("atransf_exact", "X-axis transformation", c("none"="none",
                                                      "logit (log odds, for proportions)"="logit",
                                                      "inverse logit"="ilogit",
                                                      "arcsine square-root (for proportions)"="arcsin",
                                                      "inverse arcsine square-root"="iarcsin",
                                                      "square (x²)"="isqrt"
    )
    ),
    actionButton("okexact_res", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_exact")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_exact")),      ####msummary_exact in server_meta_exact.R
           tabPanel("Forest plot", plotOutput("forest_exact"),
                    actionButton("save_fplot_exact", "Save forest plot")                     ####save_fplot in server_meta_exact.R
                   )
           )
  )#ends mainPanel
)