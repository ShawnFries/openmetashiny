sidebarLayout(
  sidebarPanel(
    actionButton("effect_norm_bivariate", "Choose effect measure"),                              ####effect_norm in server_meta_norm.R
    selectInput("fixed_norm_bivariate",                                                         ####fixed_norm in server_meta_norm.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"),
    selectInput("est_bivariate", "Estimation method (Ignored if data contains a column called 'weights' unless using Peto/MH method)", c(
                                              `Maximum likelihood`="ML",
                                              `Restricted ML`="REML"
                                             ), "REML"),
    sliderInput("digits_bivariate", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel_bivariate", "Confidence level", value="95"),
    textInput("cc_bivariate", "Continuity correction", value="0.5"),
    selectInput("addto_bivariate", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    #TODO: Add all functionality from metafor trans
    selectInput("atransf_bivariate", "Data transformation", c("none"="none",
                                                      "logit (log odds, for proportions)"="logit",
                                                      "inverse logit"="ilogit",
                                                      "arcsine square-root (for proportions)"="arcsin",
                                                      "inverse arcsine square-root"="iarcsin",
                                                      "square (x²)"="isqrt"
                                                     )
               ),
    actionButton("oknorm_res_bivariate", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_bivariate")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_norm_bivariate")),      ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", br(), br(), br(), column(12, align="center", "Meta-Analysis (Normal Approximation)"), plotOutput("forest_norm_bivariate", height="800px"),
                    actionButton("save_fplot_bivariate", "Save forest plot")                     ####save_fplot in server_meta_norm.R
                   )
           )
  )#ends mainPanel
)