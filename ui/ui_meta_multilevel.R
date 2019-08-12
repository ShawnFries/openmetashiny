sidebarLayout(
  sidebarPanel(
    actionButton("effect_multilevel", "Choose effect measure"),                              ####effect_multilevel in server_meta_multilevel.R
    selectInput("fixed_multilevel",                                                         ####fixed_multilevel in server_meta_multilevel.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"),
    selectInput("est_multilevel", "Estimation method (Ignored if data contains a column called 'weights' unless using Peto/MH method)", c(
                                                                                                                               `Maximum likelihood`="ML",
                                                                                                                               `Restricted ML`="REML"
    ), "REML"),
    sliderInput("digits_multilevel", "Number of digits to display", min=1, max=10, value=3, step=1),
    textInput("conflevel_multilevel", "Confidence level", value="95"),
    textInput("cc_multilevel", "Continuity correction", value="0.5"),
    selectInput("addto_multilevel", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    #TODO: Add all functionality from metafor trans
    selectInput("atransf_multilevel", "X-axis transformation", c("none"="none",
                                                      "logit (log odds, for proportions)"="logit",
                                                      "inverse logit"="ilogit",
                                                      "arcsine square-root (for proportions)"="arcsin",
                                                      "inverse arcsine square-root"="iarcsin",
                                                      "square (x²)"="isqrt"
    )
    ),
    actionButton("okmultilevel_res", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_multilevel")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_multilevel")),      ####msummary_multilevel in server_meta_multilevel.R
           tabPanel("Forest plot", br(), br(), br(), column(12, align="center", "Multilevel Meta-Analysis"), plotOutput("forest_multilevel"),
                    actionButton("save_fplot_multilevel", "Save forest plot")                     ####save_fplot in server_meta_multilevel.R
           )
    )
  )#ends mainPanel
)