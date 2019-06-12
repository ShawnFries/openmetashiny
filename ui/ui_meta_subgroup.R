#TODO: This is just a placeholder with the meta regression fundamental code. Need to finish this to actually use subgroups not (just) moderators

sidebarLayout(
  sidebarPanel(
    actionButton("effect_norm_subgroup", "Choose effect measure and moderators"),                              ####effect_norm_subgroup in server_meta_subgroup.R
    selectInput("fixed_norm_subgroup",                                                         ####fixed_norm_subgroup in server_meta_subgroup.R
                "Fixed or random effect",
                c(`Fixed effect`="FE", Random_effects="RE"),
                "RE"
               ),
    selectInput("est_subgroup",
                "Estimation method",
                c(`DerSimonian Laird`="DL",
                  `Hedges`="HE",
                  `Hunter-Schmidt`="HS",
                  `Sidik-Jonkman`="SJ",
                  `Maximum likelihood`="ML",
                  `Restricted ML`="REML",
                  `Empirical Bayes`="EB",
                  `Paule-Mandel`="PM",
                  `Generalized Q-statistic`="GENQ"
                 ),
                "REML"
               ),
    sliderInput("digits_subgroup", "Number of digits to display", 1, 10, 3, 1),
    textInput("conflevel_subgroup", "Confidence level", "95"),
    textInput("cc_subgroup", "Continuity correction", "0.5"),
    selectInput("addto_subgroup", "Add continuity correction to", c("all", "only0", "if0all", "none"), "only0"),
    actionButton("oknorm_res_subgroup", "Show results"),
    bookmarkButton()
  ),
  mainPanel(
    tabBox(width=12,
           tabPanel("Transformed data", tableOutput("escalcdat_subgroup")),
           tabPanel("Meta-analysis summary", verbatimTextOutput("msummary_norm_subgroup")),      ####msummary_norm in server_meta_norm.R
           tabPanel("Forest plot", uiOutput("forest_plots"), actionButton("save_fplot_subgroup", "Save forest plot") ####save_fplot in server_meta_norm.R
                   )
    )
  )#ends mainPanel
)