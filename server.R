library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard) #tabBox()
library(rhandsontable)
library(DT)
library(metafor)
library(dplyr)
library(grDevices) #png()

server <- function(input, output, session) {withMathJax()
  app <- reactiveValues(page=1)
  
  observe({
    #toggleState(id="backButton", condition=app$page > 1)
    toggleState(id="continueButton", condition=app$page == 1)
  })
  
  nextStep <- function(direction) {
    app$page <- app$page + direction
  }
  
  #observeEvent(input$backButton, nextStep(-1))
  observeEvent(input$continueButton, nextStep(1))
  
  output$page <- renderText({
    app$page
  })
  
  outputOptions(output, "page", suspendWhenHidden=F)
  
  output$ui <- renderUI({
    bootstrapPage(withMathJax(), #MathJax is for displaying Greek symbols etc.
                  useShinyjs(),
                  conditionalPanel('output.page == 1',
                                   titlePanel("What type of data do you have?"),
                                               sidebarLayout(
                                                 sidebarPanel(selectizeInput("dataType",
                                                                             "Data Type",
                                                                              list("One piece of data from each study or studies within one group"=
                                                                                    c("proportion ( \\( \\frac{x}{N} \\) )"="proportion",
                                                                                      "mean ( \\( \\mu \\) )"="mean",
                                                                                      "regression coefficient ( \\( \\beta \\) )"="regression coefficient",
                                                                                      "generic effect size ( \\(\\theta, se \\) )"="generic effect size"),
                                                                                  "Data on two or more groups per study"=
                                                                                    c("proportions ( \\( \\frac{x_1}{N_1} \\text{ vs } \\frac{x_2}{N_2} \\) )"="proportions",
                                                                                      "means ( \\( \\mu_1 \\text{ vs } \\mu_2 \\) )"="means",
                                                                                      "SMD ( \\( g \\) )"="SMD"),
                                                                                  "Data on test performance"=
                                                                                    c("diagnostic (TP/FP/FN/TN)"="diagnostic"))
                                                 )),
                                                 mainPanel(
                                                 )),
                                                 actionButton("continueButton", "Continue")
                                   ),
                  conditionalPanel('output.page > 1',
                                   
                                   navbarPage(#div(id="title", img(src="meta.png")), #logo
                                     #"OpenMetaAnalyst", #just the words
                                     title=HTML("<a style=font-size:150%;color:black;href=\"http://www.cebm.brown.edu/openmeta\">OpenMeta[Analyst]</a>"),
                                     
                                     navbarMenu(div(id="data_div", img(src="data-512.png")), tabPanel("View Data", source("ui/ui_data_csv.R", local=T)$value)
                                     ),#navbarMenu "Data"
                                     bsTooltip("data_div", "Data", "right", options=list(container="body")),
                                     
                                     navbarMenu(div(id="meta_analysis_div", img(src="meta_analysis.png")),
                                                tabPanel("Normal approximation", source("ui/ui_meta_norm.R", local=T)$value),
                                                tabPanel("Exact likelihood", source("ui/ui_meta_exact.R", local=T)$value)
                                     ), #tabPanel "Meta-analysis"
                                     #https://stackoverflow.com/questions/44953873/add-tooltip-to-tabs-in-shiny
                                     #add tooltip to the Meta-analysis tabPanel
                                     bsTooltip("meta_analysis_div", "Meta-analysis", "right", options=list(container="body")),
                                     
                                     navbarMenu(div(id="cum_meta_analysis_div", img(src="cum_meta_analysis.png")),
                                                tabPanel("Accumulative data", source("ui/ui_cum_data.R", local=T)$value),
                                                tabPanel("Normal approximation", source("ui/ui_cum_norm.R", local=T)$value),
                                                tabPanel("Exact likelihood", source("ui/ui_cum_exact.R", local=T)$value)
                                     ),#tabPanel "Cumulative meta-analysis"
                                     bsTooltip("cum_meta_analysis_div", "Cumulative meta-analysis", "right", options=list(container="body")),
                                     
                                     tabPanel(div(id="subgroup_meta_analysis_div", img(src="subgroup_ma.png"))
                                     ),#tabPanel "Subgroup meta-analysis"
                                     bsTooltip("subgroup_meta_analysis_div", "Subgroup meta-analysis", "right", options=list(container="body")),
                                     
                                     tabPanel(div(id="meta_regression_div", img(src="meta_reg.png")), source("ui/ui_meta_reg.R", local=T)$value),#tabPanel "Meta-regression"
                                     bsTooltip("meta_regression_div", "Meta regression", "right", options=list(container="body")),
                                     
                                     tabPanel(div(id="leave_one_out_div", img(src="leave_one_out.png")), source("ui/ui_leave_one_out.R", local=T)$value),#tabPanel "Leave-one-out meta-analysis"
                                     bsTooltip("leave_one_out_div", "Leave-one-out meta-analysis", "right", options=list(container="body")),
                                     
                                     tabPanel(div(id="report_div", img(src="report.png")), source("ui/ui_report.R", local=T)$value),#tabPanel "Report"
                                     bsTooltip("report_div", "Generate report", "right", options=list(container="body")))),
                  
                  tags$style(type='text/css', '.navbar { 
                             font-family: Times;
                             font-size: 30px;}')
                  
                  )})
  
  source("support/helpers.R", local = TRUE)$value
  
  source("server/server_data_csv.R", local = TRUE)$value
  
  source("server/server_meta_norm.R", local = TRUE)$value
  
  source("server/server_meta_exact.R", local = TRUE)$value
  
  source("server/server_cum_data.R", local = TRUE)$value
  
  source("server/server_cum_norm.R", local = TRUE)$value
  
  source("server/server_report.R", local = TRUE)$value
  
}