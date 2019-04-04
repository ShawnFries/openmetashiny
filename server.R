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
  
  updateSelectizeInput(session, "select", choices=list("One piece of data from each study or studies within one group"=
                                                         c("proportion ( \\( \\frac{x}{N} \\) )"="proportion",
                                                           "mean ( \\\\\\mu ) )"="mean",
                                                           "regression coefficient ( \\( \\beta \\) )"="regression coefficient",
                                                           "generic effect size ( \\(\\theta, se \\) )"="generic effect size"),
                                                       "Data on two or more groups per study"=
                                                         c("proportions ( \\( \\frac{x_1}{N_1} \\text{ vs } \\frac{x_2}{N_2} \\) )"="proportions",
                                                           "means ( \\( \\mu_1 \\text{ vs } \\mu_2 \\) )"="means",
                                                           "SMD ( \\( g \\) )"="SMD"),
                                                       "Data on test performance"=
                                                         c("diagnostic (TP/FP/FN/TN)"="diagnostic")),
                       options = list(render = I("
    {
                                                 item:   function(item, escape) { 
                                                 var html = katex.renderToString(item.label);
                                                 return '<div>' + html + '</div>'; 
                                                 },
                                                 option: function(item, escape) { 
                                                 var html = katex.renderToString(item.label);
                                                 return '<div>' + html + '</div>'; 
                                                 }
}
")))
  
  source("support/helpers.R", local = TRUE)$value
  
  source("server/server_data_csv.R", local = TRUE)$value
  
  source("server/server_meta_analysis.R", local = TRUE)$value
  
  source("server/server_cum_data.R", local = TRUE)$value
  
  source("server/server_cum_norm.R", local = TRUE)$value
  
  source("server/server_report.R", local = TRUE)$value
  
}