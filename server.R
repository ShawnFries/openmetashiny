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
  
  source("support/helpers.R", local=T)$value
  
  source("server/server_data_csv.R", local=T)$value
  
  source("server/server_meta_analysis.R", local=T)$value
  
  source("server/server_cum_data.R", local=T)$value
  
  source("server/server_cum_norm.R", local=T)$value
  
  source("server/server_report.R", local=T)$value
  
}