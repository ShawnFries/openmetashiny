library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard) #tabBox()
library(rhandsontable)
library(DT)
library(metafor)
library(dplyr)
library(grDevices) #png()
library(gridExtra)
library(grid)

server <- function(input, output, session) {
  app <- reactiveValues(page=1)

  observe({
    #toggleState(id="backButton", condition=app$page > 1)
    toggleState("continueButton", app$page == 1)
    toggleState("continueButton2", app$page == 2)
  })

  nextStep <- function(direction) {
    app$page <- app$page + direction
    print(app$page)
  }
  
  #observeEvent(input$backButton, nextStep(-1))
  observeEvent(input$continueButton, nextStep(1))
  observeEvent(input$continueButton2, nextStep(1))

  output$page <- renderText({
    app$page
  })
  
  outputOptions(output, "page", suspendWhenHidden=F)

  source("support/helpers.R", local=T)$value

  source("server/server_data_csv.R", local=T)$value

  source("server/server_meta_norm.R", local=T)$value

  source("server/server_meta_exact.R", local=T)$value

  source("server/server_meta_multilevel.R", local=T)$value
  
  source("server/server_cum_data.R", local=T)$value
  
  source("server/server_cum_norm.R", local=T)$value
  
  source("server/server_cum_exact.R", local=T)$value
  
  source("server/server_meta_subgroup.R", local=T)$value
  
  source("server/server_meta_reg.R", local=T)$value
  
  source("server/server_meta_leave_one_out.R", local=T)$value
  
  source("server/server_report.R", local=T)$value
  
}