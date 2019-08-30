library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard) #tabBox()
library(rhandsontable)
library(BiasedUrn)
#library(DT)
library(metafor)
#library(dplyr)
library(grDevices) #png()
library(gridExtra)
library(grid)

server <- function(input, output, session) {
  app <- reactiveValues(page=1)
 # print(app)
 # print(language)

 # observe({
    #toggleState(id="backButton", condition=app$page > 1)
    #toggleState("continueButton", app$page == 1)
    #toggleState("continueButton_real_or_sample", app$page == 2)
    #toggleState("continueButton2", app$page == 3)
 # })

  nextStep <- function(direction) {
    app$page <- app$page + direction
    #print(app$page)
  }

  #observeEvent(input$backButton, nextStep(-1))
  observeEvent(input$continueButton, nextStep(1))
  observeEvent(input$continueButton_real_or_sample, nextStep(1))
  observeEvent(input$continueButton_real_or_sample_de, nextStep(1))
  observeEvent(input$continueButton2, nextStep(1))
  observeEvent(input$continueButton2_de, nextStep(1))
  observeEvent(input$continueButton_sample_data, nextStep(1))

  output$page <- renderText({
    app$page
    #print(app$page)
  })
  
  outputOptions(output, "page", suspendWhenHidden=F)
  
  output$main_page <- renderUI({
    if(app$page > 3) {
                     
                     navbarPage(#div(id="title", img(src="meta.png")), #logo
                       #"OpenMetaAnalyst", #just the words
                       title=HTML("<a style=font-size:150%;color:black;href=\"http://www.cebm.brown.edu/openmeta\">OpenMeta[Analyst]</a>"),
                       
                       tabPanel(div(id="data_div", img(src="data-512.png")), source("ui/ui_data_csv.R", local=T)$value), #navbarMenu "Data"
                       bsTooltip("data_div", ifelse(input$language == "en", "View Data", "Sehen Daten"), "right", options=list(container="body")),
                       
                       navbarMenu(div(id="meta_analysis_div", img(src="meta_analysis.png")),
                                  tabPanel(ifelse(input$language == "en", "Normal approximation (linear model)", "Normaler Annäherungswerte (lineares Modell)"),
                                           source("ui/ui_meta_norm.R", local=T)$value
                                          ),
                                  tabPanel(ifelse(input$language == "en", "Exact likelihood (generalized linear model)", "Exact likelihood (generalized lineares Modell)"),
                                           source("ui/ui_meta_exact.R", local=T)$value),
                                  tabPanel(ifelse(input$language == "en",
                                                  "Multilevel model (multivariate/phylogenetic/hierarchical linear model)",
                                                  "Multilevel Modell (multivariate/phylogenetic/hierarchical lineares Modell)"
                                                 ),
                                           source("ui/ui_meta_multilevel.R", local=T)$value)
                       ),
                       #tabPanel "Meta-analysis"
                       #https://stackoverflow.com/questions/44953873/add-tooltip-to-tabs-in-shiny
                       #add tooltip to the Meta-analysis tabPanel
                       bsTooltip("meta_analysis_div", "Meta-analysis", "right", options=list(container="body")),
                       
                       navbarMenu(div(id="cum_meta_analysis_div", img(src="cum_meta_analysis.png")),
                                  tabPanel("Accumulative data", source("ui/ui_cum_data.R", local=T)$value),
                                  tabPanel("Normal approximation", source("ui/ui_cum_norm.R", local=T)$value)#,
                                  #TODO: Find a way to implement this? cumul not supported for rma.glmm.. would have to do manually
                                  #tabPanel("Exact likelihood", source("ui/ui_cum_exact.R", local=T)$value)
                       ),#tabPanel "Cumulative meta-analysis"
                       bsTooltip("cum_meta_analysis_div", "Cumulative meta-analysis", "right", options=list(container="body")),
                       
                       tabPanel(div(id="subgroup_meta_analysis_div", img(src="subgroup_ma.png")), source("ui/ui_meta_subgroup.R", local=T)$value
                       ),#tabPanel "Subgroup meta-analysis"
                       bsTooltip("subgroup_meta_analysis_div", "Subgroup meta-analysis", "right", options=list(container="body")),
                       
                       tabPanel(div(id="meta_regression_div", img(src="meta_reg.png")), source("ui/ui_meta_reg.R", local=T)$value),#tabPanel "Meta-regression"
                       bsTooltip("meta_regression_div", "Meta regression", "right", options=list(container="body")),
                       
                       tabPanel(div(id="leave_one_out_div", img(src="leave_one_out.png")), source("ui/ui_leave_one_out.R", local=T)$value),#tabPanel "Leave-one-out meta-analysis"
                       bsTooltip("leave_one_out_div", "Leave-one-out meta-analysis", "right", options=list(container="body")),
                       
                       tabPanel(div(id="report_div", img(src="report.png")), source("ui/ui_report.R", local=T)$value),#tabPanel "Report"
                       bsTooltip("report_div", "Generate report", "right", options=list(container="body")))
    
  }
    })

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