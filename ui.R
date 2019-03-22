library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(openmetar)
library(dplyr)

# dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem(div(img(src="meta_analysis.png"), "Data"), tabName = "data"),
#       #add image to menuItem text
#       #https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
#       menuItem("Meta-analysis", tabName = "meta", icon = icon("th")),
#       menuItem("Cumulative meta-analysis", tabName = "cumulative", icon = icon("dashboard")),
#       menuItem("Subgroup meta-analysis", tabName = "subgroup", icon = icon("th")),
#       menuItem("Meta-regression", tabName = "meta-reg", icon = icon("dashboard")),
#       menuItem("Leave-one-out meta-analysis", tabName = "leave-one-out", icon = icon("dashboard"))
#     )
#   ),#ends dashboardSidebar
#   dashboardBody(
#     tabItems(
#       tabItem(tabName="data"),
#       tabItem(tabName="meta"),
#       tabItem(tabName="cumulative"),
#       tabItem(tabName="subgroup"),
#       tabItem(tabName="meta-reg"),
#       tabItem(tabName="leave-one-out")
#     )
#   )#ends dashboardBody
# )

function(request){
bootstrapPage('',

navbarPage(#div(id="title", img(src="meta.png")), #logo
           #"OpenMetaAnalyst", #just the words
           title=HTML("<a style=font-size:150%;color:black;
                      href=\"http://www.cebm.brown.edu/openmeta\">OpenMeta[Analyst]</a>"),
           
           
           navbarMenu(div(id="data_div", img(src="data-512.png")),
                      tabPanel("View Data",
                               source("ui/ui_data_csv.R", local = TRUE)$value)
           ),#navbarMenu "Data"
           bsTooltip("data_div", "Data", "right", options = list(container = "body")),
           
           
           
           navbarMenu(div(id="meta_analysis_div", img(src="meta_analysis.png")),
                      tabPanel("Normal approximation",
                               source("ui/ui_meta_norm.R", local = TRUE)$value),
                      tabPanel("Exact likelihood",
                               source("ui/ui_meta_exact.R", local = TRUE)$value)
           ),#tabPanel "Meta-analysis"
           #https://stackoverflow.com/questions/44953873/add-tooltip-to-tabs-in-shiny
           #add tooltip to the Meta-analysis tabPanel
           bsTooltip("meta_analysis_div", "Meta-analysis", "right", options = list(container = "body")),
           
           
           
           navbarMenu(div(id="cum_meta_analysis_div", img(src="cum_meta_analysis.png")),
                      tabPanel("Accumulative data",
                               source("ui/ui_cum_data.R", local = TRUE)$value),
                      tabPanel("Normal approximation",
                               source("ui/ui_cum_norm.R", local = TRUE)$value),
                      tabPanel("Exact likelihood",
                               source("ui/ui_cum_exact.R", local = TRUE)$value)
           ),#tabPanel "Cumulative meta-analysis"
           bsTooltip("cum_meta_analysis_div", 
                     "Cumulative meta-analysis", "right", options = list(container = "body")),
           
           
           
           tabPanel(div(id="subgroup_meta_analysis_div", img(src="subgroup_ma.png"))
           ),#tabPanel "Subgroup meta-analysis"
           bsTooltip("subgroup_meta_analysis_div", 
                     "Subgroup meta-analysis", "right", options = list(container = "body")),
           
           
           
           tabPanel(div(id="meta_regression_div", img(src="meta_reg.png"))
           ),#tabPanel "Meta-regression"
           bsTooltip("meta_regression_div", "Meta regression", "right", options = list(container = "body")),
           
           
           
           tabPanel(div(id="leave_one_out_div", img(src="leave_one_out.png"))
           ),#tabPanel "Leave-one-out meta-analysis"
           bsTooltip("leave_one_out_div", 
                     "Leave-one-out meta-analysis", "right", options = list(container = "body")),
           
           
           tabPanel(div(id="report_div", img(src="report.png")),
                    source("ui/ui_report.R", local = TRUE)$value
           ),#tabPanel "Report"
           bsTooltip("report_div", 
                     "Generate report", "right", options = list(container = "body"))
           
),#end navbarPage


tags$style(type='text/css', '.navbar { 
                 font-family: Times;
                 font-size: 30px;}')

)
}
