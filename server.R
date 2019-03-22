library(shiny)
library(shinydashboard) #tabBox()
library(DT)
library(openmetar)
library(dplyr)
library(grDevices) #png()

server <- function(input, output, session) {
  
  source("support/helpers.R", local = TRUE)$value
  
  source("server/server_data_csv.R", local = TRUE)$value
  
  source("server/server_meta_norm.R", local = TRUE)$value
  
  source("server/server_meta_exact.R", local = TRUE)$value
  
  source("server/server_cum_data.R", local = TRUE)$value
  
  source("server/server_cum_norm.R", local = TRUE)$value
  
  source("server/server_report.R", local = TRUE)$value
  
}