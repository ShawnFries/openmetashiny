library(rhandsontable)

fluidPage(
  fluidRow(
    actionButton("upcsv", "Upload csv")
  ),
  fluidRow(
    rHandsontableOutput("hot")
  )
)
   

  
### EXCEL-like data entry
### https://jrowen.github.io/rhandsontable/#shiny  
