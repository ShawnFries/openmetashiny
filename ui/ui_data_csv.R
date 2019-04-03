library(rhandsontable)

fluidPage(
  fluidRow(
    actionButton("upcsv", "Upload csv")
  ),
  fluidRow(
    textInput("columnNames", "Enter Column Names (separated by commas) to change", value="", placeholder="A, B")
  ),
  fluidRow(
    rHandsontableOutput("hot")
  )
)
   

  
### EXCEL-like data entry
### https://jrowen.github.io/rhandsontable/#shiny  
