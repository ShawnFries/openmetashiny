library(rhandsontable)

fluidPage(
  fluidRow(
    actionButton("upcsv", "Upload csv")
  ),
  fluidRow(
    textInput("columnNames", "To edit column names enter new values here (separated by commas)", value="", placeholder="A, B")
  ),
  fluidRow(
    rHandsontableOutput("hot")
  )
)
   

  
### EXCEL-like data entry
### https://jrowen.github.io/rhandsontable/#shiny  
