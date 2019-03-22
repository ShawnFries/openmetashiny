fluidPage(
  fluidRow(
    actionButton("upcsv", "Upload csv")
  ),
  fluidRow(
    tableOutput("dat_csv"),
    actionButton("data_rename", "Rename variables"),
    tableOutput("dat_csv_renamed")
  )
)
   

  
### EXCEL-like data entry
### https://jrowen.github.io/rhandsontable/#shiny  
