library(rhandsontable)

fluidPage(
  fluidRow(actionButton("upcsv", "Upload csv")),
  fluidRow(textInput("columnNames", "To edit column names enter new values here (separated by commas)", value="", placeholder="A, B")),
  fluidRow(checkboxInput("enable_filtering",
                         "Enable row filtering? Leave this disabled while manually editing any data below
                         (which you can do in Excel-like double click fashion; right click to add/remove rows or columns)"
                        )),
  fluidRow(fluidRow(uiOutput("row_filters"))),
  fluidRow(rHandsontableOutput("hot"))
)