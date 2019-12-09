library(rhandsontable)

fluidPage(
  fluidRow(actionButton("upcsv", "Upload csv")),
 # fluidRow(actionButton("sample_data", "Import example data from a real meta-analysis")),
 # TODO: Ideally, use the above and have a modal for importing sample data (instead of a separate page) as well as one for the data type (really only needed for a blank template of particular data)
  fluidRow(textInput("columnNames", "To edit column names enter new values here (separated by commas)", value="", placeholder="A, B")),
  fluidRow(checkboxInput("enable_filtering",
                         "Enable row filtering? Leave this disabled while manually editing any data below
                         (which you can do in Excel-like double click fashion; right click to add/remove rows or columns)"
                        )),
  fluidRow(fluidRow(uiOutput("row_filters"))),
  fluidRow(rHandsontableOutput("hot")),
  # Use HTML to print raw text into app
  fluidRow(HTML("Right click a cell in the table for additional options, e.g. adding rows or columns"))
)