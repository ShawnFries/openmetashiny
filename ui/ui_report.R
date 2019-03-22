fluidPage(
  textAreaInput("reportTitle", "Type report title here", "Title ... "),
  textAreaInput("reportAuthor", "Type names of report authors here", "Authors ... "),
  checkboxInput("includeTimeCode", "Check to include date and time", value = FALSE),
  textAreaInput("reportIntro", "Type introduction to report here", "Introduction ... "),
  downloadButton("report", "Download report")
)