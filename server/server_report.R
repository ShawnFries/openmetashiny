############# downloadable report
#############https://shiny.rstudio.com/articles/generating-reports.html
output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.html",
  content = function(file) {
    ## create a temporary directory in case don't have write access to the target directory
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    ## reactive objects cannot directly be passed to report.Rmd as parameters 
    ## they have to be called and defined with a name before being passed to parameters below
    res<-res()

    
    ## Set up parameters to pass to .Rmd document
    ## all input variables used to generate the plot/table output has to be defined as 
    ## parameters here and passed to report.Rmd with the exception of the actionButtons 
    ## since they only change the state of the application but not the output themselves
    params <- list(
      res = res(),
      
      reportTitle = input$reportTitle, 
      reportAuthor = input$reportAuthor, 
      includeTimeCode = input$includeTimeCode, 
      reportIntro = input$reportIntro
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)