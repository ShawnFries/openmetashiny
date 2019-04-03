##########################################
#####           Upload csv           #####
##########################################

library(rhandsontable)
library(stringr)

# reactiveValues object for storing current data set.
vals <- reactiveValues(data=NULL, datar=NULL, dataescalc=NULL)                                          ####vals

# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
dataModal1 <- function(failed=F) {                                                        ####dataModal1
  modalDialog(
    fileInput("file1", "Choose CSV File",                                                       ####file1
              multiple=T,
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", T),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator", choices=c(Comma=",", Semicolon=";", Tab="\t"), selected=","),
    
    # Input: Select quotes ----
    radioButtons("quote", "Quote", choices=c(None="", "Double Quote"='"', "Single Quote"="'"), selected='"'),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons("disp", "Display", choices=c(Head="head", All="all"), selected="head"),
    if (failed)
      div(tags$b("Invalid name of data object", style="color: red;")),
    
    #easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okcsv", "OK")                                                              ####okcsv action button
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$upcsv, {                                                                     ####action button "upcsv" in ui_data.R
  showModal(dataModal1())                                                                       ####dataModal1
})



# When OK button is pressed, attempt to load the data set. If successful,
# remove the modal. If not show another modal, but this time with a failure
# message.
observeEvent(input$okcsv, {                                                                     ####okcsv action button observed
  # Check that data object exists and is data frame.
  if (!is.null(input$file1)) {
    vals$data <- read.csv(input$file1$datapath,                                                 ####file1 in up csv modal
                          header=input$header,
                          sep=input$sep,
                          quote=input$quote)
    vals$datar <- vals$data
    removeModal()
  } else {
    showModal(dataModal(failed=T))
  }
})

# Display selected data
output$hot <- renderRHandsontable({####dat_csv in ui_data.R
  DF <- data.frame()
  if (!is.null(vals$data)) {                                                       ####vals 1st line #### upload csv #### this file
    if (input$disp == "head") {
      DF <- head(vals$data)
      #DT::datatable(data.frame(Numbers=integer()), editable=T) # TODO: Fix this (add columns etc)
    } else {
      DF <- vals$data #DT::datatable(vals$data, editable=T))
    }
  } else {
    if (input$dataType == "proportion") {
      DF <- data.frame(names="Study A", year=as.integer(format(Sys.Date(), "%Y")), count=5, ni=10, stringsAsFactors=F)
    } else if (input$dataType == "mean") {
      DF <- data.frame(X=1, study=1, source="Location A", ni=10, mi=5, sdi=1, stringsAsFactors=F)
    } else if (input$dataType == "proportions") {
      DF <- data.frame(X=1, study=1, author="Author A", year=as.integer(format(Sys.Date(), "%Y")), ai=5, n1i=10, ci=3, n2i=15, stringsAsFactors=F)
    } else if (input$dataType == "means") {
      DF <- data.frame(X=1, study=1, source="Location A")#, n1i=10, m1i=5, sd1i=1, n2i=30, m2i=7, sd2i=2, stringsAsFactors=F)
    }
  }
  rhandsontable(DF, colHeaders=if (input$columnNames != "") str_trim(unlist(strsplit(input$columnNames,","))) else colnames(DF), stretchH="all", useTypes=F)
})




######################################
#####   change variable names    #####
######################################

dataModal0 <- function(failed=F) {
  modalDialog(
    selectInput("type_rename", "Type of data",
                choices=c("One proportion", "One mean", "Two proportions", "Two means"),
                selected="One proportion"),
    conditionalPanel(
      condition="input.type_rename == 'One proportion'",
      helpText("Measures for Dichotomous Variables: A meta-analysis may be conducted to aggregate studies that provide data for individual groups with respect to a dichotomous dependent variable. Here, one needs to specify xi and ni, denoting the number of individuals experiencing the event of interest and the total number of individuals, respectively.")),
    conditionalPanel(
      condition="input.type_rename == 'One mean'",
      helpText("Measures for Quantitative Variables: The goal of a meta-analysis may also be to characterize individual groups, where the response, characteristic, or dependent variable assessed in the individual studies is measured on some quantitative scale. In the simplest case, the raw mean for the quantitative variable is reported for each group, which then becomes the observed outcome for the meta-analysis. Here, one needs to specify mi, sdi, and ni for the observed means, the observed standard deviations, and the sample sizes, respectively.")),
    
    selectInput("var1", "Variable to rename",
                choices=names(vals$datar),
                selected=names(vals$datar)[1]),
    textInput("var1to", "Rename variable to:", "xi"),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_data_rename", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$data_rename, {
  showModal(dataModal0())
})

observeEvent(input$ok_data_rename, {
  
  names(vals$datar)[names(vals$datar) == input$var1]=input$var1to

  output$dat_csv_renamed <- renderTable({
    vals$datar
  })
  
  removeModal()
})









##################################
####         dynamic UI       ####
##################################

# output$dat_or_escalc<-renderUI({
#   if(is.null(input$oknorm_escalc)){
#     tableOutput("dat_csv")
#   }else{
#     tableOutput("escalcdat")
#   }
# })




