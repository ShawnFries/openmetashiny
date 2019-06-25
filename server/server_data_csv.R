##########################################
#####           Upload csv           #####
##########################################
#TODO: Needs to support editing before adding CSV... if doing from scratch

library(rhandsontable)
library(stringr)

csv_button_pressed <- F

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
    if (input$dataType == "proportion" & length(
                                    intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs", "prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n", "n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"),
                                    colnames(vals$data
                                    )
      )) == 2) {  # Exactly 2 matches so we can simply calculate the other one (count, N, proportion)
      if (length(intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs", "n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"),
                    colnames(vals$data
                    ))) == 2) {  # missing proportion column
        vals$data$proportion <- vals$data[[intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs"), colnames(vals$data))]] /
                                vals$data[[intersect(c("n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"), colnames(vals$data))]]
        
      } else if (length(intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n", "n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"),
                                  colnames(vals$data
                                  ))) == 2) {  # missing count column
        vals$data$count <- vals$data[[intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n"), colnames(vals$data))]] *
                           vals$data[[intersect(c("n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"), colnames(vals$data))]]
      } else {  # missing sample size
        vals$data$ni <- vals$data[[intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs"), colnames(vals$data))]]  /
                        vals$data[[intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n"), colnames(vals$data))]]
      }
      error <- mapply(function(x, y) qt(0.975, x - 1) * sqrt(y * (1 - y) / x), vals$data$ni, vals$data$proportion)
      vals$data$lower <- pmax(0, vals$data$proportion - error)
      vals$data$upper <- pmin(1, vals$data$proportion + error)
    } else if (input$dataType == "mean" & length(intersect(c("ni", "sdi"), colnames(vals$data))) == 2) {# TODO: Add checks for proper columns being there...
      error <- mapply(function(x, y) qt(0.975, x - 1) * y / sqrt(x), vals$data$ni, vals$data$sdi)
      vals$data$lower <- vals$data$mi - error
      vals$data$upper <- vals$data$mi + error
    } else if (input$dataType == "proportions" & length(intersect(c("ai", "n1i", "ci", "n2i"), colnames(vals$data))) == 4) {# TODO: Add checks for proper columns being there...
      vals$data$odds_ratio <- vals$data$ai / vals$data$n1i / (vals$data$ci / vals$data$n2i)
      sds <- sqrt(1 / vals$data$ai + 1 / vals$data$ci + 1 / (vals$data$n1i - vals$data$ai) + 1 / (vals$data$n2i - vals$data$ci))  # Asymptotic approximation..
      error <- mapply(function(x, y) qt(0.975, x - 1) * y, vals$data$n1i + vals$data$n2i, sds)
      vals$data$lower <- vals$data$odds_ratio - error
      vals$data$upper <- vals$data$odds_ratio + error
    } else if (input$dataType == "means" & length(intersect(c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i"), colnames(vals$data))) == 6) {# TODO: Add checks for proper columns being there...
      vals$data$mean_difference <- vals$data$m2i - vals$data$m1i
      sds <- vals$data$sd1i / sqrt(vals$data$n1i) + vals$data$sd2i / sqrt(vals$data$n2i)
      error <- mapply(function(x, y) qt(0.975, x - 1) * y, vals$data$n1i + vals$data$n2i, sds)
      vals$data$lower <- vals$data$mean_difference - error
      vals$data$upper <- vals$data$mean_difference + error
    }  # TODO: Add for SMD (lower/upper for SMD) and for diagnostic (lower/upper for each of sensitivity and specificity)
    vals$datar <- vals$data
    removeModal()
  } else {
    showModal(dataModal(failed=T))
  }
  csv_button_pressed <<- T
})

hot <- reactiveValues()

observe({
  if (!is.null(input$hot)) hot$data <- hot_to_r(input$hot)
})

# Display selected data

output$hot <- renderRHandsontable({####dat_csv in ui_data.R
  DF <- data.frame()
 # print(csv_button_pressed)
  if (!is.null(vals$data) & csv_button_pressed) {                                                       ####vals 1st line #### upload csv #### this file
    if (input$disp == "head") {
      DF <- head(vals$datar)
      #DT::datatable(data.frame(Numbers=integer()), editable=T) # TODO: Fix this (add columns etc)
    } else {
      DF <- vals$datar #DT::datatable(vals$data, editable=T))
    }
    #print(csv_button_pressed)
   # print(DF)
  } else if (is.null(vals$datar)) {
      if (input$dataType == "proportion") {
        DF <- data.frame(names="Study A", year=as.integer(format(Sys.Date(), "%Y")), count=5, ni=10, proportion=0.5, stringsAsFactors=F)
      } else if (input$dataType == "mean") {
        DF <- data.frame(X=1, study=1, source="Location A", ni=10, mi=5, sdi=1, stringsAsFactors=F)
      } else if (input$dataType == "proportions") {
        DF <- data.frame(X=1, study=1, author="Author A", year=as.integer(format(Sys.Date(), "%Y")), ai=5, n1i=10, ci=3, n2i=15, stringsAsFactors=F)
      } else if (input$dataType == "means") {
        DF <- data.frame(X=1, study=1, source="Location A")#, n1i=10, m1i=5, sd1i=1, n2i=30, m2i=7, sd2i=2, stringsAsFactors=F)
      }
    } else {
      DF <- hot$data
  }
  if (input$columnNames != "") {
    new_column_names <- str_trim(unlist(strsplit(input$columnNames,",")))
    new_column_names_length <- length(new_column_names)
    for (i in 1:new_column_names_length) {
      if (new_column_names[i] != "") colnames(DF)[i] <- new_column_names[i]
    }
  }
  hot$table <- rhandsontable(data.frame(DF), stretchH="all", useTypes=F)
  if (!(is.null(input$hot) | csv_button_pressed)) {
    hot$data <- hot_to_r(input$hot)
    # if (!is.null(hot$data_cache) && !identical(hot$data_cache, hot$data)) {
    #   print(hot$data)
    #   if (hot$data_cache$proportion != hot$data$proportion) { # TODO: Check if any proportion-like column name not only "proportion"
    #     #print(2)
    #     hot$data$count <- hot$data$proportion * hot$data$ni
    #   } else if (hot$data_cache$count != hot$data$count) {
    #     #print(3)
    #     hot$data$proportion <- hot$data$count / hot$data$ni
    #   } else if (hot$data_cache$ni != hot$data$ni) {
    #    # print(4)
    #     hot$data$proportion <- hot$data$count / hot$data$ni
    #   }
    # } 
    # print(hot$data)
    #
    # print(vals$data)
    vals$data <- hot$data
    vals$datar <- vals$data
    # Define minimum and maximum values for auto-data type selection (actually this would mean the data already follows that transformation and we need to reverse it)
   # proportion_column <- grep('prop', colnames(vals$data), T)[1]
    #print(proportion_column)
   # if (!is.na(proportion_column)) {
     # min_proportion <<- min(vals$data[proportion_column])
     # max_proportion <<- max(vals$data[proportion_column])
   # }
    #print(max_proportion)
    #print(exists("min_proportion") && (min_proportion < 0 || max_proportion > 1))
  } else {
    csv_button_pressed <<- F
  }
  hot$table
})