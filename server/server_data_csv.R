##########################################
#####           Upload csv           #####
##########################################
#TODO: Needs to support editing before adding CSV... if doing from scratch
# https://stackoverflow.com/questions/41161822/filter-rows-in-rhandsontable-in-r-shiny

library(rhandsontable)
library(stringr)

csv_button_pressed <- F

# reactiveValues object for storing current data set.
vals <- reactiveValues(data=NULL, datar=NULL, dataescalc=NULL)                                          ####vals

# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
dataModal1 <- function(failed=F) {                                                        ####dataModal1
  modalDialog(
    fileInput("file1", "Choose CSV File", multiple=T, accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
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
    if (failed) div(tags$b("Invalid name of data object", style="color: red;")),
    
    #easyClose = TRUE,
    footer=tagList(modalButton("Cancel"), actionButton("okcsv", "OK"))
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
    vals$data <- read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)
    if (dataType$type == "proportion" && length(intersect(c("count",
                                                            "xi",
                                                            "counts",
                                                            "x_i",
                                                            "x_is",
                                                            "xis",
                                                            "x",
                                                            "xs",
                                                            "prop",
                                                            "props",
                                                            "proportions",
                                                            "proportion",
                                                            "x/n",
                                                            "x / n",
                                                            "X / N",
                                                            "x / n",
                                                            "n",
                                                            "ns",
                                                            "ni",
                                                            "nis",
                                                            "n_is",
                                                            "n_i",
                                                            "sample size",
                                                            "sample sizes"
                                                           ), colnames(vals$data)
                                                         )
                                               ) == 2
       ) {  # Exactly 2 matches so we can simply calculate the other one (count, N, proportion)
      if (length(intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs", "n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"),
                           colnames(vals$data)
                          )
                ) == 2
         ) {  # missing proportion column
        vals$data$proportion <- vals$data[[intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs"), colnames(vals$data))]] /
                                vals$data[[intersect(c("n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"), colnames(vals$data))]]
        
      } else if (length(intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n", "n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"),
                                  colnames(vals$data)
                                 )
                       ) == 2
                ) {  # missing count column
        vals$data$count <- vals$data[[intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n"), colnames(vals$data))]] *
                           vals$data[[intersect(c("n", "ns", "ni", "nis", "n_is", "n_i", "sample size", "sample sizes"), colnames(vals$data))]]
      } else {  # missing sample size
        vals$data$ni <- vals$data[[intersect(c("count", "xi", "counts", "x_i", "x_is", "xis", "x", "xs"), colnames(vals$data))]]  /
                        vals$data[[intersect(c("prop", "props", "proportions", "proportion", "x/n", "x / n", "X / N", "x / n"), colnames(vals$data))]]
      }
      # Still one proportion data type here
      # Compute lower and upper bounds from t distribution given sample size and proportion 
      error <- mapply(function(x, y) qt(0.975, x - 1) * sqrt(y * (1 - y) / x), vals$data$ni, vals$data$proportion)
      vals$data$lower <- pmax(0, vals$data$proportion - error)
      vals$data$upper <- pmin(1, vals$data$proportion + error)
      # Add back calculation given lower and upper bounds and sample size OR sd?
    } else if (dataType$type == "proportions" && length(intersect(c("ai", "n1i", "ci", "n2i"), colnames(vals$data))) == 4) {# TODO: Add additional checks for proper columns being there...
      vals$data$odds_ratio <- vals$data$ai / vals$data$n1i / (vals$data$ci / vals$data$n2i)
      sds <- sqrt(1 / vals$data$ai + 1 / vals$data$ci + 1 / (vals$data$n1i - vals$data$ai) + 1 / (vals$data$n2i - vals$data$ci))  # Asymptotic approximation..
      error <- mapply(function(x, y) qt(0.975, x - 1) * y, vals$data$n1i + vals$data$n2i, sds)
      vals$data$lower <- vals$data$odds_ratio - error
      vals$data$upper <- vals$data$odds_ratio + error
    } else if (dataType$type == "means" && length(intersect(c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i"), colnames(vals$data))) == 6) {# TODO: Add checks for proper columns being there...
      vals$data$mean_difference <- vals$data$m2i - vals$data$m1i
      sds <- vals$data$sd1i / sqrt(vals$data$n1i) + vals$data$sd2i / sqrt(vals$data$n2i)
      error <- mapply(function(x, y) qt(0.975, x - 1) * y, vals$data$n1i + vals$data$n2i, sds)
      vals$data$lower <- vals$data$mean_difference - error
      vals$data$upper <- vals$data$mean_difference + error
    } else if (dataType$type == "proportion" && length(intersect(c("lower",
                                                             "upper"), colnames(vals$data))) == 2 && length(intersect(c("count",
                                                                                                                                 "xi",
                                                                                                                                 "counts",
                                                                                                                                 "x_i",
                                                                                                                                 "x_is",
                                                                                                                                 "xis",
                                                                                                                                 "x",
                                                                                                                                 "xs",
                                                                                                                              "n",
                                                                                                                        "ns",
                                                                                                                        "ni",
                                                                                                                        "nis",
                                                                                                                        "n_is",
                                                                                                                        "n_i",
                                                                                                                        "sample size",
                                                                                                                        "sample sizes"
  ), colnames(vals$data)
  )) == 1) {# TODO: Add checks for proper columns being there...
  #TODO: What are the most important back calculations to do? Perhaps given lower AND upper AND sample size, the actual proportion (easy) and x (count) columns..
    #also could calculate sample size from upper and lower somehow (next most important)..
    vals$data$proportion <- (vals$data$upper + vals$data$lower) / 2
    if (!is.null(vals$data$ni)) {
      vals$data$count <- vals$data$proportion * vals$data$ni
    } else if (!is.null(vals$data$count)) {
      vals$data$ni <- vals$data$count / vals$data$proportion
    }
    #error <- mapply(function(x, y) qt(0.975, x - 1) * y / sqrt(x), vals$data$ni, vals$data$sdi)
   # vals$data$lower <- vals$data$mi - error
   # vals$data$upper <- vals$data$mi + error
  } else if ((dataType$type == "proportion" && length(intersect(c("lower",
                                                                   "upper"), colnames(vals$data))) == 2 && length(intersect(c("count",
                                                                                                                                     "xi",
                                                                                                                                     "counts",
                                                                                                                                     "x_i",
                                                                                                                                     "x_is",
                                                                                                                                     "xis",
                                                                                                                                     "x",
                                                                                                                                     "xs",
                                                                                                                              "n",
                                                                                                                              "ns",
                                                                                                                              "ni",
                                                                                                                              "nis",
                                                                                                                              "n_is",
                                                                                                                              "n_i",
                                                                                                                              "sample size",
                                                                                                                              "sample sizes"
                                                                   ), colnames(vals$data)
                                                                   )) == 0)) {
    #Unknown sample size, so find t distribution that fits from upper/lower bounds to derive sample size etc.
    vals$data$proportion <- (vals$data$upper + vals$data$lower) / 2
    # Probably need to do some kind of binary search to find the degrees of freedom here.. given error (difference of proportion and upper/lower for 95% interval) and probably some initial guess
    #error <- mapply(function(x, y) qt(0.975, x - 1) * sqrt(y * (1 - y) / x), vals$data$ni, vals$data$proportion)
    df_estimates <- rep(50, length(vals$data$upper))  # Initialize df guess to 50 for all rows
    real_errors <- vals$data$upper - vals$data$proportion
    error_estimates <- mapply(function(x, y) qt(0.975, x - 1) * sqrt(y * (1 - y) / x), df_estimates, vals$data$proportion)
    previous_error_estimates <- 0
    # Increase df if estimated error too high, decrease it if too low (a sort of binary search for t distribution df). Stop if exact df value found or stops improving
    while (real_errors != error_estimates && previous_error_estimates != error_estimates) {
      previous_error_estimates <- error_estimates
      df_estimates <- mapply(function(x, y) if (x > y) round(mean(z, 0))
                                           else if (x < y) round(z, 10*z)
                                           else z
                                           , real_errors, error_estimates, df_estimates)
      error_estimates <- mapply(function(x, y) qt(0.975, x - 1) * sqrt(y * (1 - y) / x), df_estimates, vals$data$proportion)
    }
    vals$data$ni <- df_estimates
    vals$data$count <- vals$data$ni * vals$data$proportion
  } # TODO: Add for SMD (lower/upper for SMD) and for diagnostic (lower/upper for each of sensitivity and specificity)
    vals$datar <- vals$data
    removeModal()
  } 
  else {
    showModal(dataModal(failed=T))
  }
  csv_button_pressed <<- T
  column_names <- colnames(vals$datar)
  changeDataType(if ("m1i" %in% column_names) "means"
                 else if ("mi" %in% column_names) "mean"
                 else if ("t2i" %in% column_names) "event counts"
                 else if ("ti" %in% column_names) "event count"
                 else if ("count" %in% column_names) "proportion"
                 else if ("x2i" %in% column_names) "proportions"
                 else if ("ri" %in% column_names) "regression coefficient"
                 else if ("ai" %in% column_names) "cronbach alpha"
                 else F  # Pass false so that changeDataType does not execute (per an if-statement within it) if no matching data type found
  )
})

hot <- reactiveValues()

observe({
  if (!is.null(input$hot)) hot$data <- hot_to_r(input$hot)
  
  # Working! Except, now need to make removing something from filter actually flow out of the table etc...
  if (!is.null(hot$data)) {
    columns <- colnames(hot$data)
    number_of_columns <<- length(columns)
  }
  row_filters <- list()
  
  output$row_filters <- renderUI ({
    if (input$enable_filtering) {
      for (i in 1:number_of_columns) {
        row_values <- sort(unique(hot$data[[i]]))
        if (!is.null(row_values) && !is.null(number_of_columns) && number_of_columns > 0) {
          row_filters[[columns[i]]] <<- ({
            selectInput(paste("row_filters", i, sep="_"), paste("Row filter", columns[i]), row_values, row_values, multiple=T)
          })
        }
      }
      
      do.call(tagList, row_filters)
    } else if (length(row_filters) > 0) {  # If row filters have been set, reset them to blank if filters are later disabled to prevent problems if editing data
      for (i in 1:number_of_columns) {
        row_values <- vector()
        if (!is.null(row_values) && !is.null(number_of_columns) && number_of_columns > 0) {
          row_filters[[columns[i]]] <<- ({
            # Refresh, but silently in the background(hidden element that user can't see). Needs to refresh to blank in case user edits (so real filters won't interfere with their edits)
            hidden(selectInput(paste("row_filters", i, sep="_"), paste("Row filter", columns[i]), row_values, row_values, multiple=T))
          })
        }
      }
      
      do.call(tagList, row_filters)
    }
  })
  
  # for (i in 1:number_of_columns) {
  #   local({
  #     local_i <- i
  #     filter_name <- paste("row_filters", local_i, sep="_")
  #     output[[filter_name]] <- renderPlot({
  #       forest(res_subgroup[[local_i]], refline=NA, level=conflevel, digits=input$digits_subgroup,
  #              atransf=if (input$atransf_subgroup != "none") get(paste0("transf.", input$atransf_subgroup)))
  #       grid.text(paste("Subgroup:", unique_subgroups[local_i]), .5, .9, gp=gpar(cex=2))
  #     })
  #   })
  # }
  # 
  # output$msummary_norm_subgroup <- renderPrint({
  #   subgroup_index <- 0
  #   for (subgroup_analysis in res_subgroup) {
  #     subgroup_index <- subgroup_index + 1
  #     subgroup_name <- unique_subgroups[subgroup_index]
  #     print(paste("Subgroup:", subgroup_name))
  #     print(subgroup_analysis)
  #   }
  # })
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
      if (input$is_real_data == "sample") {
        DF <- get(input$sample_dataset) # I coded this input variable to the exact name of the built-in Metafor datasets. Get, as the name suggests, gets the dataframe of that name
      } else if (dataType$type == "proportion") {
        DF <- data.frame(names="Study A", year=as.integer(format(Sys.Date(), "%Y")), count=5, ni=10, proportion=0.5, stringsAsFactors=F)
      } else if (dataType$type == "mean") {
        DF <- data.frame(X=1, study=1, source="Location A", ni=10, mi=5, sdi=1, stringsAsFactors=F)
      } else if (dataType$type == "proportions") {
        DF <- data.frame(X=1, study=1, author="Author A", year=as.integer(format(Sys.Date(), "%Y")), ai=5, n1i=10, ci=3, n2i=15, stringsAsFactors=F)
      } else if (dataType$type == "means") {
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
  if (!(is.null(input$hot) || csv_button_pressed || nrow(hot_to_r(input$hot)) == 0)) {
    hot$data <- hot_to_r(input$hot)
   # print(3)
    #print(hot$data)
    # Bit of a hack to prevent the initial setup e.g. "Study A" row filters from causing the table to overwrite at the instant a new CSV is uploaded..
    # Ideally we'd hold off on updating this until the row filters have refreshed to prevent them from overwriting (when uploading a CSV first replace row filters THEN update this table)
    # TODO: Give a way of retrieving the filter values after selected instead of just deleting them? (e.g. undo button for filters..)
    # TODO: Cleaner way of selecting filters? Dropdown menus for each/better yet one dropdown for entire section instead of taking up a screen's worth of text above the table...
    if (input$enable_filtering && !is.null(colnames(hot$data)) && !is.null(row_filters) && !csv_button_pressed && length(row_filters) > 0 && nrow(hot$data) > 0) {
     # print(4)
      DF <- hot$data
     # print(number_of_columns)
     # print(DF)
          for (i in 1:number_of_columns) {
            row_values <- sort(input[[paste("row_filters", i, sep="_")]])
           # if (DF == hot_to_r(input$hot)) { # This is necessary in case user edits while this code would be running (force synchronization otherwise get to it next time table refreshes)
              if (!is.null(row_values) && !is.null(DF) && ncol(DF) > 0 && length(row_values) > 0 && nrow(DF) > 0) {
              #  print(i)
              #  print("data")
              #  print(DF)
              #  print("column")
              #  print(DF[[i]])
              #  print("filter values")
              #  print(row_values)
                DF <- DF[DF[[i]] %in% row_values, ]
               # print(DF)
              } else {
                break  # Stop filtering, they're blank or there is some other problem
              }
            #} else {
         #   
         }
        #  print(row_filters[[column]])
         # DF <- hot$data[hot$data[[column]] %in% row_filters[[column]]]
       #   print(2)
         # print(DF)
          hot$table <- rhandsontable(data.frame(DF), stretchH="all", useTypes=F)
    }
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