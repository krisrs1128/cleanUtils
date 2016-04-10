
################################################################################
# Server for clean data processing shiny app
################################################################################

library("shiny")
library("data.table")
library("cleanUtils")

# increase allowed file upload sizes
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output) {
  # get curent options
  blanks_to_nas <- reactive({ input$blanks_to_nas == "Yes" })
  convert_class <- reactive({ input$convert_class == "Yes" })
  clean_ages <- reactive({ input$clean_ages == "Yes" })
  clean_dates <- reactive({ input$clean_dates == "Yes" })
  levels_to_int <- reactive({ input$levels_to_int == "Yes" })
  remove_constant_cols <- reactive({ input$remove_constant_cols == "Yes" })
  sanitize_colnames <- reactive({ input$sanitize_colnames == "Yes" })
  sort_rows <- reactive({ input$sort_rows == "Yes" })

  # do the processing
  processed_data <- reactive({
    opts <- list(blanks_to_nas = blanks_to_nas(), convert_class = convert_class(),
                 clean_ages = clean_ages(), clean_dates = clean_dates(),
                 levels_to_int = levels_to_int(), remove_constant_cols(),
                 sanitize_colnames = sanitize_colnames(), sort_rows = sort_rows())
    if (is.null(input$raw_data)) {
      return(NULL)
    }
    cur_data <- fread(input$raw_data$datapath, colClasses = "character")
    preprocess_data(cur_data, opts)
  })

  # print current data version to the UI
  output$text_output <- renderPrint( {
    processed_data()
  })

  # different kinds of downloads
  output$download_data <- downloadHandler(
    filename = function() paste0("processed_", input$raw_data$name),
    content = function(file) {
      X <- processed_data()
      write.csv(X$X, file = file, row.names = FALSE)
    })

  output$download_levels <- downloadHandler(
    filename = function() paste0("levels_", input$raw_data$name),
    content = function(file) {
      X <- processed_data()
      write.csv(X$levels_recode, file = file, row.names = FALSE)
    })

  output$download_rdata <- downloadHandler(
    filename = function() paste0("processed_", basename(input$raw_data$name), ".RData"),
    content = function(file) {
      X <- processed_data()
      save(X, file = file)
    })
})
