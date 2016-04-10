
################################################################################
# UI for clean data processing shiny app
################################################################################

library("shiny")

shinyUI(fluidPage(
  titlePanel("Process Data"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("download_data", "Download csv"),
      downloadButton("download_levels", "Download levels"),
      downloadButton("download_rdata", "Download RData"),
      fileInput("raw_data", "CSV to process"),
      selectInput("sanitize_colnames", "Simplify column names?", choices = c("Yes", "No")),
      selectInput("levels_to_int", "Recode factors as integers?", choices = c("Yes", "No")),
      selectInput("sort_rows", "Sort rows?", choices = c("Yes", "No")),
      selectInput("blanks_to_nas", "Convert blanks to NAs?", choices = c("Yes", "No")),
      selectInput("convert_class", "Convert object classes?", choices = c("Yes", "No")),
      selectInput("clean_ages", "Clean ages?", choices = c("No", "Yes")),
      selectInput("clean_dates", "Clean dates?", choices = c("No", "Yes"))
    ),
    mainPanel(
      verbatimTextOutput("text_output")
    ))
))

