library(shiny)
library(shinyjs)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Data Upload and Filtering with Download Option"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Choose Data Type:",
                   choices = c("MH Quant" = "mh_quant", "MRMkit" = "mrmkit")),
      fileInput("datafile_path", "Upload Data File", accept = c(".csv")),
      numericInput("n_rows", "Number of rows per page:", 5, min = 1),
      numericInput("n_cols", "Number of columns per page:", 4, min = 1),
      downloadButton("download_pdf", "Download PDF"),
      downloadButton("download_excel", "Download Excel")
    ),

    mainPanel(
     tags$div(
        textInput("filter_text", "Enter keywords (comma separated):", value = ""),
        style = "margin-bottom: 20px;"
      ),

      tags$div(
        actionButton("apply_filter", "Apply Filter"),
        actionButton("clear_filter", "Unselect all"),
        style = "margin-bottom: 20px;"
      ),

      # Create the tabset panel
      tabsetPanel(
        tabPanel("Table", rHandsontableOutput("table")),
        tabPanel("Filtered table", tableOutput("filtered_table"))
      )
    )
  ),

  # Popup window with spinning wheel
  div(id = "popup",
      style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; text-align: center; padding-top: 20%;",
      div(style = "background: white; padding: 20px; border-radius: 10px;",
          p("Processing, please wait...")
      )
  ),

  useShinyjs()  # Initialize shinyjs for showing/hiding the spinner
)
