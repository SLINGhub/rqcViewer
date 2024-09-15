library(shiny)
library(shinyjs)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Response Curve Inspector (RQCsee)", windowTitle = "RQCsee"),

  sidebarLayout(
    sidebarPanel(
      radioButtons( "data_type",
                    "Rqw Data Source:",
                   choices = c("MH Quant" = "mh_quant", "MRMkit" = "mrmkit"),
                   inline=TRUE),
      fileInput("datafile_path", "Upload Data File", accept = c(".csv", ".tsv")),
      numericInput("n_rows", "Rows per page:", 5, min = 1),
      numericInput("n_cols", "Columns per page:", 4, min = 1),
      downloadButton("download_pdf", "Download PDF"),
      downloadButton("download_excel", "Download Excel")
    ),

    mainPanel(
     tags$div(
        textInput("filter_text", "Enter keywords (comma separated):", value = ""),
        style = "margin-bottom: 20px;"
      ),

      tags$div(
        actionButton("apply_selection", "Apply Selection"),
        actionButton("clear_filter", "Unselect all"),
        style = "margin-bottom: 20px;"
      ),

      # Show the table
     rHandsontableOutput("table")
     # uiOutput("table_ui")  # Dynamically render the table
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
