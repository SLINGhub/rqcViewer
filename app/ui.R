library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Data Upload and Filtering with Download Option"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Choose Data Type:",
                   choices = c("MH Quant" = "mh_quant", "MRMkit" = "mrmkit")),
      fileInput("datafile_path", "Upload Data File", accept = c(".csv")),
      numericInput("n_rows", "Number of rows per page:", 5, min = 1),
      numericInput("n_cols", "Number of columns per page:", 2, min = 1),
      downloadButton("download_pdf", "Download PDF"),
      downloadButton("download_excel", "Download Excel")
    ),

    mainPanel(

      # Create the tabset panel
      tabsetPanel(
        tabPanel("Table",
                 rHandsontableOutput("table")),
        tabPanel("Filtered table",
                 tableOutput("filtered_table"))
        )
    )
  )
)

