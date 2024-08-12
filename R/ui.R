library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Data Upload and Filtering with Download Option"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Choose Data Type:",
                   choices = c("CSV" = "csv", "Excel" = "excel")),
      fileInput("file1", "Upload Data File", accept = c(".csv", ".xlsx")),
      numericInput("n_rows", "Number of rows per page:", 5, min = 1),
      numericInput("n_cols", "Number of columns per page:", 2, min = 1),
      downloadButton("download_pdf", "Download PDF"),
      downloadButton("download_excel", "Download Excel")
    ),

    mainPanel(
      rHandsontableOutput("table"),
      tableOutput("filtered_table")
    )
  )
)

