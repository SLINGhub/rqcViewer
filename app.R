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


# Define server logic required to draw a histogram
library(shiny)
library(rhandsontable)
library(readxl)
library(writexl)
library(ggplot2)
library(gridExtra)

server <- function(input, output) {
  data <- reactive({
    req(input$file1)
    if (input$data_type == "csv") {
      read.csv(input$file1$datapath)
    } else {
      read_excel(input$file1$datapath)
    }
  })

  output$table <- renderRHandsontable({
    rhandsontable(data())
  })

  filtered_data <- reactive({
    hot_to_r(input$table)
  })

  output$filtered_table <- renderTable({
    filtered_data()
  })

  output$download_pdf <- downloadHandler(
    filename = function() { "filtered_data.pdf" },
    content = function(file) {
      pdf(file)
      plots <- list()
      df <- filtered_data()

      for (i in 1:nrow(df)) {
        p <- ggplot(df[i, , drop = FALSE], aes_string(x = names(df)[1], y = names(df)[2])) +
          geom_point()
        plots[[i]] <- p
      }

      grid.arrange(grobs = plots, nrow = input$n_rows, ncol = input$n_cols)
      dev.off()
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() { "filtered_data.xlsx" },
    content = function(file) {
      write_xlsx(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
