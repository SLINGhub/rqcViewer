
# Define server logic required to draw a histogram
library(shiny)
library(rhandsontable)
library(readxl)
library(writexl)
library(ggplot2)
library(gridExtra)
library(midar)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile_path)

   # Create a MidarExperiment object (S4)
    mexp <- MidarExperiment()

    if (input$data_type == "mh_quant") {

      # Load data
      mexp <- midar::import_masshunter(mexp, path = input$datafile_path$datapath)

    } else if (input$data_type == "mrmkit"){

      # Load data
      mexp <- midar::import_mrmkit(mexp, path = input$datafile_path$datapath)

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


