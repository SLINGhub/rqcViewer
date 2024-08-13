options(shiny.maxRequestSize=50*1024^2)

# Define server logic required to draw a histogram
library(shiny)
library(rhandsontable)
library(readxl)
library(writexl)
library(ggplot2)
library(gridExtra)
library(midar)
library(tidyverse)
library(dplyr)
library(stringr)

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

    mexp@dataset_orig
  })

  output$table <- renderRHandsontable({

    data_sub <- data() |>
      distinct(raw_data_filename) |>
      mutate(is_rqc = str_detect(raw_data_filename, "RQC"),
             rqc_series_id = NA_character_,
             relative_sample_amount = NA_real_) |>
      select(raw_data_filename, is_rqc, rqc_series_id, relative_sample_amount)

    rhandsontable(data_sub)
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


