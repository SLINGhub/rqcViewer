options(shiny.maxRequestSize = 50 * 1024^2)  # Increase file upload size limit

library(shiny)
library(rhandsontable)
library(writexl)
library(ggplot2)
library(gridExtra)
library(midar)
library(tidyverse)
library(dplyr)
library(stringr)
library(DataEditR)
library(shinyjs)

server <- function(input, output, session) {

  data <- reactive({
    req(input$datafile_path)

    # Create a MidarExperiment object (S4)
    mexp <- MidarExperiment()

    if (input$data_type == "mh_quant") {
      mexp <- midar::rawdata_import_agilent(mexp, path = input$datafile_path$datapath, file_format = "csv")
    } else if (input$data_type == "mrmkit") {
      mexp <- midar::import_mrmkit(mexp, path = input$datafile_path$datapath)
    }

    mexp <- midar::metadata_from_data(mexp)
    mexp
  })

  output$table <- renderRHandsontable({

    #todo: add acquisition_time_stamp and inj_volume
    data_select <- data()@dataset_orig |>
      select(raw_data_filename, sample_name) |>
      distinct(raw_data_filename, sample_name, .keep_all = FALSE) |>
      mutate(is_rqc = str_detect(raw_data_filename, "RQC"),
             rqc_series_id = NA_character_,
             relative_sample_amount = NA_real_)

    #todo: define acquisition_time_stamp type
    rhandsontable(data_select, width = 1000, height = 600) |>
      # hot_col("acquisition_time_stamp", dateFormat = "%Y-%m-%d %H:%M:%S", type = "date") |>
      hot_cols(columnSorting = TRUE)
  })

  user_annotated_tbl <- reactive({
    hot_to_r(input$table)
  })

  output$filtered_table <- renderTable({
    user_annotated_tbl() |> dplyr::filter(is_rqc)
  })

  # Function to generate the plot and save it to a temporary file
  generate_plot_pdf <- function() {
    temp_file <- tempfile(fileext = ".pdf")

    mexp <- data()
    annot <- user_annotated_tbl()
    annot <- annot |>
      dplyr::filter(is_rqc) |>
      rename(analysis_id = raw_data_filename)

    metadata_responsecurves(mexp) <- as_tibble(annot)

    plot_responsecurves(data = mexp,
                        use_filt_data = FALSE,
                        output_pdf = TRUE,
                        path = temp_file)

    temp_file
  }

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), ".pdf", sep = "")
    },

    content = function(file) {
      # Show spinner
      shinyjs::show("popup")

      # Generate the plot and save to a temporary file
      plot_file <- generate_plot_pdf()

      # Copy the plot to the final location
      file.copy(plot_file, file, overwrite = TRUE)

      # Hide spinner
      shinyjs::hide("popup")
    }
  )

  #table output
  output$download_excel <- downloadHandler(
    filename = function() {
      "user_annotated_tbl.xlsx"
    },

    content = function(file) {
      # Show spinner
      shinyjs::show("popup")

      # Write the table to an Excel file
      write_xlsx(user_annotated_tbl(), file)

      # Hide spinner
      shinyjs::hide("popup")
    }
  )
}
