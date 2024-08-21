options(shiny.maxRequestSize=50*1024^2)
# remotes::install_github("SLINGhub/midar@development")

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
library(DataEditR)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile_path)

   # Create a MidarExperiment object (S4)
    mexp <- MidarExperiment()

    if (input$data_type == "mh_quant") {
      # Load data
      mexp <- midar::rawdata_import_agilent(mexp, path = input$datafile_path$datapath, file_format = "csv")
    } else if (input$data_type == "mrmkit"){
      # Load data
      mexp <- midar::import_mrmkit(mexp, path = input$datafile_path$datapath)
    }

    # Obtain basic/available metadata from data
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

  output$download_pdf <- downloadHandler(
    filename = function() { "response_curve.pdf" },
    content = function(file) {

      mexp <- data()

      annot <- user_annotated_tbl()

      annot <- annot |>
        dplyr::filter(is_rqc) |>
        rename(analysis_id = raw_data_filename)

      metadata_responsecurves(mexp) <- as_tibble(annot)

      plot_responsecurves(data = mexp,
                          use_filt_data = FALSE,
                          output_pdf = TRUE,
                          path = file)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() { "user_annotated_tbl.xlsx" },
    content = function(file) {
      write_xlsx(user_annotated_tbl(), file)
    }
  )
}


