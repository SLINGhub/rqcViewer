options(shiny.maxRequestSize = 50 * 1024^2)  # Increase file upload size limit
#remotes::install_github("SLINGhub/midar@development")
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


  # Initialize and render the editable rhandsontable
  output$table <- renderRHandsontable({
    #todo: add acquisition_time_stamp and inj_volume
    data_select <- data()@dataset_orig |>
      select(raw_data_filename, sample_name) |>
      distinct(raw_data_filename, sample_name, .keep_all = FALSE) |>
      mutate(is_selected = FALSE,
             rqc_series_id = NA_character_,
             relative_sample_amount = NA_real_)

    #todo: define acquisition_time_stamp type
    rhandsontable(data_select, width = 1000, height = 600) |>
      # hot_col("acquisition_time_stamp", dateFormat = "%Y-%m-%d %H:%M:%S", type = "date") |>
      hot_cols(columnSorting = TRUE)
  })

  # Capture the table edited by the user
  user_annotated_tbl <- reactive({
    hot_to_r(input$table)
  })

  # Render the filtered table based on user edits and selection
  output$filtered_table <- renderTable({
    user_annotated_tbl() |> dplyr::filter(is_selected)
  })

  # Handle the filtering logic
  observeEvent(input$apply_filter, {
    filter_terms <- str_split(input$filter_text, ",")[[1]]
    filter_terms <- str_trim(filter_terms)  # Trim whitespace

    if (length(filter_terms) > 0 && !all(is.na(filter_terms))) {
      # Update the is_selected column based on filter
      user_annotated_tbl() <- user_annotated_tbl() %>%
        mutate(is_selected = sapply(raw_data_filename, function(value) any(str_detect(value, filter_terms))))

      # # Update the filtered table to show the user's edited values
      # output$filtered_table <- renderTable({
      #   filtered_data %>%
      #     filter(is_selected) %>%
      #     select(raw_data_filename, sample_name, is_selected, rqc_series_id, relative_sample_amount)
      # })

      # # Update the rhandsontable with the filtered data
      # output$table <- renderRHandsontable({
      #   rhandsontable(filtered_data, width = 1000, height = 600) |>
      #     hot_cols(columnSorting = TRUE)
      # })
    }
  })

  # Clear the filter and reset the table
  observeEvent(input$clear_filter, {
    updateTextInput(session, "filter_text", value = "")

    # # Reset the table to the original data
    # output$table <- renderRHandsontable({
    #   data()@dataset_orig |>
    #     select(raw_data_filename, sample_name) |>
    #     distinct(raw_data_filename, sample_name, .keep_all = FALSE) |>
    #     mutate(is_selected = FALSE,
    #            rqc_series_id = NA_character_,
    #            relative_sample_amount = NA_real_) |>
    #     rhandsontable(width = 1000, height = 600) |>
    #     hot_cols(columnSorting = TRUE)
    # })

    # # Capture the table edited by the user
    # user_annotated_tbl <- reactive({
    #   hot_to_r(input$table)
    })

    # # Reset the filtered table as well
    # output$filtered_table <- renderTable({
    #   user_annotated_tbl()
    # })
  # })
}





