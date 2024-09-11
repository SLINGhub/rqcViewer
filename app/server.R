options(shiny.maxRequestSize = 50 * 1024^2)  # Increase file upload size limit
#remotes::install_github("SLINGhub/midar@development")
library(shiny)
library(rhandsontable)
library(writexl)
library(ggplot2)
library(midar)
library(tidyverse)
library(dplyr)
library(stringr)
library(shinyjs)

server <- function(input, output, session) {

  rv <- reactiveValues(mexp = MidarExperiment(),
                       tbl_samples = tibble(),
                       show_filtered = FALSE)

  observeEvent(input$datafile_path, {
    # Create a MidarExperiment object (S4)
    mxp <- MidarExperiment()

    if (input$data_type == "mh_quant") {
      mxp <- midar::rawdata_import_agilent(mxp, path = input$datafile_path$datapath, file_format = "csv")
    } else if (input$data_type == "mrmkit") {
      mxp <- midar::import_mrmkit(mxp, path = input$datafile_path$datapath)
    }

    mxp <- midar::metadata_from_data(mxp)

    #todo: add acquisition_time_stamp and inj_volume
    tbl <- mxp@dataset_orig |>
      select(raw_data_filename, sample_name) |>
      distinct(raw_data_filename, sample_name, .keep_all = FALSE) |>
      mutate(is_selected = FALSE,
             rqc_series_id = NA_character_,
             relative_sample_amount = NA_real_)
    rv$mexp <- mxp
    rv$tbl_samples <- tbl
  })

  # Initialize and render the editable rhandsontable
  output$table <- renderRHandsontable({
   rhandsontable(rv$tbl_samples, width = 1000, height = 600) %>%
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE)
     })

  # Capture the table edited by the user
  observeEvent(input$table, {
    rv$tbl_samples <- hot_to_r(input$table)
 })

  # Selection logic
  observeEvent(input$apply_selection, {
    # TODO: sep can be replaced by | to generate the regex directly
    filter_terms <- str_split(input$filter_text, ",", simplify = TRUE)[1,]
    filter_terms <- str_trim(filter_terms)  # Trim white space

    if (all(filter_terms != "")) {
      # Update the is_selected column based on filter
      rv$tbl_samples  <- rv$tbl_samples |>
        mutate(is_selected = str_detect(raw_data_filename, paste(filter_terms, collapse = '|')))
    }
  })

  # Clear the selection
  observeEvent(input$clear_filter, {
    updateTextInput(session, "filter_text", value = "")
    rv$tbl_samples <- rv$tbl_samples |>
      mutate(is_selected = FALSE)
  })

  # Function to generate the plot and save it to a temporary file
  generate_plot_pdf <- function() {
    temp_file <- tempfile(fileext = ".pdf")

    mexp <- rv$mexp
    annot <- rv$tbl_samples |>  filter(is_selected)
    annot <- annot |>
      rename(analysis_id = raw_data_filename) |>
      mutate(relative_sample_amount = relative_sample_amount / 100)

    metadata_responsecurves(mexp) <- as_tibble(annot)

    plot_responsecurves(data = mexp,
                        use_filt_data = FALSE,
                        columns_page = input$n_cols,
                        rows_page = input$n_rows,
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





