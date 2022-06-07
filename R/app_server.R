#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom haven read_sas read_sav read_dta
#' @importFrom readxl read_xls read_xlsx
#' @importFrom readr read_rds
#' @importFrom reactable renderReactable
#' @noRd
app_server <- function(input, output, session) {
  src <- 'www/statgarten.png'
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })

  inputData <- reactiveVal(NULL)

  observeEvent(input$fileInputID, {
    file <- input$fileInputID

    ext <- file$datapath |>
      tools::file_ext() |>
      tolower()

    req(file)

    shinyjs::hide(id = "desc")
    shinyjs::hide(id = "fileInputID")

    shinyjs::show(id = "showAll")
    shinyjs::enable(id = "showAll")

    shinyjs::show(id = "LoadButton")
    shinyjs::show(id = "LoadTest")
    shinyjs::show(id = "SideBox")

    if (ext == "csv") {
      file$datapath |>
        read.csv() |>
        inputData()
    }
    if (ext == "tsv") {
      file$datapath |>
        read.csv(sep = "\t") |>
      inputData()
    }
    if (ext == "sas7bdat" || ext == "sas7bcat") {
      file$datapath |>
        haven::read_sas() |>
        inputData()
    }
    if (ext == "sav") {
      file$datapath |>
        haven::read_sav() |>
        inputData()
    }
    if (ext == "dta") {
      file$datapath |>
        haven::read_dta() |>
        inputData()
    }
    if (ext == "xls") {
      file$datapath |>
        readxl::read_xls() |>
        inputData()
    }
    if (ext == "xlsx") {
      file$datapath |>
        readxl::read_xlsx() |>
        inputData()
    }
    if (ext == "rds") {
      file$datapath |>
        readr::read_rds() |>
        inputData()
    }
    if (ext == "rda" || ext == "rdata") {
      file$datapath |>
        load() |>
        inputData()
    }


    output$DT <-
      inputData() |>
      getDT(all = TRUE) |>
      reactable::renderReactable()

  })

  mod_filterModule_server("filterModule_1", inputData)

  mod_subsetModule_server("subsetModule_1", inputData)

  mod_mutateModule_server("mutateModule_1", inputData)

  mod_cleanModule_server("cleanModule_1", inputData)

  mod_splitModule_server("splitModule_1", inputData)

  mod_exportModule_server("exportModule_1", inputData)
  # Your application server logic

  mod_reshapeModule_server("reshapeModule_1", inputData)

  observeEvent(input$showAll,{
    # only shows input data exists
    if(is.null(inputData())){return()}

    if(input$showAll == TRUE){
      output$DT <-
        inputData() |>
        getDT(all = TRUE) |>
        reactable::renderReactable()
    }
    if(input$showAll == FALSE){
      output$DT <-
        inputData() |>
        getDT() |>
        reactable::renderReactable()
    }

  })

}
