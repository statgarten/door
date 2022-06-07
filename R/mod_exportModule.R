#' exportModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_exportModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("exportFilename"),
      label = "filename"
    ),
    selectInput(
      inputId = ns("exportOption"),
      label = "file extension",
      selected = ".csv",
      multiple = FALSE,
      choices = c(".csv", ".rda", ".sqlite", ".xlsx")
    ),
    downloadButton(
      outputId = ns("exportButton"),
      label = "export"
    )
  )
}

#' exportModule Server Functions
#'
#' @noRd
#' @importFrom writexl write_xlsx
#' @importFrom RSQLite dbWriteTable dbConnect dbDriver dbDisconnect

mod_exportModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$exportButton <- downloadHandler(
      filename = function() {
        if (input$exportFilename == "") {
          return(paste0("downloaded", input$exportOption))
        }
        return(paste0(input$exportFilename, input$exportOption))
      },
      content = function(con) {
        if (input$exportOption == ".csv") {
          write.csv(inputData(), file = con, row.names = FALSE)
        }
        if (input$exportOption == ".rda") {
          saveRDS(inputData, file = con)
        }
        if (input$exportOption == ".sqlite") {
          RSQLite::dbWriteTable(
            conn = RSQLite::dbConnect(
              RSQLite::dbDriver("SQLite"),
              dbname = con
            ),
            name = "data1",
            value = inputData()
          )
          RSQLite::dbDisconnect()
          # con <- dbConnect(drv=RSQLite::SQLite(), dbname="SQLITE_FILENAME")
          # data <- dbGetQuery(conn=con, statement = "SELECT * FROM 'data1'")
        }
        if (input$exportOption == ".xlsx") {
          writexl::write_xlsx(inputData(), path = con)
        }
      }
    )
  })
}

## To be copied in the UI
# mod_exportModule_ui("exportModule_1")

## To be copied in the server
# mod_exportModule_server("exportModule_1")
