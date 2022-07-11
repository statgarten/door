#' briefModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
mod_briefModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("BRIEF MODULE UI"),
    selectInput(
      inputId = ns("excludeColumn"),
      label = "select Not Numeric",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    actionButton(
      inputId = ns("briefButton"),
      label = "brief",
      icon = icon("angle-down")
    )
  )
}

#' briefModule Server Functions
#'
#' @noRd
mod_briefModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if (opened() != "Brief") {
        return()
      }
      updateSelectizeInput(
        session,
        inputId = "excludeColumn",
        label = "select Not Numeric",
        choices = colnames(inputData()),
        server = TRUE,
      )
    })
    observeEvent(input$briefButton, {
      exc <- NULL
      if (!is.null(input$excludeColumn)) {
        exc <- sapply(input$excludeColumn, function(i) {
          which(i == colnames(inputData()))
        })
      }

      print(board::brief(inputData(), exc = exc))
    })
  })
}

## To be copied in the UI
# mod_briefModule_ui("briefModule_1")

## To be copied in the server
# mod_briefModule_server("briefModule_1")
