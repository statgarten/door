#' filterModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @importFrom shinyjs disabled enable
#' @importFrom DT renderDT
mod_filterModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("filterColumn"),
      label = "filterSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    selectInput(
      inputId = ns("filterOperator"),
      label = NULL,
      choices = c(">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"),
      selected = NULL,
      multiple = FALSE,
      width = '100%'
    ),
    textInput(
      inputId = ns("filterVariable"),
      label = NULL,
      placeholder = 'criteria',
      width = '100%'
    ),
    actionButton(
      inputId = ns("filterButton"),
      label = "filter",
      icon = icon("angle-down"),
      width = '90%',
      style = 'margin: auto;'
    )
  )
}

#' filterModule Server Functions
#'
#' @noRd
mod_filterModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if(opened()!="Filter"){return()}
      updateSelectizeInput(
        session,
        inputId = "filterColumn", # ns() not work
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$filterButton, {
      if (input$filterOperator == "In") {
        eval(parse(
          text =
            paste0(
              "inputData( inputData() |> filter( ", input$filterColumn, " %in% ",
              "c(", input$filterVariable, ") ) )"
            )
        ))
      }
      if (input$filterOperator == "Not In") {
        eval(parse(
          text =
            paste0(
              "inputData( inputData() |> filter( !", input$filterColumn, " %in% ",
              "c(", input$filterVariable, ") ) )"
            )
        ))
      }
      if (input$filterOperator == "Contains") {
        eval(parse(
          text =
            paste0(
              "inputData (", "inputData() |> filter(grepl(", input$filterVariable, ", ", input$filterColumn,
              ") ) )"
            )
        ))
      }
      if (input$filterOperator == "Not Contains") {
        eval(parse(
          text =
            paste0(
              "inputData (", "inputData() |> filter( ! grepl(", input$filterVariable, ", ", input$filterColumn,
              ") ) )"
            )
        ))
      }

      if (input$filterOperator %in% c(">", ">=", "<", "<=", "==", "!=")) {
        eval(parse(
          text =
            paste0(
              "inputData(inputData() |> ",
              "filter(", input$filterColumn, input$filterOperator, input$filterVariable, "))"
            )
        ))
      }

      output$DT <-
        inputData() |>
        getDT(all = TRUE) |>
        reactable::renderReactable()

      updateSelectizeInput(
        session,
        inputId = "filterColumn",
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_filterModule_ui("filterModule_1")

## To be copied in the server
# mod_filterModule_server("filterModule_1")
