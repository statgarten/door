#' mutateModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon
mod_mutateModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # which column
    selectInput(
      inputId = ns("mutateColumn"),
      label = "mutateSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    # option
    selectInput(
      inputId = ns("mutateOperator"),
      label = "mutateOpeartorLabel",
      choices = c("Round", "Log", "Log10", "Sqrt", "-", "Min-Max", "Normal", "Binarize"),
      selected = NULL,
      multiple = FALSE
    ),
    #
    textInput(
      inputId = ns("mutateVariable"),
      label = "mutateVariableLabel",
    ),
    selectInput(
      inputId = ns("binaryOperator"),
      label = "mutateOperatorLabel2",
      choice = c(">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"),
      selected = NULL,
      multiple = FALSE
    ),
    # apply button
    actionButton(
      inputId = ns("mutateButton"),
      label = "mutate",
      icon = icon("angle-down")
    )
  )
}

#' mutateModule Server Functions
#'
#' @noRd
mod_mutateModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if(opened()!="Mutate"){return()}
      updateSelectizeInput(
        session,
        inputId = "mutateColumn",
        label = "mutateSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$mutateButton, {
      inputData(
        scissor::trans(
          inputData = inputData(),
          column = input$mutateColumn,
          operator = input$mutateOperator
        )
      )
    })
  })
}

## To be copied in the UI
# mod_mutateModule_ui("mutateModule_1")

## To be copied in the server
# mod_mutateModule_server("mutateModule_1")
