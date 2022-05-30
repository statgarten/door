#' filterModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filterModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("loadFilterColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    shinyjs::disabled(
      selectInput(
        inputId = ns("filterColumn"),
        label = "filterSelectLabel",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectInput(
        inputId = ns("filterOperator"),
        label = "filterOpeartorLabel",
        choices = c(">", ">=", "<", "<=", "==", "!="),
        selected = NULL,
        multiple = FALSE
      ),
      textInput(
        inputId = ns("filterVariable"),
        label = "filterVariableLabel"
      ),
      actionButton(
        inputId = ns("filterButton"),
        label = "filter",
        icon = icon("angle-down")
      )
    )
  )
}

#' filterModule Server Functions
#'
#' @noRd
mod_filterModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$loadFilterColumn, {
      shinyjs::enable(id = "filterColumn")
      shinyjs::enable(id = "filterOperator")
      shinyjs::enable(id = "filterVariable")
      shinyjs::enable(id = "filterButton")

      updateSelectizeInput(
        session,
        inputId = "filterColumn",
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$filterButton, {
      eval(parse(
        text =
          paste0(
            "inputData(inputData() %>% ",
            "filter(", input$filterColumn, input$filterOperator, input$filterVariable, "))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
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
