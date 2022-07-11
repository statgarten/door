#' reshapeModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import sortable
#' @import reactable
#' @importFrom dplyr select
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reshapeModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("reshapeSortable")),
    actionButton(
      inputId = ns("applyReshape"),
      label = "apply change",
      icon = icon("angle-down")
    )
  )
}

#' reshapeModule Server Functions
#'
#' @noRd
mod_reshapeModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(opened, {
      output$reshapeSortable <- renderUI({
        rank_list(
          text = "Order of Columns",
          labels = colnames(inputData()),
          input_id = ns("reshapeSortable"),
          class = c("custom-sortable", "default-sortable") # add custom style
        )
      })
    })

    observeEvent(input$applyReshape, {
      # why this work?
      v <- scissor::reorder(
        inputData = isolate(inputData()),
        columns = input$reshapeSortable
      )

      inputData(v)

      # this will not work
      # inputData () |> inputData() |> select(input$reshapeSortable)

      output$reshapeSortable <- renderUI({
        rank_list(
          text = "Order of Columns",
          labels = colnames(inputData()),
          input_id = ns("reshapeSortable"),
          class = c("default-sortable", "custom-sortable") # add custom style
        )
      })
    })
  })
}

## To be copied in the UI
# mod_reshapeModule_ui("reshapeModule_1")

## To be copied in the server
# mod_reshapeModule_server("reshapeModule_1")
