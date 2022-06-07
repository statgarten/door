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
    actionButton(
      inputId = ns("loadreshapeColumn"),
      label = "load variables",
      icon = icon("check")
    ),
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
mod_reshapeModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$loadreshapeColumn, {
      output$reshapeSortable <- renderUI({
        rank_list(
          text = "text",
          labels = colnames(inputData()),
          input_id = ns("reshapeSortable")
        )
      })
    })

    observeEvent(input$applyReshape, {
      # why this work?
      v <- inputData() |> select(input$reshapeSortable)
      inputData(v)

      # this will not work
      # inputData () |> inputData() |> select(input$reshapeSortable)

      output$DT <-
        inputData() |>
        getDT(all = TRUE) |>
        reactable::renderReactable()


    })

    # inputData <- eventReactive(input$reshapeSortable, {
    #   inputData() |> select(input$reshapeSortable)
    # })

    observeEvent(input$reshapeSortable, {

      # print(inputData() |> colnames())

      # print(inputData() |> select(input$reshapeSortable) |> colnames())

      # print(inputData(inputData() |> select(input$reshapeSortable)) |> colnames())
      # eval(parse(
      #   text =
      #     paste0(
      #       "inputData( inputData() |> select( ", input$reshapeSortable, ") )"
      #     )
      # ))

      # print(inputData() |> colnames())

      # inputData(
      #   inputData() |>
      #   dplyr::select(input$sortable[1:3])
      # )

      # inputData() |>
      #   dim() |>
      #   print()
      #
      # inputData()
      # colnames() |>
      # print()


      # eval(parse(
      #   text =
      #     paste0("inputData( inputData() |> dplyr::select( ", input$sortable, ") )")
      # ))
      # print('after')
      # print(colnames(inputData()))
      # inputData(inputData[,input$sortable])
    })
  })
}

## To be copied in the UI
# mod_reshapeModule_ui("reshapeModule_1")

## To be copied in the server
# mod_reshapeModule_server("reshapeModule_1")
