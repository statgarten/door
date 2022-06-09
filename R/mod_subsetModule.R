#' subsetModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subsetModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("subsetColumn"),
      label = "subsetSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    actionButton(
      inputId = ns("subsetButton"),
      label = "subset",
      icon = icon("angle-down")
    )
  )
}

#' subsetModule Server Functions
#'
#' @noRd
mod_subsetModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(opened(), {
      if(opened()!="Subset"){return()}
      updateSelectizeInput(
        session,
        inputId = "subsetColumn",
        label = "subsetSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$subsetButton, {
      eval(parse(
        text =
          paste0(
            "inputData( inputData() %>% ",
            "select(-", input$subsetColumn, "))"
          )
      ))

      output$DT <-
        inputData() |>
        getDT(all = TRUE) |>
        reactable::renderReactable()

      updateSelectizeInput(
        session,
        inputId = "subsetColumn",
        label = "subsetSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )

    })
  })
}

## To be copied in the UI
# mod_subsetModule_ui("subsetModule_1")

## To be copied in the server
# mod_subsetModule_server("subsetModule_1")
