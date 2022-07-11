#' relationModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_relationModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("var1"),
      label = "variable 1",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    selectInput(
      inputId = ns("var2"),
      label = "variable 2",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    )
  )
}

#' relationModule Server Functions
#'
#' @noRd
mod_relationModule_server <- function(id, inputData, ggobj, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if (opened() != "Relation") {
        return()
      }
      updateSelectizeInput(
        session,
        inputId = "var1",
        label = "variable 1",
        choices = c("", colnames(inputData())),
        selected = NULL,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "var2",
        label = "variable 2",
        choices = c("", colnames(inputData())),
        selected = NULL,
        server = TRUE
      )
    })

    observeEvent(input$var2, {
      if (input$var2 == "") {
        return(0)
      }

      ggobj(
        board::relation(
          var1 = inputData()[, input$var1],
          var2 = inputData()[, input$var2]
        )
      )
    })
  })
}

## To be copied in the UI
# mod_relationModule_ui("relationModule_1")

## To be copied in the server
# mod_relationModule_server("relationModule_1")
