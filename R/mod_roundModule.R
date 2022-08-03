#' roundModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_roundModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    sliderInput(
      inputId = ns("num"),
      label = "Round Digits",
      min = -5,
      max = 5,
      value = 0,
      step = 1
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}

#' roundModule Server Functions
#'
#' @noRd
mod_roundModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Column <- renderUI({
      selectInput(
        inputId = ns("cols"),
        label = "on Column",
        choices = colnames(inputData()),
        multiple = FALSE
      )
    })

    output$description <- renderText({
      paste0("12345.56789 -> ", round(12345.56789, digits = input$num))
    })

    data_rounded <- reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::trans(
        inputData = data,
        column = input$cols,
        operator = "Round",
        value = input$num
      )

      data
    })

    return(data_rounded)
  })
}

## To be copied in the UI
# mod_roundModule_ui("roundModule_1")

## To be copied in the server
# mod_roundModule_server("roundModule_1")
