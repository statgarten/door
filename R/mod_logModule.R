#' logModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    radioButtons(
      inputId = ns("type"),
      label = "Type of Log",
      choices = c("2", "e", "10"),
      selected = "e",
      inline = TRUE
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}

#' logModule Server Functions
#'
#' @noRd
mod_logModule_server <- function(id, inputData) {
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

    observeEvent(input$type, {
      if (input$type == "2") {
        output$description <- renderText({
          "log2(4) -> 2"
        })
      }

      if (input$type == "e") {
        output$description <- renderText({
          "log(e^2) -> 2"
        })
      }

      if (input$type == "10") {
        output$description <- renderText({
          "log10(100) -> 2"
        })
      }
    })

    data_logged <- reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::trans(
        inputData = data,
        column = input$cols,
        operator = ifelse(input$type == "2", "Log2", ifelse(input$type == "e", "Log", "Log10"))
      )

      data
    })

    return(data_logged)
  })
}

## To be copied in the UI
# mod_logModule_ui("logModule_1")

## To be copied in the server
# mod_logModule_server("logModule_1")
