#' splitModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_splitModule_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(
      outputId = ns('Column')
    ),

    textInput(
      inputId = ns('keyword'),
      label = '',
      placeholder = '/'
    ),

    h4('Example'),
    verbatimTextOutput(
      ns('description')
    )
  )

}

#' splitModule Server Functions
#'
#' @noRd
mod_splitModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Column <- renderUI({
      selectInput(
        inputId = ns('cols'),
        label = 'on Column',
        choices = colnames(inputData()),
        multiple = FALSE
      )
    })

    output$description <- renderText({
      paste0('[COL] 12/34 -> [COLA] 12, [COLB] 34')
    })

    data_splited <- reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::split(
        inputData = data,
        column = input$cols,
        splitby = input$keyword
      )

      data
    })

    return(data_splited)

    observeEvent(input$splitButton, {
      inputData(
        scissor::split(
          inputData = inputData(),
          column = input$splitColumn,
          splitby = input$splitKeyword
        )
      )

      updateSelectizeInput(
        session,
        inputId = "splitColumn",
        label = "splitSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_splitModule_ui("splitModule_1")

## To be copied in the server
# mod_splitModule_server("splitModule_1")
