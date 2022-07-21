#' binarizeModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_binarizeModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns('Column')
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("operator"),
          label = "",
          choice = c(">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"),
          selected = ">",
          multiple = FALSE
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = ns('value'),
          label = '',
          placeholder = '2'
        )
      )
    ),
    h5('Example'),
    verbatimTextOutput(
      ns('description')
    )
  )
}

#' binarizeModule Server Functions
#'
#' @noRd
mod_binarizeModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
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
      paste(
        "x > 2",
        "[0, 1, 2, 3, 4, 5, 6] -> [0, 0, 0, 1, 1, 1, 1]",
        "# In requires value with c() : ",
        "ex: c('X', 'Y', 'Z') ",
        sep = '\n'
      )
    })

    data_binarize <- reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::binarize(
        inputData = data,
        column = input$cols,
        operator = input$operator,
        value = input$value
      )

      data
    })

    return(data_binarize)

  })
}

## To be copied in the UI
# mod_binarizeModule_ui("binarizeModule_1")

## To be copied in the server
# mod_binarizeModule_server("binarizeModule_1")
