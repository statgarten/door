#' ttSplitModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ttSplitModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        uiOutput(
          outputId = ns("target")
        ),
        sliderInput(
          inputId = ns("ratio"),
          label = "trainSetRatio 지정",
          min = 0,
          max = 1,
          value = 0.7
        )
      ),
      column(
        width = 8,
        verbatimTextOutput(
          outputId = ns("str")
        )
      )
    ),
    DT::dataTableOutput(
      outputId = ns("train")
    ),

    # h4('Example'),
    # verbatimTextOutput(
    #   ns('description')
    # )
  )
}

#' ttSplitModule Server Functions
#'
#' @noRd
mod_ttSplitModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$target <- renderUI({
      selectInput(
        inputId = ns("cols"),
        label = "targetVar 지정",
        choices = colnames(inputData()),
        multiple = FALSE
      )
    })

    # output$description <- renderText({
    #   paste0('DESCRIPTION WILL BE HERE')
    # })

    output$train <- DT::renderDataTable({
      datatable(splitresult()$train)
    })

    output$str <- renderPrint(utils::str(splitresult()$train))

    splitresult <- reactive({
      req(inputData())
      data <- inputData()
      result <- goophi::trainTestSplit(
        data = data,
        target = input$cols,
        prop = input$ratio
      )
      list(
        train = result$train,
        test = result$test,
        dataSplit = result$dataSplit,
        target = isolate(input$cols),
        formula = isolate(paste0(input$cols, " ~ ."))
      )
    })

    return(splitresult)
  })
}

## To be copied in the UI
# mod_ttSplitModule_ui("ttSplitModule_1")

## To be copied in the server
# mod_ttSplitModule_server("ttSplitModule_1")
