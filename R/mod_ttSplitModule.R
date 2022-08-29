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
        ),
        actionButton(inputId = ns('split'),label = 'Split')
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

    observeEvent(inputData(), {
      data <- inputData()
      factors <- names(Filter(is.factor, data))
      numerics <- names(Filter(is.numeric, data))
      choices <- sort(unique(c(factors, numerics)))

      output$target <- renderUI({
        selectInput(
          inputId = ns("cols"),
          label = "targetVar 지정",
          choices = choices, # use only factor (classification) or numeric (regression)
          multiple = FALSE
        )
      })

    })

    observeEvent(input$split, {

      data <- inputData()
      result <- goophi::trainTestSplit(
        data = data,
        target = input$cols,
        prop = input$ratio
      )

      output$train <- DT::renderDataTable(datatable(result$train[1:50,]) )
      output$str <- renderPrint(utils::str(result$train))
    })

    splitresult <-
      reactive({

        data <- inputData()

        result <- goophi::trainTestSplit(
          data = data,
          target = input$cols,
          prop = input$ratio
        )

        formula <- paste0(input$cols, " ~ .")

        rec <- goophi::prepForCV(
          data = result$train,
          formula = formula,
          seed = '1234'
        )

        list(
          train = result$train,
          test = result$test,
          dataSplit = result$dataSplit,
          rec = rec,
          target = isolate(input$cols),
          formula = formula
        )
      })

    return(splitresult)

  })
}

## To be copied in the UI
# mod_ttSplitModule_ui("ttSplitModule_1")

## To be copied in the server
# mod_ttSplitModule_server("ttSplitModule_1")
