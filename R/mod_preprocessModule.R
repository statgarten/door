#' preprocessModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preprocessModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        numericInput(inputId = ns("seed"), label = "", value = "1234"),
        fluidRow(
          column(
            width = 6,
            prettySwitch(
              inputId = ns("impute"),
              label = "impute",
              status = "success",
              fill = TRUE
            )
          ),
          column(
            width = 6,
            uiOutput(
              outputId = ns("imputeBlock")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            prettySwitch(
              inputId = ns("norm"),
              label = "normalize",
              status = "success",
              fill = TRUE
            )
          ),
          column(
            width = 6,
            uiOutput(
              outputId = ns("normBlock")
            )
          )
        )
      ),
      column(
        width = 6,
        verbatimTextOutput(
          outputId = ns("recipe")
        )
      )
    )
  )
}

#' preprocessModule Server Functions
#'
#' @noRd
mod_preprocessModule_server <- function(id, splitresult) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$impute, {
      if (input$impute) {
        output$imputeBlock <- renderUI(
          tagList(
            selectInput(
              inputId = ns("numericImputeType"),
              label = NULL, # "imputationType 지정",
              choices = c("mean", "bag", "knn", "linear", "lower", "median", "roll"),
              selected = "mean"
            ),
            selectInput(
              inputId = ns("nominalImputeType"),
              label = NULL, # "imputationType 지정",
              choices = c("mode", "bag", "knn"),
              selected = "mode"
            )
          )
        )
      } else {
        output$imputeBlock <- renderUI(tagList())
      }
    })

    observeEvent(input$norm, {
      if (input$norm) {
        output$normBlock <- renderUI(
          tagList(
            selectInput(
              inputId = ns("normType"),
              label = NULL, # "nomalizationType 지정",
              choices = c("center", "normalization", "range", "scale"),
              selected = "center"
            )
          )
        )
      } else {
        output$normBlock <- renderUI(tagList())
      }
    })


    formula <- reactive({
      req(splitresult())
      paste0(splitresult()$target, " ~ .")
    })

    processresult <- reactive({
      req(splitresult())
      data <- splitresult()$train

      result <- stove::prepForCV(
        data = data,
        formula = formula(),
        imputation = input$impute,
        nominalImputationType = input$nominalImputeType,
        numericImputationType = input$numericImputeType,
        normalization = input$norm,
        normalizationType = ifelse(is.null(input$normType), "", input$normType),
        seed = input$seed
      )
      result
    })

    output$recipe <- renderPrint({
      processresult()
      # formula()
    })

    return(processresult)
  })
}

## To be copied in the UI
# mod_preprocessModule_ui("preprocessModule_1")

## To be copied in the server
# mod_preprocessModule_server("preprocessModule_1")
