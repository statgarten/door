#' modelingModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modelingModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("mode"),
          label = "mode 지정",
          choices = c("classification", "regression", "clustering"),
          selected = NULL
        ),
        selectInput(
          inputId = ns("algo"),
          label = "algo 지정",
          choices = c("logistic Regression" = "LogisticR")
        ),
        selectInput(
          inputId = ns("engine"),
          label = "engine 지정",
          choices = c("glmnet")
        ),
        selectInput(
          inputId = ns("fold"),
          label = "v 지정",
          choices = c(1:5),
          selected = 5
        ),
        selectInput(
          inputId = ns("metric"),
          label = "metric 지정",
          choices = c("roc_auc"),
          selected = "roc_auc"
        )
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 4,
            numericInput(
              inputId = ns("penaltyRangeMin"),
              label = "penaltyRangeMin",
              value = 0.1,
              min = 0.1,
              max = 1,
              step = 0.1
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("penaltyRangeMax"),
              label = "penaltyRangeMax",
              value = 20,
              min = 1,
              max = 20,
              step = 1
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("penaltyRangeLevels"),
              label = "penaltyRangeLevels",
              value = 5,
              min = 1,
              max = 5,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(
              inputId = ns("mixtureRangeMin"),
              label = "mixtureRangeMin",
              value = 0,
              min = 0,
              max = 1,
              step = 0.1
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("mixtureRangeMax"),
              label = "mixtureRangeMax",
              value = 1,
              min = 1,
              max = 2,
              step = 0.1
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("mixtureRangeLevels"),
              label = "mixtureRangeLevels",
              value = 5,
              min = 1,
              max = 5,
              step = 1
            )
          )
        ),
        actionButton(
          inputId = ns("applyModel"),
          label = "모델 생성 버튼"
        ),
        shinyjs::hidden(
          div(
            id = ns("models"),
            shinycssloaders::withSpinner(
              verbatimTextOutput(
                outputId = ns("obj")
              )
            )
          )
        )
      )
    )
  )
}

#' modelingModule Server Functions
#'
#' @noRd
mod_modelingModule_server <- function(id, splitresult, processresult, models_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # splitData <- trainTestSplit Result
    # splitresult$train
    # splitresult$test
    # splitresult$dataSplit
    # splitresult$target
    # splitresult$formula

    # processresult <- prepForCV Result

    modelObj <- reactive({
      Obj <- goophi::logisticRegression(
        algo = input$algo,
        engine = input$engine,
        mode = input$mode,
        trainingData = splitresult()$train,
        splitedData = splitresult()$dataSplit,
        formula = splitresult()$formula, ## Confirmed

        rec = processresult(),
        v = input$fold,
        penaltyRangeMin = input$penaltyRangeMin,
        penaltyRangeMax = input$penaltyRangeMax,
        penaltyRangeLevels = input$penaltyRangeLevels,
        mixtureRangeMin = input$mixtureRangeMin,
        mixtureRangeMax = input$mixtureRangeMax,
        mixtureRangeLevels = input$mixtureRangeLevels,
        metric = input$metric
      )

      Obj <- Obj$finalFittedModel

      Obj
    })

    observeEvent(input$applyModel, {
      shinyjs::show(id = "models")
      ## loader

      name <- isolate(paste0(input$algo, "_", input$engine))
      models_list(
        append(models_list, list("LogisticR_glmnet" = modelObj()))
      )

      output$obj <- renderPrint({
        setdiff(names(models_list()), "")
      })
    })



    return(models_list)
  })
}

## To be copied in the UI
# mod_modelingModule_ui("modelingModule_1")

## To be copied in the server
# mod_modelingModule_server("modelingModule_1")
