#' modelingModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_modelingModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column( # Result area
        width = 9,
        # Cluster
        conditionalPanel(
          "input.mode == 'clustering'",
          ns = ns,
          plotOutput(outputId = ns("ClusterPlot")),
          plotOutput(outputId = ns("optimalK")),
          verbatimTextOutput(outputId = ns("ClusterResult")),
        ),

        # Regression
        conditionalPanel(
          "input.mode == 'regression'",
          ns = ns,
          fluidRow(
            column(
              width = 6,
              plotOutput(outputId = ns("RegressionPlot"))
            ),
            column(
              width = 6,
              plotOutput(outputId = ns("RMSEPlot"))
            )
          )
        ),

        # Classification
        conditionalPanel(
          "input.mode == 'classification'",
          ns = ns,
          plotOutput(outputId = ns("confusionMatrix")),
          plotOutput(outputId = ns("rocCurve"))
        ),

        # Regression & Classification both
        conditionalPanel(
          "input.mode == 'classification' || input.mode == 'regression'",
          ns = ns,
          verbatimTextOutput(outputId = ns("EvalMatrix"))
        )
      ),
      column( # Options
        width = 3,
        selectInput(
          inputId = ns("mode"),
          label = "mode 지정",
          choices = c("classification", "regression", "clustering"),
          selected = NULL,
          width = "100%"
        ),
        selectInput(
          inputId = ns("algo"),
          label = "algo 지정",
          choices = c(
            "Logistic Regression" = "logisticRegression",
            "Linear Regression" = "linearRegression",
            "K Nearest Neighbor" = "KNN",
            "Naive Bayes" = "naiveBayes",
            "MLP",
            "Decision Tree" = "decisionTree",
            "Random Forest" = "randomForest",
            "XGBoost",
            "lightGBM",
            "K Means Clustering" = "KMC"
          ),
          width = "100%"
        ),
        selectInput(
          inputId = ns("engine"),
          label = "engine 지정",
          choices = c(
            "glmnet", # logitistic Regression, Linera Regression
            "kknn", # KNN
            "klaR", # NB
            "nnet", # MLP
            "rpart", # decisionTree
            "ranger", # randomForest
            "xgboost", # XGBoost
            "lightgbm", # Light GBM,
            "-" # KMC
          ),
          width = "100%"
        ),
        actionButton(
          ns("hyper"),
          label = "show hyper",
          icon = icon("gear"),
          style = "font-weight:bold; background:#B8906B; color:black; width:100%; margin-bottom: 0.5em;"
        ),
        actionButton( # Main Action
          inputId = ns("applyModel"),
          label = "Build Model",
          style = "background: #004B4D;border-radius: 0;color: white;border: 0; font-weight: bold; width: 100%"
        ),
        br(),
        br(),
        shinyjs::hidden(
          div(
            id = ns("models"),
            shinycssloaders::withSpinner(
              verbatimTextOutput(
                outputId = ns("obj")
              )
            ),
            selectInput(
              inputId = ns("reportML"),
              label = "생성된 모델",
              choices = NULL,
              selected = NULL
            ),
            actionButton(
              inputId = ns("generateReport"),
              label = "report 생성",
              style = "background: #004B4D;border-radius: 0;color: white;border: 0; font-weight: bold; width: 100%"
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
mod_modelingModule_server <- function(id, splitresult, models_list, tuned_results_list) {
  # mod_modelingModule_server <- function(id, splitresult, processresult, models_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$hyper, {
      # Hyper Parameters
      showModal(
        modalDialog(
          easyClose = TRUE,
          footer = NULL,
          title = "HyperParameter Options",
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = ns("fold"),
                label = "fold",
                value = 2,
                min = 2,
                max = 10,
                step = 1
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = ns("gridNum"),
                label = "gridNum 지정",
                min = 1, max = 10, step = 1, value = 5,
                width = "100%"
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = ns("iter"),
                label = "iter 지정",
                min = 1, max = 10, step = 1, value = 2,
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns("seed"),
                label = "seed 지정",
                min = 1, max = 9999, step = 1, value = 1234,
                width = "100%"
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId = ns("metric"),
                label = "metric 지정",
                choices = c("roc_auc", "rmse"),
                selected = "roc_auc",
                width = "100%"
              )
            )
          ),
          ## KMC
          conditionalPanel(
            "input.algo == 'KMC'",
            ns = ns,
            hr(),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("maxK"),
                  label = "maxK",
                  value = 15,
                  min = 2,
                  max = 100,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("nStart"),
                  label = "nStart",
                  value = 25,
                  min = 1,
                  max = 175,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("iterMax"),
                  label = "iterMax",
                  value = 10,
                  min = 1,
                  max = 5000,
                  step = 50
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("nBoot"),
                  label = "nBoot",
                  value = 100,
                  min = 1,
                  max = 500,
                  step = 50
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = ns("algorithm"),
                  label = "algorithm",
                  choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                  selected = "Hartigan-Wong"
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = ns("selectOptimal"),
                  label = "selectOptimal",
                  choices = c("silhouette", "gap_stat"),
                  selected = "silhouette"
                )
              )
            )
          ),
          actionButton(inputId = ns("setHyper"), label = icon("check"))
        )
      )
    })

    observeEvent(input$applyModel, {
      shinyjs::show(id = "models")

      if (input$algo == "logisticRegression") {
        modelObj <- reactive({
          Obj <- stove::logisticRegression(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula, ## Confirmed
            rec = splitresult()$rec,
            v = ifelse(is.null(input$fold), 2, input$fold),
            gridNum = input$gridNum,
            iter = input$iter,
            metric = input$metric,
            seed = input$seed
          )

          # Obj <- Obj$finalFittedModel
          Obj <- Obj$finalized$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("logisticRegression_glmnet" = modelObj()))
        )
      }
      # build model
      if (input$algo == "linearRegression") {

        modelObj <- reactive({
          Obj <- stove::linearRegression(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = ifelse(is.null(input$fold),2, input$fold),
            gridNum = ifelse(is.null(input$gridNum), 4, input$gridNum),#
            iter = ifelse(is.null(input$iter), 2, input$iter), #
            metric = ifelse(is.null(input$metric), 'rmse' ,input$metric),
            seed = ifelse(is.null(input$seed), 1234, input$seed) #
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result
        models_list(
          append(models_list(), list("linearRegression_glmnet" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("linearRegression_glmnet" = tls))
        )
      }

      if (input$algo == "KNN") {


        modelObj <- reactive({
          Obj <- stove::KNN(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = ifelse(is.null(input$fold),2, input$fold),
            gridNum = ifelse(is.null(input$gridNum), 4, input$gridNum),#
            iter = ifelse(is.null(input$iter), 2, input$iter), #
            metric = ifelse(is.null(input$metric), 'rmse' ,input$metric),
            seed = ifelse(is.null(input$seed), 1234, input$seed) #
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("KNN_kknn" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("KNN_kknn" = tls))
        )
      }

      if (input$algo == "naiveBayes") {
        modelObj <- reactive({
          Obj <- stove::naiveBayes(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum,
            iter = input$iter,
            metric = input$metric,
            seed = input$seed
          )

          # Obj <- Obj$finalFittedModel
          Obj <- Obj$finalized$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("naiveBayes_klaR" = modelObj()))
        )
      }

      if (input$algo == "MLP") {
        modelObj <- reactive({
          Obj <- stove::MLP(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum,
            iter = input$iter,
            metric = input$metric,
            seed = input$seed
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("MLP_nnet" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("MLP_nnet" = tls))
        )
      }

      if (input$algo == "decisionTree") {
        modelObj <- reactive({
          Obj <- stove::decisionTree(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum, #
            iter = input$iter, #
            metric = input$metric,
            seed = input$seed
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("decisionTree_rpart" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("decisionTree_rpart" = tls))
        )
      }

      if (input$algo == "randomForest") {
        modelObj <- reactive({
          Obj <- stove::randomForest(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum,
            iter = input$iter,
            metric = input$metric,
            seed = input$seed
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("randomForest_ranger" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("randomForest_ranger" = tls))
        )
      }

      if (input$algo == "XGBoost") {
        modelObj <- reactive({
          Obj <- stove::xgBoost(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum, #
            iter = input$iter, #
            metric = input$metric,
            seed = input$seed #
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("XGBoost_xgboost" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("XGBoost_xgboost" = tls))
        )
      }

      if (input$algo == "lightGBM") {
        modelObj <- reactive({
          Obj <- stove::lightGbm(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = splitresult()$rec,
            v = input$fold,
            gridNum = input$gridNum,
            iter = input$iter,
            metric = input$metric,
            seed = input$seed
          )

          Obj
        })

        model <- modelObj()$finalized$finalFittedModel
        tls <- modelObj()$bayes_opt_result

        models_list(
          append(models_list(), list("lightGBM_lightgbm" = model))
        )

        tuned_results_list(
          append(tuned_results_list(), list("lightGBM_lightgbm" = tls))
        )
      }

      if (input$algo == "KMC") {
        data <- rbind(splitresult()$train, splitresult()$test)

        modelObj <- reactive({
          Obj <- stove::kMeansClustering(
            data = data,
            maxK = ifelse(is.null(input$maxK), 15, input$maxK),
            nStart = ifelse(is.null(input$nStart), 25, input$nStart),
            iterMax = ifelse(is.null(input$iterMax), 10, input$iterMax),
            nBoot = ifelse(is.null(input$nBoot), 100, input$nBoot),
            algorithm = ifelse(is.null(input$algorithm), "Hartigan-Wong", input$algorithm),
            selectOptimal = ifelse(is.null(input$selectOptimal), "silhouette", input$selectOptimal),
            seedNum = ifelse(is.null(input$seed), 6471, input$seed)
          )

          Obj
        })

        models_list(
          append(models_list(), list("KmeansClustering" = modelObj()))
        )
      }
      # name <- isolate(paste0(input$algo, "_", input$engine))

      output$obj <- renderPrint({
        setdiff(names(models_list()), "")
      })

      updateSelectInput(
        inputId = "reportML",
        label = "생성된 모델",
        choices = names(models_list()),
        selected = NULL
      )
    })

    observeEvent(input$generateReport, {
      req(input$reportML)
      if (input$mode == "clustering") {
        # if (input$reportML == "KmeansClustering") {
        data <- rbind(splitresult()$train, splitresult()$test)

        Obj <- models_list()$KmeansClustering

        vis_result <- Obj

        output$ClusterPlot <- renderPlot(vis_result$clustVis)
        output$optimalK <- renderPlot(vis_result$optimalK)
        output$ClusterResult <- renderPrint({
          Obj
        })
      }

      # Regression
      if (input$mode == "regression") {
        print("models list: ")
        print(models_list())
        print("tuned results list: ")
        print(tuned_results_list())

        vis_result <- stove::regressionPlot(
          modelName = input$reportML,
          modelsList = models_list(),
          targetVar = splitresult()$target
        )

        output$RegressionPlot <- renderPlot(vis_result)

        print("vis results")

        rmse_plot <- stove::plotRmseComparison(
          tunedResultsList = tuned_results_list(),
          v = ifelse(is.null(input$fold),2, input$fold),
          iter = ifelse(is.null(input$iter), 2, input$iter) #
        )

        output$RMSEPlot <- renderPlot(rmse_plot)

        print("rmse plot")
        output$EvalMatrix <- renderPrint({
          stove::evalMetricsR(
            modelsList = models_list(),
            targetVar = splitresult()$target
          )
        })
      }

      # Classification
      if (input$mode == "classification") {
        # if (input$reportML %in%
        # c("LogisticR_glmnet", "KNN_kknn", "decisionTree_rpart", "lightGBM_lightgbm", "MLP_nnet", "NB_klaR", "randomForest_ranger", "XGBoost_xgboost") ) {

        # Specific on Confusion Matrix
        output$confusionMatrix <- renderPlot({
          stove::confusionMatrix(
            modelName = input$reportML,
            modelsList = models_list(),
            targetVar = splitresult()$target
          )
        })

        # Multiple on ROC?
        rc <- stove::rocCurve(
          modelsList = models_list(),
          targetVar = splitresult()$target
        )
        output$rocCurve <- renderPlot(rc)

        # Multiple on Evaluation Matrix?
        output$EvalMatrix <- renderPrint({
          stove::evalMetricsC(
            modelsList = models_list(),
            targetVar = splitresult()$target
          )
        })
      }
    })

    observeEvent(input$mode, {
      req(input$mode)
      if (input$mode == "classification") {
        updateSelectInput(
          inputId = "algo",
          label = "algo 지정",
          choices = c(
            "Logistic Regression" = "logisticRegression",
            "K Nearest Neighbor" = "KNN",
            "Naive Bayes" = "naiveBayes",
            "MLP",
            "Decision Tree" = "decisionTree",
            "Random Forest" = "randomForest",
            "XGBoost",
            "lightGBM"
          ),
          selected = NULL
        )
        updateSelectInput(
          inputId = "metric",
          label = "metric 지정",
          choices = "roc_auc",
          selected = "roc_auc"
        )
        shinyjs::enable("metric")
      }
      if (input$mode == "regression") {
        updateSelectInput(
          inputId = "algo",
          label = "algo 지정",
          choices = c(
            "Linear Regression" = "linearRegression",
            "K Nearest Neighbor" = "KNN",
            "MLP",
            "Decision Tree" = "decisionTree",
            "Random Forest" = "randomForest",
            "XGBoost",
            "lightGBM"
          ),
          selected = NULL
        )

        updateSelectInput(
          inputId = "metric",
          label = "metric 지정",
          choices = "rmse",
          selected = "rmse"
        )
        shinyjs::enable("metric")
      }

      if (input$mode == "clustering") {
        updateSelectInput(
          inputId = "algo",
          label = "algo 지정",
          choices = ("K Means Clustering" <- "KMC"),
          selected = NULL
        )
        shinyjs::disable("metric")
        updateSelectInput(
          inputId = "metric",
          label = "metric 지정",
          choices = "",
          selected = ""
        )
      }
    })

    observeEvent(input$algo, {
      req(input$mode)
      if (input$algo == "logisticRegression") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "glmnet" # logitistic Regression, Linera Regression
        )
      }
      if (input$algo == "linearRegression") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "glmnet" # logitistic Regression, Linera Regression
        )
      }
      if (input$algo == "KNN") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "kknn" # KNN
        )
      }
      if (input$algo == "naiveBayes") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "klaR" # NB
        )
      }
      if (input$algo == "MLP") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "nnet", # MLP
        )
      }
      if (input$algo == "decisionTree") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "rpart" # decisionTree
        )
      }
      if (input$algo == "randomForest") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "ranger" # randomForest
        )
      }
      if (input$algo == "XGBoost") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "xgboost" # XGBoost
        )
      }
      if (input$algo == "lightGBM") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "lightgbm" # Light GBM,
        )
      }
      if (input$algo == "KMC") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "-" # KMC
        )
      }
    })

    return(list(models_list = models_list, tuned_results_list = tuned_results_list))
  })
}

## To be copied in the UI
# mod_modelingModule_ui("modelingModule_1")

## To be copied in the server
# mod_modelingModule_server("modelingModule_1")
