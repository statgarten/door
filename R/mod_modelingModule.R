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
          plotOutput(outputId = ns("RegressionPlot"))
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
        sliderInput(
          inputId = ns("fold"),
          label = "v 지정",
          min = 1, max = 5, step = 1, value = 2,
          width = "100%"
        ),
        sliderInput(
          inputId = ns("gridNum"),
          label = "gridNum 지정",
          min = 1, max = 10, step = 1, value = 5,
          width = "100%"
        ),
        sliderInput(
          inputId = ns("iter"),
          label = "iter 지정",
          min = 5, max = 50, step = 5, value = 10,
          width = "100%"
        ),
        numericInput(
          inputId = ns("seed"),
          label = "seed 지정",
          min = 1, max = 9999, step = 1, value = 1234,
          width = "100%"
        ),
        selectInput(
          inputId = ns("metric"),
          label = "metric 지정",
          choices = c("roc_auc", "rmse"),
          selected = "roc_auc",
          width = "100%"
        ),
        #actionButton(
        #  ns("hyper"),
        #  label = "show hyper",
        #  icon = icon("gear"),
        #  style = "font-weight:bold; background:#b2bec3; color:black; width:100%;"
        #),
        actionButton( # Main Action
          inputId = ns("applyModel"),
          label = "모델 생성 버튼",
          style = "font-weight: bold;background: #3EC70B;color: white; width: 100%"
        ),
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
              style = "font-weight: bold;background: #00b894;color: white; width: 100%"
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
mod_modelingModule_server <- function(id, splitresult, models_list) {
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

          ## Penalty
          # Logistic / Linear / MLP

          conditionalPanel(
            "input.algo == 'logisticRegression' || input.algo == 'MLP'",
            # "input.algo == 'logisticRegression' || input.algo == 'LinearR' || input.algo == 'MLP'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("penaltyRangeMin"),
                  label = "penaltyRangeMin",
                  value = 0.1,
                  min = 0.0001,
                  max = 1,
                  step = 0.0001
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("penaltyRangeMax"),
                  label = "penaltyRangeMax",
                  value = 1,
                  min = 0.0001,
                  max = 1,
                  step = 0.0001
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("penaltyRangeLevels"),
                  label = "penaltyRangeLevels",
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## Mixtures
          # Logistic / Linear Regression
          conditionalPanel(
            "input.algo == 'logisticRegression'",
            # "input.algo == 'logisticRegression' || input.algo == 'LinearR'",
            ns = ns,
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
                  min = 0,
                  max = 1,
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
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## Neighbors
          # KNN
          conditionalPanel(
            "input.algo == 'KNN'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("neighborsRangeMin"),
                  label = "neighborsRangeMin",
                  value = 5,
                  min = 1,
                  max = 10,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("neighborsRangeMax"),
                  label = "neighborsRangeMax",
                  value = 10,
                  min = 1,
                  max = 10,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("neighborsRangeLevels"),
                  label = "neighborsRangeLevels",
                  value = 10,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## Smooth & Laplace
          # NB
          conditionalPanel(
            "input.algo == 'naiveBayes'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("smoothnessRangeMin"),
                  label = "smoothnessRangeMin",
                  value = 0.5,
                  min = 0.5,
                  max = 1.5,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("smoothnessRangeMax"),
                  label = "smoothnessRangeMax",
                  value = 1.5,
                  min = 1.5,
                  max = 1.5,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("smoothnessRangeLevels"),
                  label = "smoothnessRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("LaplaceRangeMin"),
                  label = "LaplaceRangeMin",
                  value = 0,
                  min = 0,
                  max = 3,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("LaplaceRangeMax"),
                  label = "LaplaceRangeMax",
                  value = 3,
                  min = 0,
                  max = 3,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("LaplaceRangeLevels"),
                  label = "LaplaceRangeLevels",
                  value = 4,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),



          ## hidden_units & epoches
          # MLP
          conditionalPanel(
            "input.algo == 'MLP'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("hiddenUnitsRangeMin"),
                  label = "hiddenUnitsRangeMin",
                  value = 1,
                  min = 1,
                  max = 10,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("hiddenUnitsRangeMax"),
                  label = "hiddenUnitsRangeMax",
                  value = 10,
                  min = 1,
                  max = 10,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("hiddenUnitsRangeLevels"),
                  label = "hiddenUnitsRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("epochsRangeMin"),
                  label = "epochsRangeMin",
                  value = 10,
                  min = 10,
                  max = 1000,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("epochsRangeMax"),
                  label = "epochsRangeMax",
                  value = 100,
                  min = 10,
                  max = 1000,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("epochsRangeLevels"),
                  label = "epochsRangeLevels",
                  value = 2,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),


          ## treeDepth
          # decisionTree lightgbm, xgboost
          conditionalPanel(
            "input.algo == 'decisionTree' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("treeDepthRangeMin"),
                  label = "treeDepthRangeMin",
                  value = 1,
                  min = 1,
                  max = 15,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("treeDepthRangeMax"),
                  label = "treeDepthRangeMax",
                  value = 15,
                  min = 1,
                  max = 15,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("treeDepthRangeLevels"),
                  label = "treeDepthRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## minN
          # decisionTree, randomForest, lightGBM, xgboost
          conditionalPanel(
            "input.algo == 'decisionTree' || input.algo=='randomForest' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("minNRangeMin"),
                  label = "minNRangeMin",
                  value = 2,
                  min = 2,
                  max = 40,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("minNRangeMax"),
                  label = "minNRangeMax",
                  value = 40,
                  min = 2,
                  max = 40,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("minNRangeLevels"),
                  label = "minNRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## costComplexity
          # decisionTree
          conditionalPanel(
            "input.algo == 'decisionTree'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("costComplexityRangeMin"),
                  label = "costComplexityRangeMin",
                  value = -2,
                  min = -10,
                  max = -1,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("costComplexityRangeMax"),
                  label = "costComplexityRangeMax",
                  value = -1,
                  min = -10,
                  max = -1,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("costComplexityRangeLevels"),
                  label = "costComplexityRangeLevels",
                  value = 2,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),

          ## mtry & trees
          # randomForest, XGboost, lightBGM
          conditionalPanel(
            "input.algo=='randomForest' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("mtryRangeMin"),
                  label = "mtryRangeMin",
                  value = 1,
                  min = 1,
                  max = 20,
                  step = 1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("mtryRangeMax"),
                  label = "mtryRangeMax",
                  value = 20,
                  min = 1,
                  max = 20,
                  step = 11
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("mtryRangeLevels"),
                  label = "mtryRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("treesRangeMin"),
                  label = "treesRangeMin",
                  value = 100,
                  min = 1,
                  max = 2000,
                  step = 50
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("treesRangeMax"),
                  label = "treesRangeMax",
                  value = 1000,
                  min = 1,
                  max = 2000,
                  step = 50
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("treesRangeLevels"),
                  label = "treesRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),


          ## learnRate & lossReduction
          # XGBOOST, LightGBM
          conditionalPanel(
            "input.algo == 'XGBoost' || input.algo == 'lightGBM'",
            ns = ns,
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("learnRateRangeMin"),
                  label = "learnRateRangeMin",
                  value = -2,
                  min = -10,
                  max = -1,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("learnRateRangeMax"),
                  label = "learnRateRangeMax",
                  value = -1,
                  min = -10,
                  max = -1,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("learnRateRangeLevels"),
                  label = "learnRateRangeLevels",
                  value = 2,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = ns("lossReductionRangeMin"),
                  label = "lossReductionRangeMin",
                  value = -1,
                  min = -10,
                  max = 1.5,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("lossReductionRangeMax"),
                  label = "lossReductionRangeMax",
                  value = 1,
                  min = -10,
                  max = 1.5,
                  step = 0.1
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("lossReductionRangeLevels"),
                  label = "lossReductionRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              )
            )
          ),


          ## sampleSize & stopIter
          # xgboost

          conditionalPanel(
            "input.algo == 'XGBoost'",
            ns = ns,
            fluidRow(
              column(
                width = 3,
                numericInput(
                  inputId = ns("sampleSizeRangeMin"),
                  label = "sampleSizeRangeMin",
                  value = 0,
                  min = 0,
                  max = 1,
                  step = 0.1
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = ns("sampleSizeRangeMax"),
                  label = "sampleSizeRangeMax",
                  value = 1,
                  min = 0,
                  max = 1,
                  step = 0.1
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = ns("sampleSizeRangeLevels"),
                  label = "sampleSizeRangeLevels",
                  value = 3,
                  min = 1,
                  max = 20,
                  step = 1
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = ns("stopIter"),
                  label = "stopIter",
                  value = 30,
                  min = 3,
                  max = 500,
                  step = 1
                )
              )
            )
          ),

          ## KMC

          conditionalPanel(
            "input.algo == 'KMC'",
            ns = ns,
            fluidRow(
              column(
                width = 3,
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
                width = 3,
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
                width = 3,
                numericInput(
                  inputId = ns("iterMax"),
                  label = "iterMax",
                  value = 10,
                  min = 1,
                  max = 5000,
                  step = 50
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = ns("nBoot"),
                  label = "nBoot",
                  value = 100,
                  min = 1,
                  max = 500,
                  step = 50
                )
              )
            ),
            fluidRow(
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
              ),
              column(
                width = 4,
                numericInput(
                  inputId = ns("seedNum"),
                  label = "seedNum",
                  value = 6471,
                  min = 1,
                  max = 9999,
                  step = 1
                )
              )
            )
          )
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
            v = input$fold,
            penaltyRangeMin = ifelse(is.null(input$penaltyRangeMin), 0.001, input$penaltyRangeMin),
            penaltyRangeMax = ifelse(is.null(input$penaltyRangeMax), 1.0, input$penaltyRangeMax),
            penaltyRangeLevels = ifelse(is.null(input$penaltyRangeLevels), 5, input$penaltyRangeLevels),
            mixtureRangeMin = ifelse(is.null(input$mixtureRangeMin), 0, input$mixtureRangeMin),
            mixtureRangeMax = ifelse(is.null(input$mixtureRangeMax), 1, input$mixtureRangeMax),
            mixtureRangeLevels = ifelse(is.null(input$mixtureRangeLevels), 5, input$mixtureRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

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
            v = input$fold,
            gridNum = input$gridNum, #
            iter = input$iter, #
            metric = input$metric,
            seed = input$seed #
          )
          Obj <- Obj$finalFittedModel

          Obj
        })
        models_list(
          append(models_list(), list("linearRegression_glmnet" = modelObj()))
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
            v = input$fold,
            neighborsRangeMin = ifelse(is.null(input$neighborsRangeMin), 1, input$neighborsRangeMin),
            neighborsRangeMax = ifelse(is.null(input$neighborsRangeMax), 10, input$neighborsRangeMax),
            neighborsRangeLevels = ifelse(is.null(input$neighborsRangeLevels), 10, input$neighborsRangeLevels),
            metric = input$metric
          )


          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("KNN_kknn" = modelObj()))
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
            smoothnessRangeMin = ifelse(is.null(input$smoothnessRangeMin), 0.5, input$smoothnessRangeMin),
            smoothnessRangeMax = ifelse(is.null(input$smoothnessRangeMax), 1.5, input$smoothnessRangeMax),
            smoothnessRangeLevels = ifelse(is.null(input$smoothnessRangeLevels), 3, input$smoothnessRangeLevels),
            LaplaceRangeMin = ifelse(is.null(input$LaplaceRangeMin), 0, input$LaplaceRangeMin),
            LaplaceRangeMax = ifelse(is.null(input$LaplaceRangeMax), 3, input$LaplaceRangeMax),
            LaplaceRangeLevels = ifelse(is.null(input$LaplaceRangeLevels), 4, input$LaplaceRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

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
            hiddenUnitsRangeMin = ifelse(is.null(input$hiddenUnitsRangeMin), 1, input$hiddenUnitsRangeMin),
            hiddenUnitsRangeMax = ifelse(is.null(input$hiddenUnitsRangeMax), 10, input$hiddenUnitsRangeMax),
            hiddenUnitsRangeLevels = ifelse(is.null(input$hiddenUnitsRangeLevels), 3, input$hiddenUnitsRangeLevels),
            penaltyRangeMin = ifelse(is.null(input$penaltyRangeMin), 0.001, input$penaltyRangeMin),
            penaltyRangeMax = ifelse(is.null(input$penaltyRangeMax), 1, input$penaltyRangeMax),
            penaltyRangeLevels = ifelse(is.null(input$penaltyRangeLevels), 3, input$penaltyRangeLevels),
            epochsRangeMin = ifelse(is.null(input$epochsRangeMin), 10, input$epochsRangeMin),
            epochsRangeMax = ifelse(is.null(input$epochsRangeMax), 100, input$epochsRangeMax),
            epochsRangeLevels = ifelse(is.null(input$epochsRangeLevels), 2, input$epochsRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("MLP_nnet" = modelObj()))
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
            treeDepthRangeMin = ifelse(is.null(input$treeDepthRangeMin), 1, input$treeDepthRangeMin),
            treeDepthRangeMax = ifelse(is.null(input$treeDepthRangeMax), 15, input$treeDepthRangeMax),
            treeDepthRangeLevels = ifelse(is.null(input$treeDepthRangeLevels), 3, input$treeDepthRangeLevels),
            minNRangeMin = ifelse(is.null(input$minNRangeMin), 2, input$minNRangeMin),
            minNRangeMax = ifelse(is.null(input$minNRangeMax), 40, input$minNRangeMax),
            minNRangeLevels = ifelse(is.null(input$minNRangeLevels), 3, input$minNRangeLevels),
            costComplexityRangeMin = ifelse(is.null(input$costComplexityRangeMin), -2, input$costComplexityRangeMin),
            costComplexityRangeMax = ifelse(is.null(input$costComplexityRangeMax), -1, input$costComplexityRangeMax),
            costComplexityRangeLevels = ifelse(is.null(input$costComplexityRangeLevels), 2, input$costComplexityRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("decisionTree_rpart" = modelObj()))
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
            mtryRangeMin = ifelse(is.null(input$mtryRangeMin), 1, input$mtryRangeMin),
            mtryRangeMax = ifelse(is.null(input$mtryRangeMax), 20, input$mtryRangeMax),
            mtryRangeLevels = ifelse(is.null(input$mtryRangeLevels), 3, input$mtryRangeLevels),
            treesRangeMin = ifelse(is.null(input$treesRangeMin), 100, input$treesRangeMin),
            treesRangeMax = ifelse(is.null(input$treesRangeMax), 1000, input$treesRangeMax),
            treesRangeLevels = ifelse(is.null(input$treesRangeLevels), 3, input$treesRangeLevels),
            minNRangeMin = ifelse(is.null(input$minNRangeMin), 2, input$minNRangeMin),
            minNRangeMax = ifelse(is.null(input$minNRangeMax), 40, input$minNRangeMax),
            minNRangeLevels = ifelse(is.null(input$minNRangeLevels), 3, input$minNRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("randomForest_ranger" = modelObj()))
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
            treeDepthRangeMin = ifelse(is.null(input$treeDepthRangeMin), 5, input$treeDepthRangeMin),
            treeDepthRangeMax = ifelse(is.null(input$treeDepthRangeMax), 15, input$treeDepthRangeMax),
            treeDepthRangeLevels = ifelse(is.null(input$treeDepthRangeLevels), 3, input$treeDepthRangeLevels),
            treesRangeMin = ifelse(is.null(input$treesRangeMin), 8, input$treesRangeMin),
            treesRangeMax = ifelse(is.null(input$treesRangeMax), 32, input$treesRangeMax),
            treesRangeLevels = ifelse(is.null(input$treesRangeLevels), 3, input$treesRangeLevels),
            learnRateRangeMin = ifelse(is.null(input$learnRateRangeMin), -2, input$learnRateRangeMin),
            learnRateRangeMax = ifelse(is.null(input$learnRateRangeMax), -1, input$learnRateRangeMax),
            learnRateRangeLevels = ifelse(is.null(input$learnRateRangeLevels), 2, input$learnRateRangeLevels),
            mtryRangeMin = ifelse(is.null(input$mtryRangeMin), 0, input$mtryRangeMin),
            minNRangeMax = ifelse(is.null(input$minNRangeMax), 1, input$minNRangeMax),
            minNRangeLevels = ifelse(is.null(input$minNRangeLevels), 3, input$minNRangeLevels),
            lossReductionRangeMin = ifelse(is.null(input$lossReductionRangeMin), -1, input$lossReductionRangeMin),
            lossReductionRangeMax = ifelse(is.null(input$lossReductionRangeMax), 1, input$lossReductionRangeMax),
            lossReductionRangeLevels = ifelse(is.null(input$lossReductionRangeLevels), 3, input$lossReductionRangeLevels),
            sampleSizeRangeMin = ifelse(is.null(input$sampleSizeRangeMin), 0, input$sampleSizeRangeMin),
            sampleSizeRangeMax = ifelse(is.null(input$sampleSizeRangeMax), 1, input$sampleSizeRangeMax),
            sampleSizeRangeLevels = ifelse(is.null(input$sampleSizeRangeLevels), 3, input$sampleSizeRangeLevels),
            stopIter = ifelse(is.null(input$stopIter), 30, input$stopIter),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("XGBoost_xgboost" = modelObj()))
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
            treeDepthRangeMin = ifelse(is.null(input$treeDepthRangeMin), 5, input$treeDepthRangeMin),
            treeDepthRangeMax = ifelse(is.null(input$treeDepthRangeMax), 15, input$treeDepthRangeMax),
            treeDepthRangeLevels = ifelse(is.null(input$treeDepthRangeLevels), 3, input$treeDepthRangeLevels),
            treesRangeMin = ifelse(is.null(input$treesRangeMin), 10, input$treesRangeMin),
            treesRangeMax = ifelse(is.null(input$treesRangeMax), 100, input$treesRangeMax),
            treesRangeLevels = ifelse(is.null(input$treesRangeLevels), 2, input$treesRangeLevels),
            learnRateRangeMin = ifelse(is.null(input$learnRateRangeMin), -2, input$learnRateRangeMin),
            learnRateRangeMax = ifelse(is.null(input$learnRateRangeMax), -1, input$learnRateRangeMax),
            learnRateRangeLevels = ifelse(is.null(input$learnRateRangeLevels), 2, input$learnRateRangeLevels),
            mtryRangeMin = ifelse(is.null(input$mtryRangeMin), 1, input$mtryRangeMin),
            mtryRangeMax = ifelse(is.null(input$mtryRangeMax), 20, input$mtryRangeMax),
            mtryRangeLevels = ifelse(is.null(input$mtryRangeLevels), 3, input$mtryRangeLevels),
            minNRangeMin = ifelse(is.null(input$minNRangeMin), 2, input$minNRangeMin),
            minNRangeMax = ifelse(is.null(input$minNRangeMax), 40, input$minNRangeMax),
            minNRangeLevels = ifelse(is.null(input$minNRangeLevels), 3, input$minNRangeLevels),
            lossReductionRangeMin = ifelse(is.null(input$lossReductionRangeMin), -1, input$lossReductionRangeMin),
            lossReductionRangeMax = ifelse(is.null(input$lossReductionRangeMax), 1, input$lossReductionRangeMax),
            lossReductionRangeLevels = ifelse(is.null(input$lossReductionRangeLevels), 3, input$lossReductionRangeLevels),
            metric = input$metric
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("lightGBM_lightgbm" = modelObj()))
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
            seedNum = ifelse(is.null(input$seedNum), 6471, input$seedNum),
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
        # if (input$reportML == "LinearR_glmnet") {
        vis_result <- stove::regressionPlot(
          modelName = input$reportML,
          modelsList = models_list(),
          targetVar = splitresult()$target
        )

        output$RegressionPlot <- renderPlot(vis_result)
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

    return(models_list)
  })
}

## To be copied in the UI
# mod_modelingModule_ui("modelingModule_1")

## To be copied in the server
# mod_modelingModule_server("modelingModule_1")
