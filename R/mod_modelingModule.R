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
          choices = c(
            "Logistic Regression" = "LogisticR",
            "Linear Regression" = "LinearR",
            "K Nearest Neighbor" = "KNN",
            "Naive Bayes" = "NB",
            "MLP",
            "Decision Tree" = "DT",
            "Random Forest" = "RF",
            "XGBoost",
            "lightGBM",
            "K Means Clustering" = "KMC"
          )
        ),
        selectInput(
          inputId = ns("engine"),
          label = "engine 지정",
          choices = c(
            "glmnet", # logitistic Regression, Linera Regression
            "kknn", # KNN
            "kiaR", # NB
            "nnet", # MLP
            "rpart", # DT
            "ranger", # RF
            "xgboost", # XGBoost
            "lightgbm", # Light GBM,
            "-" # KMC
          )
        ),
        selectInput(
          inputId = ns("fold"),
          label = "v 지정",
          choices = c(1:20),
          selected = 5
        ),
        selectInput(
          inputId = ns("metric"),
          label = "metric 지정",
          choices = c("roc_auc", "rmse"),
          selected = "roc_auc"
        )
      ),
      column(
        width = 6,

        ## Penalty
        # Logistic / Linear / MLP
        conditionalPanel(
          "input.algo == 'LogisticR' || input.algo == 'LinearR' || input.algo == 'MLP'",
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
          "input.algo == 'LogisticR' || input.algo == 'LinearR'",
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
          "input.algo == 'NB'",
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
        # DT lightgbm, xgboost
        conditionalPanel(
          "input.algo == 'DT' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
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
        # DT, RF, lightGBM, xgboost
        conditionalPanel(
          "input.algo == 'DT' || input.algo=='RF' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
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
        # DT
        conditionalPanel(
          "input.algo == 'DT'",
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
        # RF, XGboost, lightBGM
        conditionalPanel(
          "input.algo=='RF' || input.algo == 'XGBoost' || input.algo == 'lightGBM'",
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
                inputId = ns("treesRangeLeels"),
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
        ),
        fluidRow(
          selectInput(
            inputId = ns("reportML"),
            label = "생성된 모델",
            choices = NULL,
            selected = NULL
          ),
          actionButton(
            inputId = ns("generateReport"),
            label = "report 생성"
          ),

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

    observeEvent(input$applyModel, {
      shinyjs::show(id = "models")

      if (input$algo == "LogisticR") {
        modelObj <- reactive({
          Obj <- goophi::logisticRegression(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula, ## Confirmed
            rec = splitresult()$rec,
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
        models_list(
          append(models_list(), list("LogisticR_glmnet" = modelObj()))
        )
      }

      if (input$algo == "LinearR") {
        modelObj <- reactive({
          Obj <- goophi::linearRegression(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
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
        models_list(
          append(models_list(), list("LinearR_glmnet" = modelObj()))
        )
      }


      ## CHECK
      if (input$algo == "KNN") {
        modelObj <- reactive({
          Obj <- goophi::KNN(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            neighborsRangeMin = input$neighborsRangeMin,
            neighborsRangeMax = input$neighborsRangeMax,
            neighborsRangeLevels = input$neighborsRangeLevels,
            metric = "roc_auc"
          )


          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("KNN_kknn" = modelObj()))
        )
      }

      if (input$algo == "NB") {
        modelObj <- reactive({
          Obj <- goophi::naiveBayes(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            smoothnessRangeMin = input$smoothnessRangeMin,
            smoothnessRangeMax = input$smoothnessRangeMax,
            smoothnessRangeLevels = input$smoothnessRangeLevels,
            LaplaceRangeMin = input$LaplaceRangeMin,
            LaplaceRangeMax = input$LaplaceRangeMax,
            LaplaceRangeLevels = input$LaplaceRangeLevels,
            metric = "roc_auc"
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("NB_klaR" = modelObj()))
        )
      }

      if (input$algo == "MLP") {
        modelObj <- reactive({
          Obj <- goophi::MLP(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            hiddenUnitsRangeMin = input$hiddenUnitsRangeMin,
            hiddenUnitsRangeMax = input$hiddenUnitsRangeMax,
            hiddenUnitsRangeLevels = input$hiddenUnitsRangeLevels,
            penaltyRangeMin = input$penaltyRangeMin,
            penaltyRangeMax = input$penaltyRangeMax,
            penaltyRangeLevels = input$penaltyRangeLevels,
            epochsRangeMin = input$epochsRangeMin,
            epochsRangeMax = input$epochsRangeMax,
            epochsRangeLevels = input$epochsRangeLevels,
            metric = "roc_auc"
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("MLP_nnet" = modelObj()))
        )
      }

      if (input$algo == "DT") {
        modelObj <- reactive({
          Obj <- goophi::decision_tree(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            treeDepthRangeMin = input$treeDepthRangeMin,
            treeDepthRangeMax = input$treeDepthRangeMax,
            treeDepthRangeLevels = input$treeDepthRangeLevels,
            minNRangeMin = input$minNRangeMin,
            minNRangeMax = input$minNRangeMax,
            minNRangeLevels = input$minNRangeLevels,
            costComplexityRangeMin = input$costComplexityRangeMin,
            costComplexityRangeMax = input$costComplexityRangeMax,
            costComplexityRangeLevels = input$costComplexityRangeLevels,
            metric = "roc_auc"
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("DT_rpart" = modelObj()))
        )
      }

      if (input$algo == "RF") {
        modelObj <- reactive({
          Obj <- goophi::randomForest(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            mtryRangeMin = input$mtryRangeMin,
            mtryRangeMax = input$mtryRangeMax,
            mtryRangeLevels = input$mtryRangeLevels,
            treesRangeMin = input$treesRangeMin,
            treesRangeMax = input$treesRangeMax,
            treesRangeLevels = input$treesRangeLevels,
            minNRangeMin = input$minNRangeMin,
            minNRangeMax = input$minNRangeMax,
            minNRangeLevels = input$minNRangeLevels,
            metric = "roc_auc"
          )

          Obj <- Obj$finalFittedModel

          Obj
        })

        models_list(
          append(models_list(), list("RF_ranger" = modelObj()))
        )
      }

      if (input$algo == "XGBoost") {
        modelObj <- reactive({
          Obj <- goophi::xgBoost(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            treeDepthRangeMin = input$treeDepthRangeMin,
            treeDepthRangeMax = input$treeDepthRangeMax,
            treeDepthRangeLevels = input$treeDepthRangeLevels,
            treesRangeMin = input$treesRangeMin,
            treesRangeMax = input$treesRangeMax,
            treesRangeLevels = input$treesRangeLevels,
            learnRateRangeMin = input$learnRateRangeMin,
            learnRateRangeMax = input$learnRateRangeMax,
            learnRateRangeLevels = input$learnRateRangeLevels,
            mtryRangeMin = input$mtryRangeMin,
            mtryRangeMax = input$mtryRangeMax,
            mtryRangeLevels = input$mtryRangeLevels,
            minNRangeMin = input$minNRangeMin,
            minNRangeMax = input$minNRangeMax,
            minNRangeLevels = input$minNRangeLevels,
            lossReductionRangeMin = input$lossReductionRangeMin,
            lossReductionRangeMax = input$lossReductionRangeMax,
            lossReductionRangeLevels = input$lossReductionRangeLevels,
            sampleSizeRangeMin = input$sampleSizeRangeMin,
            sampleSizeRangeMax = input$sampleSizeRangeMax,
            sampleSizeRangeLevels = input$sampleSizeRangeLevels,
            stopIter = input$stopIter,
            metric = "roc_auc"
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
          Obj <- goophi::lightGbm(
            algo = input$algo,
            engine = input$engine,
            mode = input$mode,
            trainingData = splitresult()$train,
            splitedData = splitresult()$dataSplit,
            formula = splitresult()$formula,
            rec = processresult(),
            v = input$fold,
            treeDepthRangeMin = input$treeDepthRangeMin,
            treeDepthRangeMax = input$treeDepthRangeMax,
            treeDepthRangeLevels = input$treeDepthRangeLevels,
            treesRangeMin = input$treesRangeMin,
            treesRangeMax = input$treesRangeMax,
            treesRangeLevels = input$treesRangeLevels,
            learnRateRangeMin = input$learnRateRangeMin,
            learnRateRangeMax = input$learnRateRangeMax,
            learnRateRangeLevels = input$learnRateRangeLevels,
            mtryRangeMin = input$mtryRangeMin,
            mtryRangeMax = input$mtryRangeMax,
            mtryRangeLevels = input$mtryRangeLevels,
            minNRangeMin = input$minNRangeMin,
            minNRangeMax = input$minNRangeMax,
            minNRangeLevels = input$minNRangeLevels,
            lossReductionRangeMin = input$lossReductionRangeMin,
            lossReductionRangeMax = input$lossReductionRangeMax,
            lossReductionRangeLevels = input$lossReductionRangeLevels,
            metric = "roc_auc"
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
          Obj <- goophi::kMeansClustering(
            data = data,
            maxK = input$maxK,
            nStart = input$nStart,
            iterMax = input$iterMax,
            nBoot = input$nBoot,
            algorithm = input$algorithm,
            selectOptimal = input$selectOptimal,
            seedNum = input$seedNum # CHECK
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
      if (input$reportML == "KmeansClustering") {
        data <- rbind(splitresult()$train, splitresult()$test)

        Obj <- models_list()$KmeansClustering

        vis_result <- goophi::clusteringVis(
          data = data,
          model = Obj, # pass
          maxK = input$maxK, # pass
          # nStart = input$nStart, # pass
          nBoot = input$nBoot, # pass
          selectOptimal = input$selectOptimal # pass
        )

        output$ClusterPlot <- renderPlot(vis_result$clustVis)
        output$optimalK <- renderPlot(vis_result$optimalK)
        output$ClusterResult <- renderPrint({
          Obj
        })
      }

      if (input$reportML == "LinearR_glmnet") {
        Obj <- models_list()$LinearR_glmnet

        vis_result <- goophi::regressionPlot(
          modelName = "LinearR_glmnet",
          modelsList = models_list(),
          targetVar = splitresult()$target
        )

        output$RegressionPlot <- renderPlot(vis_result)
        output$EvalMatrix <- renderPrint({
          goophi::evalMetricsR(
            modelsList = models_list(),
            targetVar = splitresult()$target
          )
        })
      }

      if (input$reportML == "LogisticR_glmnet") {
        Obj <- models_list()$LogisticR_glmnet

        rc <- goophi::rocCurve(
          modelsList = models_list(),
          targetVar = splitresult()$target
        )
        output$rocCurve <- renderPlot(rc)

        cm <- goophi::confusionMatrix(
          modelName = "LogisticR_glmnet",
          modelsList = models_list(),
          targetVar = splitresult()$target
        )

        output$confusionMatrix <- renderPlot(cm)

        output$EvalMatrix <- renderPrint({
          goophi::evalMetricsC(
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
            "Logistic Regression" = "LogisticR",
            "K Nearest Neighbor" = "KNN",
            "Naive Bayes" = "NB",
            "MLP",
            "Decision Tree" = "DT",
            "Random Forest" = "RF",
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
            "Linear Regression" = "LinearR",
            "K Nearest Neighbor" = "KNN",
            "MLP",
            "Decision Tree" = "DT",
            "Random Forest" = "RF",
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
          choices = "KMC",
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
      if (input$algo == "LogisticR") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "glmnet" # logitistic Regression, Linera Regression
        )
      }
      if (input$algo == "LinearR") {
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
      if (input$algo == "NB") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "kiaR" # NB
        )
      }
      if (input$algo == "MLP") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "nnet", # MLP
        )
      }
      if (input$algo == "DT") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "rpart" # DT
        )
      }
      if (input$algo == "RF") {
        updateSelectInput(
          inputId = "engine",
          label = "engine 지정",
          choices = "ranger" # RF
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
