#' mlrModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import caret
#' @importFrom shiny NS tagList
mod_mlrModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 9,
      column(
        width = 6,
        plotOutput(ns("plot"), width = "100%")
      ),
      column(
        width = 6,
        verbatimTextOutput(ns("text"))
      )
    ),
    column( # Options
      width = 3,
      selectInput(
        ns("x"),
        "x",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      selectInput(
        ns("y"),
        "y",
        choices = NULL,
        width = "100%"
      ),
      actionButton( # Main Action
        ns("reg"),
        "reg",
        style = "font-weight: bold;background: #3EC70B;color: white; width: 100%"
      )
    )
  )

  # actionLink(
  #   ns('interpret'),
  #   'Interpretation of Linear Regression Output',
  #   onclick = 'window.open("https://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output", "_blank")'
  # )
}

#' mlrModule Server Functions
#'
#' @noRd
mod_mlrModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(inputId = "x", label = "x", choices = colnames(data))
      updateSelectizeInput(inputId = "y", label = "y", choices = colnames(data))
    })

    observeEvent(input$reg, {
      data <- inputData()
      x <- paste0(input$x, collapse = " + ") # 1
      f <- as.formula(paste0(input$y, " ~ ", x)) # 2
      model <- step(lm(f, data), trace = 0)

      output$text <- renderPrint({
        summary(model)
      })

      vI <- caret::varImp(model)
      vI$Features <- rownames(vI)
      colnames(vI)[1] <- "Importance"
      rownames(vI) <- NULL
      output$plot <- renderPlot({
        ggplot(
          data = vI,
          aes(y = reorder(Features, Importance), x = Importance, fill = Importance)
        ) +
          geom_bar(stat = "identity") +
          theme(legend.position = "none") +
          xlab(NULL) +
          ylab(NULL) +
          ggtitle("Feature Importance")
      })
    })
  })
}

## To be copied in the UI
# mod_mlrModule_ui("mlrModule_1")

## To be copied in the server
# mod_mlrModule_server("mlrModule_1")
