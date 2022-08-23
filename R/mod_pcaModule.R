#' pcaModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom showtext showtext_auto
#' @import ggbiplot
#' @importFrom plotly ggplotly
mod_pcaModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("biplotSlot")),
    sliderInput(inputId = ns("slotSize"), label = "height of plot", min = 400, max = 1000, step = 50, value = 400),
    selectInput(inputId = ns("columns"), label = "columns", choices = NULL, multiple = TRUE),
    checkboxInput(inputId = ns("scale"), "scale"),
    selectInput(inputId = ns("group"), label = "group", choices = NULL),
    actionButton(inputId = ns("pca"), label = "draw")
  )
}

#' pcaModule Server Functions
#'
#' @noRd
mod_pcaModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)
    observeEvent(input$slotSize, { # change slot Size
      req(input$slotSize)
      output$biplotSlot <- renderUI({
        plotlyOutput(outputId = ns("biplot"), height = paste0(input$slotSize, "px"))
      })
    })

    # data <- scholar %>% mutate(학제별 = as.factor(학제별), 설립별 = as.factor(설립별))

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "columns",
        label = "columns",
        choices = names(Filter(is.numeric, data))
      )

      updateSelectInput(
        inputId = "group",
        label = "group",
        choices = names(Filter(is.factor, data))
      )
    })

    observeEvent(input$pca, {
      data <- inputData()
      showtext_auto()
      output$biplot <- renderPlotly({
        groups <- data[[input$group]]
        data <- data %>%
          select(input$columns) %>%
          replace(is.na(.), 0)
        pp <- prcomp(data, scale. = input$scale)
        ggbiplot(
          pp,
          obs.scale = 1,
          var.scale = 1,
          ellipse = TRUE,
          circle = TRUE,
          groups = groups
        ) %>%
          ggplotly()
      })
    })
  })
}

## To be copied in the UI
# mod_pcaModule_ui("pcaModule_1")

## To be copied in the server
# mod_pcaModule_server("pcaModule_1")
