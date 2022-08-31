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
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("columns"),
          label = "",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        width = 4,
        selectInput(inputId = ns("group"), label = "", choices = NULL)
      ),
      column(
        width = 4,
        fluidRow(
          sliderInput(
            inputId = ns("slotSize"),
            label = "height of plot",
            min = 400, max = 1000, step = 50, value = 400,
            ticks = FALSE
          ),
          checkboxInput(inputId = ns("scale"), "variable normalize")
        )
      )
    ),
    actionButton(inputId = ns("pca"), label = "draw")
  )
}

#' pcaModule Server Functions
#'
#' @noRd
mod_pcaModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    showtext_auto()
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
        label = "PCA columns (Numeric)",
        choices = names(Filter(is.numeric, data))
      )

      updateSelectInput(
        inputId = "group",
        label = "Group columns (Factor)",
        choices = names(Filter(is.factor, data))
      )
    })

    observeEvent(input$pca, {
      req(input$pca)

      data <- inputData()

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
