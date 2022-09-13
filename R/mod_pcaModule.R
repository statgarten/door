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
    shinyjs::hidden(
      uiOutput(outputId = ns("biplotSlot"))
    ),
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = ns("columns"),
          label = "",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        width = 3,
        selectInput(inputId = ns("group"), label = "", choices = NULL)
      ),
      column(
        width = 3,
        selectInput(inputId = ns("labels"), label = "", choices = NULL)
      ),
      column(
        width = 3,
        fluidRow(
          sliderInput(
            inputId = ns("slotSize"),
            label = "height of plot",
            min = 400, max = 1000, step = 50, value = 400,
            ticks = FALSE
          ),
          checkboxInput(inputId = ns("scale"), "variable normalize",value = TRUE)
        )
      )
    ),
    actionButton(inputId = ns("pca"), label = "draw", style = 'font-weight: bold;background: #3EC70B;color: white; width: 100%')
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

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "columns",
        label = "PCA columns (Numeric)",
        choices = names(Filter(is.numeric, data))
      )

      updateSelectizeInput(
        inputId = "labels",
        label = "Labels-Opt (Character)",
        choices = c('NULL', names(Filter(is.character, data)))
      )

      updateSelectInput(
        inputId = "group",
        label = "Group columns (Factor)",
        choices = names(Filter(is.factor, data))
      )
    })

    observeEvent(input$pca, { # draw plot
      req(input$pca)

      data <- inputData()

      groups <- data[[input$group]]
      labels <- data[[input$labels]]

      shinyjs::show(id = "biplotSlot")

      output$biplot <- renderPlotly({

        data <- data %>%
          select(input$columns) %>%
          replace(is.na(.), 0)

        pp <- prcomp(data, scale. = input$scale)

        if(input$labels =='NULL'){labels <- NULL}

        ggbiplot(
          pp,
          obs.scale = 1,
          var.scale = 1,
          labels = labels,
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
