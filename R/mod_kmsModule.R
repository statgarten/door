#' kmsModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom factoextra fviz_cluster
#' @importFrom plotly ggplotly plotlyOutput
#' @importFrom shiny NS tagList
mod_kmsModule_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 9,
      plotlyOutput(ns('plot'), width = '100%')
    ),
    column( # Options
      width = 3,
      sliderInput(
        ns('k'),
        'k',
        min = 2,
        max = 10,
        value = 4,
        step = 1,
        width = '100%'
      ),
      checkboxInput(
        ns('scale'),
        'scale',
        value = TRUE,
        width = '100%'
      ),
      selectInput(
        inputId = ns("labels"),
        label = "",
        choices = NULL,
        width = '100%'
      ),
      actionButton( # Main Action
        ns('cluster'),
        'cluster',
        style = 'font-weight: bold;background: #3EC70B;color: white; width: 100%'
      )
    )
  )
}

#' kmsModule Server Functions
#'
#' @noRd
mod_kmsModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    req(inputData)


    observeEvent(inputData(), {
      data <- inputData()

      updateSelectizeInput(
        inputId = "labels",
        label = "Labels-Opt (Character)",
        choices = c('NULL', names(Filter(is.character, data)))
      )
    })

    observeEvent(input$cluster, {

      data <- inputData()

      print(input$labels)
      if(input$labels != 'NULL'){ # keep label variable
        labels <- data[[input$labels]]
      }

      data <- Filter(is.numeric, data) # select numeric only

      if(input$scale){
        data <- scale(data)
      }

      km.res <- kmeans(data, centers = input$k)

      if(input$labels != 'NULL'){
        rownames(data) <- labels
      }

      output$plot <- renderPlotly({
        plotly::ggplotly(
          factoextra::fviz_cluster(
            km.res,
            data = data,
            ggtheme = theme_minimal()
          ) +
            theme(legend.position = 'none')
        )
      })
    })

  })
}

## To be copied in the UI
# mod_kmsModule_ui("kmsModule_1")

## To be copied in the server
# mod_kmsModule_server("kmsModule_1")
