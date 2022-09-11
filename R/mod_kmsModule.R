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
  tagList(
    sliderInput(ns('k'),'k',min = 2, max = 10, value = 4, step = 1),
    checkboxInput(ns('scale'), 'scale',value = TRUE),
    actionButton(ns('cluster'), 'cluster'),
    plotlyOutput(ns('plot'))
  )
}

#' kmsModule Server Functions
#'
#' @noRd
mod_kmsModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    req(inputData)

    observeEvent(input$cluster, {

      data <- inputData()

      data <- Filter(is.numeric, data)
      if(input$scale){
        data <- scale(data)
      }

      km.res <- kmeans(data, centers = input$k)
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
