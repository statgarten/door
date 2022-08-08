#' mlReportModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mlReportModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns('models'), label = 'select Model', choices = NULL, selected = NULL),
    actionButton(inputId = ns('apply'), label = 'apply'),

    shinyjs::hidden(
      div(
        id = ns('clusters'),
        plotOutput(outputId = ns('ClusterPlot')),
        plotOutput(outputId = ns('optimalK'))
      )
    )


  )
}

#' mlReportModule Server Functions
#'
#' @noRd
mod_mlReportModule_server <- function(id, models_list, splitresult, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(models_list,{
      req(models_list)
      updateSelectizeInput(
        inputId = 'models',
        label = 'select Model',
        choices = names(models_list()),
        selected = NULL
      )
    })


    observeEvent(input$apply,{
      if(grepl('cluster', input$models)){



        data <- rbind(
          splitresult()$train,
          splitresult()$test
        )

        params <- models_list()$params

        model <- isolate(input$models)


        output$ClusterPlot <- renderPlot(vis_resut$clustVis)
        output$optimalK <- renderPlot(vis_result$optimalK)
        shinyjs::show('clusters')
      }

    })



  })
}

## To be copied in the UI
# mod_mlReportModule_ui("mlReportModule_1")

## To be copied in the server
# mod_mlReportModule_server("mlReportModule_1")
