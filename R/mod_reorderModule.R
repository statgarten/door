#' reorderModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reorderModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("Sortable")),
    # h4('Example'),
    # verbatimTextOutput(
    #   ns('description')
    # )
  )
}

#' reorderModule Server Functions
#'
#' @noRd
mod_reorderModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$Sortable <- renderUI({
      rank_list(
        text = "Columns",
        labels = colnames(inputData()),
        input_id = ns("columns"),
        class = c("default-sortable") # add custom style
      )
    })

    data_reorder <-reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::reorder(
        inputData = data,
        column = input$columns
      )

      data
    })

    return(data_reorder)

  })
}

## To be copied in the UI
# mod_reorderModule_ui("reorderModule_1")

## To be copied in the server
# mod_reorderModule_server("reorderModule_1")
