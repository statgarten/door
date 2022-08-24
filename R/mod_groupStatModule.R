#' groupStatModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom reactable reactableOutput reactable renderReactable
mod_groupStatModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactableOutput(
      outputId = ns('myTable')
    ),
    selectizeInput(
      inputId = ns('groups'),
      label = 'groups',
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(
      inputId = ns('func'),
      label = 'func',
      choices = c("mean", 'median', 'sd', 'iqr' = 'IQR', 'mad', 'min', 'max')
    ), # not quantile, first, last, nth, n, n_distint
    actionButton(
      inputId = ns('build'),
      label = 'build'
    )
  )
}

#' groupStatModule Server Functions
#'
#' @import dplyr
#' @noRd
mod_groupStatModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = 'groups',
        label = 'groups',
        choices = names(Filter(is.factor, data))
      )
    })

    observeEvent(input$build,{
      req(input$build)
      data <- inputData()
      v <- data %>%
        group_by(across(input$groups)) %>%
        summarise(across(names(Filter(is.numeric, data)), list(input$func))) %>%
        reactable()

      output$myTable <- renderReactable({v})
    })

  })
}

## To be copied in the UI
# mod_groupStatModule_ui("groupStatModule_1")

## To be copied in the server
# mod_groupStatModule_server("groupStatModule_1")
