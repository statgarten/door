#' filterModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @import dplyr
#' @importFrom shinyjs disabled enable
#' @importFrom scissor subset
#'
mod_filterModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("filterColumn"),
      label = "filterSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    selectInput(
      inputId = ns("filterOperator"),
      label = NULL,
      choices = c(">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"),
      selected = NULL,
      multiple = FALSE,
      width = "100%"
    ),
    textInput(
      inputId = ns("filterVariable"),
      label = NULL,
      placeholder = "criteria",
      width = "100%"
    ),
    selectInput(
      inputId = ns(paste0("cond", 1)),
      label = NULL,
      choices = c("And", "Or")
    ),
    uiOutput(ns("filterPage2")),

    ###

    actionButton(
      inputId = ns("addFilter"),
      label = NULL,
      icon = icon("plus", class = NULL, lib = "font-awesome")
    ),
    actionButton(
      inputId = ns("filterButton"),
      label = "filter",
      icon = icon("angle-down"),
      width = "90%",
      style = "margin: auto;"
    )
  )
}

#' filterModule Server Functions
#'
#' @noRd
mod_filterModule_server <- function(id, inputData, opened) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if (opened() != "Filter") {
        return()
      }
      updateSelectizeInput(
        session,
        inputId = "filterColumn", # ns() not work
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$filterButton, {
      inputData(
        scissor::subset(
          inputData = inputData(),
          column = input$filterColumn,
          operator = input$filterOperator,
          value = input$filterVariable
        )
      )
    })
  })
}

## To be copied in the UI
# mod_filterModule_ui("filterModule_1")

## To be copied in the server
# mod_filterModule_server("filterModule_1")
