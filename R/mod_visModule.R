#' visModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotGen
#' @import plotly
mod_visModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # type of chart
    selectInput(
      inputId = ns("chartType"),
      label = "Select Chart",
      choices = c("", "pie"),
      selected = ""
    ),
    # x
    selectInput(
      inputId = ns("xColumn"),
      label = "select X",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    # y
    selectInput(
      inputId = ns("yColumn"),
      label = "select Y",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    actionButton(
      inputId = ns("plotGen"),
      label = "plot"
    )
  )
}

#' visModule Server Functions
#'
#' @noRd
mod_visModule_server <- function(id, inputData, opened, plotlyobj) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(opened(), {
      if (opened() != "vis") {
        return()
      }
      updateSelectizeInput(
        session,
        inputId = "xColumn", # ns() not work
        label = "select X",
        choices = colnames(inputData()),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "yColumn", # ns() not work
        label = "select Y",
        choices = c("", colnames(inputData())),
        server = TRUE
      )
    })

    observeEvent(input$plotGen, {
      yval <- ifelse(is.null(input$yColumn), NULL, input$yColumn)

      plotlyobj(
        plotGen::plotGen(
          data = inputData(),
          type = input$chartType,
          criteria = input$xColumn,
          describe = yval
        )
      )
    })
  })
}

## To be copied in the UI
# mod_visModule_ui("visModule_1")

## To be copied in the server
# mod_visModule_server("visModule_1")
