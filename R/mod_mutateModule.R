#' mutateModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon
mod_mutateModule_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # load column
    actionButton(
      inputId = ns("loadMutateColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),

    # which column
    selectInput(
      inputId = ns("mutateColumn"),
      label = "mutateSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    # option
    selectInput(
      inputId = ns("mutateOperator"),
      label = "mutateOpeartorLabel",
      choices = c("Round", "Log", "Log10", "Sqrt", "-", "Min-Max", "Normal", "Binarize(not)"),
      selected = NULL,
      multiple = FALSE
    ),

    #
    textInput(
      inputId = ns("mutateVariable"),
      label = "mutateVariableLabel",
    ),

    # apply button
    actionButton(
      inputId = ns("mutateButton"),
      label = "mutate",
      icon = icon("angle-down")
    )
  )
}

#' mutateModule Server Functions
#'
#' @noRd
mod_mutateModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$loadMutateColumn, {
      updateSelectizeInput(
        session,
        inputId = "mutateColumn",
        label = "mutateSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$mutateButton, {
      if (input$mutateOperator == "Round") {
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = round(", input$mutateColumn, ", ", input$mutateVariable, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "Log"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = log(", input$mutateColumn, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "Log10"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = log10(", input$mutateColumn, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "Sqrt"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = sqrt(", input$mutateColumn, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "-"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = -(", input$mutateColumn, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "Min-Max"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = minmax(", input$mutateColumn, ")) )"
            )
        ))
      }

      if(input$mutateOperator == "Normal"){
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = normalize(", input$mutateColumn, ")) )"
            )
        ))
      }


      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

## To be copied in the UI
# mod_mutateModule_ui("mutateModule_1")

## To be copied in the server
# mod_mutateModule_server("mutateModule_1")
