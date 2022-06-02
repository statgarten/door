#' splitModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_splitModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("loadSplitColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    selectInput(
      inputId = ns("splitColumn"),
      label = "splitSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    # keyword
    textInput(
      inputId = ns("splitkeyword"),
      label = "splitKeywordLabel"
    ),

    # colnameA
    textInput(
      inputId = ns("splitA"),
      label = "colA"
    ),
    # colnameB
    textInput(
      inputId = ns("splitB"),
      label = "colB"
    ),
    actionButton(
      inputId = ns("splitButton"),
      label = "split",
      icon = icon("angle-down")
    )
  )
}

#' splitModule Server Functions
#'
#' @noRd
mod_splitModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$loadSplitColumn, {
      updateSelectizeInput(
        session,
        inputId = "splitColumn",
        label = "splitSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$splitButton, {
      eval(parse(
        text =
          paste0(
            "inputData( inputData() %>% ",
            "tidyr::separate(", input$splitColumn, ", sep = '", input$splitkeyword, "', into = c('", input$splitA, "','", input$splitB, "'), fill = 'right'))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

## To be copied in the UI
# mod_splitModule_ui("splitModule_1")

## To be copied in the server
# mod_splitModule_server("splitModule_1")
