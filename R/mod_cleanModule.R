#' cleanModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cleanModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    ## LOAD COLUMNS
    actionButton(
      inputId = ns("loadCleanColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),

    ## <SELECT> column names
    selectInput(
      inputId = ns("cleanColumn"),
      label = "cleanSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    ## Operation Option: Remove / Replace
    selectInput(
      inputId = ns("cleanOperator"),
      label = "cleanOpeartorLabel",
      choices = c("Remove", "Replace"),
      selected = NULL,
      multiple = FALSE
    ),

    ## Remove / Replace Keyword: null, [userInput]
    textInput(
      inputId = ns("cleanVariable"),
      label = "cleanVariableLabel",
    ),
    textInput(
      inputId = ns("cleanKeyword"),
      label = "cleanKeywordLabel",
    ),

    ## <Button> Clean
    actionButton(
      inputId = ns("cleanButton"),
      label = "clean",
      icon = icon("angle-down")
    )
  )
}

#' cleanModule Server Functions
#'
#' @noRd
mod_cleanModule_server <- function(id, inputData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$loadCleanColumn, {
      updateSelectizeInput(
        session,
        inputId = "cleanColumn",
        label = "cleanSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$cleanButton, {
      if (input$cleanOperator == "Remove") {
        # data $ column -> filter(which is not .)

        if (input$cleanVariable == "NA") {
          eval(parse(
            text =
              paste0(
                "inputData( inputData() %>% ",
                "filter(!is.na(", input$cleanColumn, ")))"
              )
          ))
        } else {
          eval(parse(
            text =
              paste0(
                "inputData( inputData() %>% ",
                "filter(!grepl(", input$cleanVariable, ", ", input$cleanColumn, ")))"
              )
          ))
        }
      }

      if (input$cleanOperator == "Replace") {
        keyword <- ifelse(is.null(input$cleanKeyword), "", input$cleanKeyword)

        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(", input$cleanColumn, " = ",
              "ifelse(", input$cleanColumn, " == ", input$cleanVariable, ",", keyword, ",", input$cleanColumn, ")))"
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
# mod_cleanModule_ui("cleanModule_1")

## To be copied in the server
# mod_cleanModule_server("cleanModule_1")
