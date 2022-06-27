#' variableModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr select
mod_variableModule_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("variableDescription"),
      label = "Select Variable",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
  )
}

#' variableModule Server Functions
#'
#' @noRd
mod_variableModule_server <- function(id, inputData, opened, distobj, distobj2){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(opened(), {
      if(opened()!="Variable"){return()}
      updateSelectizeInput(
        session,
        inputId = "variableDescription",
        label = "Select Variable",
        choices = c("", colnames(inputData())),
        server = TRUE
      )
    })

    observeEvent(input$variableDescription, {
        if(input$variableDescription==''){return(0)}

        numericV <- inputData() %>%
          dplyr::select(where(is.numeric)) %>%
          colnames()

        if(input$variableDescription %in% numericV){ # if is numeric only
          distobj(
              board::distribute(inputData()[,input$variableDescription])
          )
        }

        distobj2(
          board::ggpie(inputData()[,input$variableDescription])
        )

      }

    )
  })
}

## To be copied in the UI
# mod_variableModule_ui("variableModule_1")

## To be copied in the server
# mod_variableModule_server("variableModule_1")
