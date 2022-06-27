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
#' @importFrom shinydashboardPlus descriptionBlock
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
mod_variableModule_server <- function(id, inputData, opened, distobj, distobj2, uiobj){
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

          des <- board::describe(inputData()[,input$variableDescription])
          out <- board::outlier(inputData()[,input$variableDescription])
          uiobj(
            tagList(
              fluidRow(
                column(width = 6, descriptionBlock(header = des$count, number = 'Count', marginBottom = FALSE) ),
                column(width = 6, descriptionBlock(header = des$m, number = 'Mean', marginBottom = FALSE) )
              ),
              fluidRow(
                column(width = 6, descriptionBlock(header = des$q2, number = 'Median', marginBottom = FALSE) ),
                column(width = 6, descriptionBlock(header = des$s, number = 'Std', marginBottom = FALSE ) )
              ),
              fluidRow(
                column(width = 6, descriptionBlock(header = des$q0, number = 'Min', marginBottom = FALSE) ),
                column(width = 6, descriptionBlock(header = des$q1, number = '25%', marginBottom = FALSE) )
              ),
              fluidRow(
                column(width = 6, descriptionBlock(header = des$q3, number = '75%', marginBottom = FALSE) ),
                column(width = 6, descriptionBlock(header = des$q4, number = 'Max', marginBottom = FALSE ) )
              ),
              fluidRow(
                column(width = 3, descriptionBlock(header = out$lo, number = 'Large Outlier Count', marginBottom = FALSE) ),
                column(width = 3, descriptionBlock(header = out$ov, number = 'Large Outlier Value', marginBottom = FALSE) ),
                column(width = 3, descriptionBlock(header = out$lu, number = 'Small Outlier Count', marginBottom = FALSE) ),
                column(width = 3, descriptionBlock(header = out$uv, number = 'Small Outlier Value', marginBottom = FALSE) )
              )
            )
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
