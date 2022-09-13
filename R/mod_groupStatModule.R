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
mod_groupStatModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(
      outputId = ns("myTable")
    ),
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          inputId = ns("groups"),
          label = "",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        width = 6,
        selectInput(
          inputId = ns("func"),
          label = "summary",
          choices = c("mean", "median", "sd", "iqr" = "IQR", "mad", "min", "max")
        ) # not quantile, first, last, nth, n, n_distint
      )
    ),
    actionButton(
      inputId = ns("build"),
      label = "build",
      style = 'font-weight: bold;background: #3EC70B;color: white; width: 100%'
    )
    #,
    # tags$button(
    #   tagList(fontawesome::fa("download"), "Download as CSV"),
    #   class = "btn btn-default shiny-download-link",
    #   onclick = "Reactable.downloadDataCSV('groupStatModule_1-myTable', 'downloads.csv')"
    # )
  )
}

#' groupStatModule Server Functions
#'
#' @import dplyr
#' @noRd
mod_groupStatModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "groups",
        label = "group by (Factor / Character)",
        choices = union(names(Filter(is.factor, data)), names(Filter(is.character, data)))
      )
    })

    observeEvent(input$build, {
      req(input$build)
      data <- inputData()
      v <- data %>%
        group_by(across(input$groups)) %>%
        summarise(across(names(Filter(is.numeric, data)), match.fun(input$func), na.rm = TRUE)) %>%
        reactable()

      output$myTable <- renderReactable(v)
    })
  })
}

## To be copied in the UI
# mod_groupStatModule_ui("groupStatModule_1")

## To be copied in the server
# mod_groupStatModule_server("groupStatModule_1")
