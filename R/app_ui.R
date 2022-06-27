#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny dplyr tidyr
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box dashboardSidebar dashboardPage dashboardFooter dashboardControlbar descriptionBlock
#' @importFrom reactable reactableOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "StatGarten",
        titleWidth = 300,
        controlbarIcon = icon("gear", verify_fa = FALSE),
        leftUi = tagList(
          div(
            shinyWidgets::radioGroupButtons(
              inputId = "module",
              label = NULL,
              # status = 'warning',
              choices = c("Import", "Vis", "EDA", "Report"),
              selected = "Import",
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", style = "color: gold"),
                no = tags$i(class = "fa fa-circle-o", style = "color: gold")
              ) # ,
              # width = "10em"
            ),
            style = "margin-bottom: -11.5px; text-align: center; font-weight: bold;"
          )
        )
      ),

      # SIDE MODULE
      sidebar = dashboardSidebar(
        minified = FALSE,
        width = 300,
        htmlOutput("Logo", style = "text-align: center; margin-bottom:3em; margin-top:3em;"),
        conditionalPanel(
          condition = 'input.module == "Import"',
          shinyjs::hidden(
            div(
              id = "ImportBox",
              style = "text-align:center;",
              shinyWidgets::pickerInput(
                inputId = "ImportFunction",
                label = "Functions",
                choices = c("", "Filter", "Subset", "Mutate", "Clean", "Split", "Reshape", "Export"),
                choicesOpt = list(
                  subtext = c("", "fil", "sub", "mut", "cle", "spl", "res", "exp"),
                  style = rep(c("color: black"), 8)
                ),
                options = list(
                  style = "btn-info"
                ),
                selected = NULL
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Filter"',
                mod_filterModule_ui("filterModule_1")
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Subset"',
                mod_subsetModule_ui("subsetModule_1")
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Mutate"',
                mod_mutateModule_ui("mutateModule_1"),
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Clean"',
                mod_cleanModule_ui("cleanModule_1")
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Split"',
                mod_splitModule_ui("splitModule_1")
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Reshape"',
                mod_reshapeModule_ui("reshapeModule_1")
              ),
              conditionalPanel(
                condition = 'input.ImportFunction == "Export"',
                mod_exportModule_ui("exportModule_1")
              )
            )
          )
        ),
        conditionalPanel(
          condition = 'input.module == "Vis"',
          p("Not Implemented")
        ),
        conditionalPanel(
          condition = 'input.module == "EDA"',
          shinyjs::hidden(
            div(
              id = "EDABox",
              style = "text-align:center;",
              shinyWidgets::pickerInput(
                inputId = "EDAFunction",
                label = "EDA Functions",
                choices = c("", "Brief", "Relation", "Variable"),
                choicesOpt = list(
                  subtext = c("", "bri", "rel", "var"),
                  style = rep(c("color: black"), 4)
                ),
                options = list(
                  style = "btn-info"
                ),
                selected = NULL
              ),
              conditionalPanel(
                condition = 'input.EDAFunction == "Brief"',
                mod_briefModule_ui("briefModule_1")
              ),
              conditionalPanel(
                condition = 'input.EDAFunction == "Relation"',
                mod_relationModule_ui("relationModule_1")
              ),
              conditionalPanel(
                condition = 'input.EDAFunction == "Variable"',
                mod_variableModule_ui("variableModule_1")
              )
            )
          )
        ),
        conditionalPanel(
          condition = 'input.module == "Report"',
          p("Not Implemented")
        )
        # OUTRO
      ),
      body = dashboardBody(
        fluidPage(
          useShinyjs(),
          div(
            style = "margin-bottom : 1em; padding-right: 15px; padding-left: 15px;",
            reactable::reactableOutput(
              outputId = "DT"
            )
          ),
          ### Import panel
          conditionalPanel(
            condition = 'input.module == "Import"',
            p(
              id = "desc",
              "Description Text will be here"
            ),
            fileInput(
              inputId = "fileInputID",
              label = NULL,
              accept = c(".csv", ".tsv", ".sas7bdat", ".sas7bcat", ".sav", ".dta", ".xls", ".xlsx", ".rda", ".rds", ".rdata"),
              buttonLabel = "Browse local files",
              placeholder = "or Drag & Drop in Here",
              multiple = FALSE,
              width = "100%"
            ),
            shinyjs::disabled(
              shinyjs::hidden(
                shinyWidgets::prettySwitch(
                  inputId = "showAll",
                  label = "Show every data",
                  status = "success",
                  value = TRUE,
                  fill = TRUE
                )
              )
            )
          ),

          ### EDA panel
          conditionalPanel(
            condition = 'input.module == "EDA"',
            shinydashboardPlus::box(
              title = "Dataset Description",
              status = "purple",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  width = 6,
                  uiOutput("dataDimension")
                ),
                column(
                  width = 6,
                  uiOutput("missingData")
                )
              )
            ),
            shinydashboardPlus::box(
              title = "Variables",
              status = "purple",
              solidHeader = TRUE,
              width = 12,
              reactableOutput("reactOutput")
            ),
            shinydashboardPlus::box(
              title = "Correlation",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              plotOutput("corplot")
            ),
            shinydashboardPlus::box(
              title = "Correlation2",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              plotOutput("corplot2")
            ),
            shinydashboardPlus::box(
              title = "Distribution (Numeric Only)" ,
              status = "navy",
              solidHeader = TRUE,
              width = 12,
              plotOutput("distplot")
            ),
            shinydashboardPlus::box(
              title = "Distribution" ,
              status = "navy",
              solidHeader = TRUE,
              width = 12,
              plotOutput("distplot2")
            ),

          )
        )
      ),
      controlbar = dashboardControlbar(disable = TRUE),
      footer = dashboardFooter(
        left = NULL,
        right = actionButton(
          inputId = "Outro",
          label = "Github / Manual",
          style = "margin: auto; width: 100%",
          onclick = "window.open('https://github.com/statgarten', '_blank')",
          icon = icon("github")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "statgarten"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
