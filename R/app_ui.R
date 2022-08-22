#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny dplyr tidyr
#' @import plotly
#' @import datamods
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @import esquisse
#' @import shiny.i18n
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box dashboardHeader dashboardSidebar dashboardPage dashboardFooter dashboardControlbar descriptionBlock
#' @importFrom reactable reactableOutput
#' @importFrom shinyBS bsTooltip bsPopover bsButton
#' @noRd
app_ui <- function(request) {
  i18n_shiny <- golem::get_golem_options(which = "translator")
  i18n_shiny$set_translation_language("en")

  tagList(
    shiny.i18n::usei18n(i18n_shiny),
    golem_add_external_resources(),
    dashboardPage(
      skin = "black",
      header = dashboardHeader(
        title = i18n_shiny$t("Statgarten"),
        titleWidth = NULL,
        controlbarIcon = icon("gear", verify_fa = FALSE), # to hide error
        leftUi = tagList(
          #### LANGUAGES
          radioGroupButtons(
            inputId = "lang",
            label = NULL,
            choiceNames = lapply(seq_along(c("en", "kr")), function(i) tagList(tags$img(src = flags[i], width = 30, height = 20))),
            choiceValues = i18n_shiny$get_languages(),
            individual = TRUE
          )
        )
      ),
      # Sidebar
      sidebar = dashboardSidebar(disable = TRUE, minified = FALSE, width = 0),
      body = dashboardBody(
        useShinyjs(),
        fluidPage(
          ## Module Selector
          shinyjs::disabled( # used to enable after file upload
            div(
              id = "moduleSelector",
              shinyWidgets::radioGroupButtons(
                inputId = "module",
                label = NULL,
                choices = c("Import", "Vis", "EDA", "Stat", "Report", "ML"),
                selected = "Import",
                individual = TRUE,
                justified = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-circle", style = "color: #37E2D5"),
                  no = tags$i(class = "fa fa-circle", style = "color: #FBCB0A")
                )
              )
            )
          ),
          # Data View
          shinyjs::hidden(
            div(
              id = "viewModule",
              shinydashboardPlus::box(
                title = i18n_shiny$t("Uploaded Data"),
                collapsible = TRUE,
                collapsed = FALSE,
                options = list(background = "#FFFDEE"),
                solidHeader = TRUE,
                status = "purple",
                width = 12,
                reactable::reactableOutput(outputId = "DT")
              )
            )
          ),
          ### Import panel
          conditionalPanel(
            condition = 'input.module == "Import"',
            div(
              id = "importModule",
              tabsetPanel(
                tabPanel( # File (Default)
                  title = i18n_shiny$t("Files"),
                  uiOutput(outputId = "datamods_import_file"),
                  h4(HTML(paste0("Example Dataset from ", tags$a('datatoys', href = 'https://statgarten.github.io/datatoys/')))),
                  uiOutput(outputId = 'exampleDataset')
                ),
                tabPanel( # URL
                  title = i18n_shiny$t("URL"),
                  br(),
                  shinyWidgets::actionBttn( # example data load
                    inputId = "exampleURL",
                    label = i18n_shiny$t("Load Example data"),
                    style = "material-flat",
                    size = "sm",
                    block = TRUE,
                    color = "royal"
                  ),
                  uiOutput(outputId = "datamods_import_url")
                ),
                tabPanel( # Google Sheet
                  title = i18n_shiny$t("Google Sheet"),
                  uiOutput(outputId = "datamods_import_googlesheets")
                )
              )
            ),
            shinyjs::hidden(
              div(
                id = "importModuleActionButtons",
                actionButton(inputId = "showUpdateModule", label = i18n_shiny$t("Update Data")), ## Update
                actionButton(inputId = "showFilterModule", label = i18n_shiny$t("Filter Data")), ## Filter
                actionButton(inputId = "showTransformModule", label = i18n_shiny$t("Transform Data")), ## Transform
                actionButton(inputId = "showReorderModule", label = i18n_shiny$t("Reorder Data")), ## Reorder
                actionButton(inputId = "showExportModule", label = i18n_shiny$t("Export Data")) ## Export
              )
            )
          ),

          ### Vis panel
          conditionalPanel(
            condition = 'input.module == "Vis"',
            shinyjs::hidden(
              div(
                id = "visModule",
                uiOutput(outputId = "esquisse_ui")
              )
            )
          ),

          ### EDA panel
          conditionalPanel(
            condition = 'input.module == "EDA"',
            shinyjs::hidden(
              div(
                id = "edaModule",
                shinydashboardPlus::box(
                  title = "Dataset Description",
                  status = "purple",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 6, uiOutput(outputId = "dataDimension")),
                    column(width = 6, uiOutput(outputId = "missingData"))
                  )
                ),
                shinydashboardPlus::box(
                  title = "Correlation",
                  status = "purple",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput(outputId = "corplot")
                ),
                shinydashboardPlus::box(
                  title = "Variables",
                  status = "purple",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  width = 6,
                  reactableOutput(outputId = "reactOutput")
                ),
                shinydashboardPlus::box(
                  title = "Distribution",
                  status = "purple",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  mod_variableModule_ui("variableModule_1"),
                  fluidRow(
                    column(width = 4, plotOutput("distplot")),
                    column(width = 4, plotOutput("distplot2")),
                    column(width = 4, uiOutput("distBox"))
                  )
                )
              )
            )
          ),
          ## Stat Panel
          conditionalPanel(
            condition = 'input.module == "Stat"',
            shinyjs::hidden(
              div(
                id = "StatModule",
                selectInput(
                  inputId = "tableOneStrata",
                  label = "Group by",
                  choices = NULL,
                  selected = NULL
                ),
                actionButton(
                  inputId = "generateTable",
                  label = "generate Table"
                ),
                reactableOutput(outputId = "tableOne")
              )
            )
          ),
          ## ML Panel
          conditionalPanel(
            condition = 'input.module == "ML"',
            shinyjs::hidden(
              div(
                id = "MLModule",
                ## split
                shinydashboardPlus::box(
                  style = "height:50vh; overflow-y: scroll;",
                  title = "TrainTestSplit",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 12,
                  mod_ttSplitModule_ui(id = "ttSplitModule_1")
                  # ,
                  # footer = actionButton(
                  #   inputId = ("applyML"),
                  #   label = tagList(
                  #     phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                  #     i18n("Apply changes")
                  #   ),
                  #   width = "100%"
                  # )
                ),
                ## preprocess
                shinydashboardPlus::box(
                  style = "height:400px; overflow-y: scroll;",
                  title = "Preprocess",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 12,
                  mod_preprocessModule_ui("preprocessModule_1") # ,
                  # footer = actionButton(
                  #   inputId = ("applyML"),
                  #   label = tagList(
                  #     phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                  #     i18n("Apply changes")
                  #   ),
                  #   width = "100%"
                  # )
                ),
                shinydashboardPlus::box(
                  style = "height:400px;",
                  title = "Modeling",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 12,
                  mod_modelingModule_ui("modelingModule_1") # ,
                  # footer = actionButton(
                  #   inputId = ("applyML"),
                  #   label = tagList(
                  #     phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                  #     i18n("Apply changes")
                  #   ),
                  #   width = "100%"
                  # )
                )
              )
            ),
          ),

          ## Report Panel
          conditionalPanel(
            condition = 'input.module == "Report"',
            shinyjs::hidden(
              div(
                id = "ReportModule",
                radioButtons("format", "Document format", c("PDF", "HTML", "Word"), inline = TRUE),
                downloadButton(outputId = "downloadReport")
              )
            )
          )
        )
      ),
      controlbar = dashboardControlbar(disable = TRUE),
      footer = NULL
      # actionButton(
      #   inputId = "Outro",
      #   label = NULL,
      #   style = "margin: auto; width: 100%; background: #590696; color: #FFF",
      #   onclick = "window.open('https://github.com/statgarten', '_blank')",
      #   icon = icon("github", style = "font-size: 1.3em;")
      # )
      #  actionButton(
      #  inputId = "showGuide",
      #  label = "Guide",
      #  style = "margin: auto; width: 100%; background: #C70A80; color: #FFF",
      #  icon = icon("question", style = "font-size: 1.3em;")
      # )
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



## language

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kr.svg"
)
