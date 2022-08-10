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
    usei18n(i18n_shiny),
    golem_add_external_resources(),
    dashboardPage(
      skin = "black",
      header = dashboardHeader(
        title = i18n_shiny$t("Statgarten"),
        titleWidth = NULL,
        # title = tagList(
        #   span(class = "logo-lg", "statgarten"),
        #   img(src = "www/statgarten.png", style = "width :150%")
        # ),
        # titleWidth = 300,
        controlbarIcon = icon("gear", verify_fa = FALSE), # to hide error
        leftUi = tagList(
          div(
            id='moduleSelector',
            shinyWidgets::radioGroupButtons(
              inputId = "module",
              label = NULL,
              # status = 'warning',
              choices = c(
                "Import",
                "Vis",
                "EDA",
                "Stat",
                "ML",
                "Report"
              ),
              selected = "Import",
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", style = "color: #37E2D5"),
                # no = tags$i(class = "fa fa-circle-o", style = "color: gold") : empty
                no = tags$i(class = "fa fa-circle", style = "color: #FBCB0A")
              )
            )
          ),
          div(
            actionButton(
              inputId = "Outro",
              label = NULL,
              style = "margin: auto; width: 100%; background: #590696; color: #FFF",
              onclick = "window.open('https://github.com/statgarten', '_blank')",
              icon = icon("github", style = "font-size: 1.3em;")
            )
          ),
          div(
            actionButton(
              inputId = "showGuide",
              label = "Guide",
              style = "margin: auto; width: 100%; background: #C70A80; color: #FFF",
              icon = icon("question", style = "font-size: 1.3em;")
            )
          ),
          div(
            radioGroupButtons(
              inputId = "lang",
              label = NULL,
              choiceNames = lapply(seq_along(countries), function(i) tagList(tags$img(src = flags[i], width = 30, height = 20))),
              choiceValues = i18n_shiny$get_languages(),
              individual = TRUE
            )
          )
        )
      ),

      # SIDE MODULE
      sidebar = ,
      dashboardSidebar(
        disable = TRUE,
        minified = FALSE,
        width = 0
        # width = 300,
        # conditionalPanel(
        #   condition = 'input.module == "Import"',
        #   shinyjs::hidden(
        #     div(
        #       id = "ImportBox",
        #       style = "text-align:center;",
        #       shinyWidgets::pickerInput(
        #         inputId = "ImportFunction",
        #         label = "Functions",
        #         choices = c(
        #           "",
        #           # "Filter",
        #           # "Subset",
        #           # "Mutate",
        #           # "Clean",
        #           # "Split",
        #           "Reshape",
        #           "Export"
        #         ),
        #         choicesOpt = list(
        #           subtext = c(
        #             "",
        #             # "select data with criteria",
        #             # "delete column",
        #             # "mut",
        #             # "cle",
        #             "spl", "res", "exp"
        #           ),
        #           style = rep(c("color: black"), 8)
        #         ),
        #         options = list(), # style = "btn-info"
        #         selected = NULL
        #       ),
        #       conditionalPanel(
        #         condition = 'input.ImportFunction == "Filter"',
        #         mod_filterModule_ui("filterModule_1")
        #       ),
        #       conditionalPanel(
        #         condition = 'input.ImportFunction == "Subset"',
        #         mod_subsetModule_ui("subsetModule_1")
        #       ),
        #       conditionalPanel(
        #         condition = 'input.ImportFunction == "Mutate"',
        #         mod_mutateModule_ui("mutateModule_1"),
        #       ),
        #       conditionalPanel(
        #         condition = 'input.ImportFunction == "Clean"',
        #         mod_cleanModule_ui("cleanModule_1")
        #       ),
        #       # conditionalPanel(
        #       #   condition = 'input.ImportFunction == "Split"',
        #       #   mod_splitModule_ui("splitModule_1")
        #       # ),
        #       conditionalPanel(
        #         condition = 'input.ImportFunction == "Reshape"',
        #         mod_reshapeModule_ui("reshapeModule_1")
        #       )# ,
        #       # conditionalPanel(
        #       #   condition = 'input.ImportFunction == "Export"',
        #       #   mod_exportModule_ui("exportModule_1")
        #       # )
        #     )
        #   )
        # )

        # conditionalPanel(
        #   condition = 'input.module == "EDA"',
        #   shinyjs::hidden(
        #     div(
        #       id = "EDABox",
        #       style = "text-align:center;",
        #       shinyWidgets::pickerInput(
        #         inputId = "EDAFunction",
        #         label = "EDA Functions",
        #         choices = c("",
        #                     # "Brief",
        #                     #"Relation",
        #                     "Variable"),
        #         choicesOpt = list(
        #           subtext = c("",
        #                       # "bri",
        #                       # "rel",
        #                       "var"),
        #           style = rep(c("color: black"), 2
        #                       # 3
        #                       # 4
        #                       )
        #         ),
        #         options = list(
        #           style = "btn-info"
        #         ),
        #         selected = NULL
        #       ),
        #       # conditionalPanel(
        #       #   condition = 'input.EDAFunction == "Brief"',
        #       #   mod_briefModule_ui("briefModule_1")
        #       # ),
        #       # conditionalPanel(
        #       #   condition = 'input.EDAFunction == "Relation"',
        #       #   mod_relationModule_ui("relationModule_1")
        #       # ),
        #       # conditionalPanel(
        #       #   condition = 'input.EDAFunction == "Variable"',
        #       #   mod_variableModule_ui("variableModule_1")
        #       # )
        #     )
        #   )
        # ),
      ),
      body = dashboardBody(
        fluidPage(
          useShinyjs(),

          ## View

          shinyjs::hidden(
            div(
              id = "viewModule",
              # style = "margin-bottom : 1em; padding-right: 15px; padding-left: 15px;",
              # h3("Data View"),
              shinydashboardPlus::box(
                title = i18n_shiny$t("Uploaded Data"),
                collapsible = TRUE,
                collapsed = FALSE,
                options = list(
                  background = "#FFFDEE"
                ),
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
            # h3(
            #  id = "desc",
            #  "Statgarten supports Data Wrangling / Visualize / EDA and Export results"
            # ),
            div(
              id = "importModule",
              tabsetPanel(
                tabPanel( # File (Default)
                  "File",
                  datamods::import_file_ui(
                    id = "importModule_1",
                    preview_data = TRUE,
                    file_extensions = c(
                      ".csv", ".dta", ".fst", ".rda", ".rds",
                      ".rdata", ".sas7bcat", ".sas7bdat",
                      ".sav", ".tsv", ".txt", ".xls", ".xlsx"
                    )
                  )
                ),
                tabPanel( # URL
                  "URL",
                  # actionButton(
                  #   inputId = 'exampleURL',
                  #   label = 'load example data'
                  # ),
                  br(),
                  shinyWidgets::actionBttn(
                    inputId = "exampleURL",
                    label = "load example data",
                    style = "material-flat",
                    size = "sm",
                    block = TRUE,
                    color = "royal" # Purple - White
                    # 'default' # White - Blue
                    # "primary" # Blue - White
                  ),
                  uiOutput(
                    outputId = 'datamods_import_url'
                  )
                  # datamods::import_url_ui(
                  #   id = "importModule_2"
                  # )
                ),
                tabPanel( # Google Sheet
                  "Google Sheet",
                  datamods::import_googlesheets_ui(
                    id = "importModule_3"
                  )
                )
              )
            ),

            ## Update
            shinyjs::hidden(
              div(
                id = "updateModule",
                shinydashboardPlus::box(
                  style = "height:400px;",
                  title = "Select / Rename / Convert data",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  ui2(
                    id = "updateModule_1",
                    title = FALSE
                  ),
                  footer = actionButton(
                    inputId = "updateModule_1-validate",
                    label = tagList(
                      phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                      i18n("Apply changes")
                    ),
                    width = "100%"
                  )
                )
              )
            ),

            ## Filter

            shinyjs::hidden(
              div(
                id = "filterModule",
                shinydashboardPlus::box(
                  style = "height:400px;overflow-y: scroll;",
                  title = "Filter data",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  datamods::filter_data_ui(id = "filterModule_2", show_nrow = FALSE),
                  footer = actionButton(
                    inputId = "applyFilter",
                    label = tagList(
                      phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                      i18n("Apply changes")
                    ),
                    width = "100%"
                  )
                )
              )
            ),

            ## Transform

            shinyjs::hidden(
              div(
                id = "transformModule",
                shinydashboardPlus::box(
                  style = "height:400px;overflow-y: scroll;",
                  title = "Transform data",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  tabsetPanel(
                    id = "transformPanel",
                    tabPanel(
                      title = "Round",
                      icon = icon("scissors"),
                      mod_roundModule_ui("roundModule_1")
                    ),
                    tabPanel( # Log2 / Log / Log10
                      title = "Log",
                      icon = icon("ruler"),
                      mod_logModule_ui("logModule_1")
                    ),
                    tabPanel(
                      title = "Replace",
                      icon = icon("font"),
                      mod_replaceModule_ui("replaceModule_1")
                    ),
                    tabPanel(
                      title = "Etc",
                      icon = icon("minus"),
                      mod_etcModlue_ui("etcModule_1")
                    ),
                    tabPanel(
                      title = "Binarize",
                      icon = icon("slash"),
                      mod_binarizeModule_ui("binModule_1")
                    )
                  ),
                  footer = actionButton(
                    inputId = ("applyRound"),
                    label = tagList(
                      phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                      i18n("Apply changes")
                    ),
                    width = "100%"
                  )
                )
              )
            ),

            ## Split

            shinyjs::hidden(
              div(
                id = "splitModule",
                shinydashboardPlus::box(
                  style = "height:400px; overflow-y: scroll;",
                  title = "Split data",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  mod_splitModule_ui(id = "splitModule_1"),
                  footer = actionButton(
                    inputId = ("applySplit"),
                    label = tagList(
                      phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                      i18n("Apply changes")
                    ),
                    width = "100%"
                  )
                )
              )
            ),

            ## Reorder

            shinyjs::hidden(
              div(
                id = "reorderModule",
                shinydashboardPlus::box(
                  style = "height:400px; overflow-y: scroll;",
                  title = "Reorder Column",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  mod_reorderModule_ui(id = "reorderModule_1"),
                  footer = actionButton(
                    inputId = ("applyReorder"),
                    label = tagList(
                      phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                      i18n("Apply changes")
                    ),
                    width = "100%"
                  )
                )
              )
            ),
            shinyjs::hidden(
              div(
                id = "exportModule",
                shinydashboardPlus::box(
                  style = "height:400px; overflow-y: scroll;",
                  title = "Export Data",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 6,
                  mod_exportModule_ui(id = "exportModule_1") # ,
                  # footer = actionButton(
                  #   inputId = ("applyExport"),
                  #   label = tagList(
                  #     phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                  #     i18n("Export Data")
                  #   ),
                  #   width = "100%"
                  # )
                )
              )
            )
          ),


          ### Vis panel
          conditionalPanel(
            condition = 'input.module == "Vis"',
            shinyjs::hidden(
              div(
                id = "visModule",
                shinydashboardPlus::box(
                  title = "Drag column to visualize",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "purple",
                  width = 12,
                  conditionalPanel(
                    condition = 'input.module == "Vis"',
                    shinyjs::hidden(
                      div(
                        id = "VisBox",
                        style = "text-align:center;",
                        shinyWidgets::checkboxGroupButtons(
                          inputId = "aes",
                          label = "Graph options",
                          choices = c(
                            "fill", "color", "size", "shape", "facet", "facet_row", "facet_col"
                          ),
                          selected = c("fill", "color", "size", "facet"),
                          justified = TRUE,
                          checkIcon = list(
                            yes = icon("ok", lib = "glyphicon")
                          )
                        )
                      )
                    )
                  ),
                  hr(),
                  esquisse_ui(
                    id = "visModule_e",
                    header = FALSE,
                    controls = c("labs", "parameters", "appearance", "code")
                  )
                )
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
                    column(
                      width = 6,
                      uiOutput("dataDimension")
                    ),
                    column(
                      width = 6,
                      uiOutput("missingData")
                    )
                    # zero
                  )
                ),
                shinydashboardPlus::box(
                  title = "Correlation",
                  status = "purple",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("corplot")
                ),
                shinydashboardPlus::box(
                  title = "Variables",
                  status = "purple",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  width = 6,
                  reactableOutput("reactOutput")
                ),
                shinydashboardPlus::box(
                  title = "Distribution",
                  status = "purple",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  # conditionalPanel(
                  #  condition = 'input.EDAFunction == "Variable"',
                  mod_variableModule_ui("variableModule_1"),
                  # ),
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
                  inputId = 'tableOneStrata',
                  label = 'Group by',
                  choices = NULL,
                  selected = NULL
                ),
                actionButton(
                  inputId = 'generateTable',
                  label = 'generate Table'
                ),
                reactableOutput(outputId = 'tableOne')
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

                # TEMPORARY NOT USE
                  # shinydashboardPlus::box(
                  #   style = "height:400px; overflow-y: scroll;",
                  #   title = "ML Report",
                  #   collapsible = TRUE,
                  #   collapsed = FALSE,
                  #   solidHeader = TRUE,
                  #   status = "purple",
                  #   width = 12 # ,
                  #
                  # mod_mlReportModule_ui("mlReportModule_1") #
                  # footer = actionButton(
                  #   inputId = ("applyML"),
                  #   label = tagList(
                  #     phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
                  #     i18n("Apply changes")
                  #   ),
                  #   width = "100%"
                  # )
                  #)

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
                # h1("Not Implemented")
              )
            )
          )
        )
      ),
      controlbar = dashboardControlbar(disable = TRUE) # ,
      # footer = dashboardFooter(
      #  left = NULL,
      #  right =
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

#' @import datamods
ui2 <- function(id, title = TRUE) {
  ns <- NS(id)
  if (isTRUE(title)) {
    title <- tags$h4(i18n("Update & select variables"),
      class = "datamods-title"
    )
  }
  tags$div(
    class = "datamods-update",
    html_dependency_pretty(),
    tags$div(
      style = "min-height: 25px;",
      reactable::reactableOutput(
        outputId = ns("table")
      )
    ),
    tags$br()
  )
}


##
countries <- c("en", "kr")

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kr.svg"
)
