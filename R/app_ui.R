#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny dplyr tidyr
#' @import datamods
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @import esquisse
#' @import shiny.i18n
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box dashboardHeader dashboardSidebar dashboardPage dashboardFooter dashboardControlbar descriptionBlock
#' @importFrom reactable reactableOutput
#' @importFrom board mod_distributionModule_ui
#' @importFrom colorpen mod_mapVisModule_ui
#' @importFrom colorpen mod_pairModule_ui
#' @importFrom soroban mod_pcaModule_ui
#' @importFrom soroban mod_treeModule_ui
#' @importFrom soroban mod_groupStatModule_ui
#' @importFrom soroban mod_mlrModule_ui
#' @importFrom soroban mod_kmsModule_ui
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
        title = HTML(
          '<h3 style="margin-top: 10px; color: #FFF;">Statgarten</h3>'
        ),
        titleWidth = NULL,
        controlbarIcon = icon("gear", verify_fa = FALSE), # to hide error
        leftUi = tagList(
          #### LANGUAGES
          radioGroupButtons(
            inputId = "lang",
            label = NULL,
            choiceNames = lapply(seq_along(c("en", "kr")), function(i) tagList(tags$img(src = flags[i], width = 30, height = 20))),
            choiceValues = i18n_shiny$get_languages(),
            selected = NULL,
            individual = TRUE
          ),
          actionButton("defaultGuide", label = "Quick Start", icon = icon("question")),
          uiOutput("guideButton") # Guide Button
        )
      ),
      # Sidebar
      sidebar = dashboardSidebar(disable = TRUE, minified = FALSE, width = 0),
      body = dashboardBody(
        useShinyjs(),
        fluidPage(
          ## Module Selector
          # used to enable after file upload
          uiOutput("moduleSelector"),
          # Data View
          shinyjs::hidden(
            fluidRow(
              id = "viewModule",
              shinydashboardPlus::box(
                title = i18n_shiny$t("Uploaded Data"),
                collapsible = TRUE,
                collapsed = FALSE,
                options = list(background = "#FFFDEE"),
                icon = icon("table"),
                status = "navy",
                width = 12,
                reactable::reactableOutput(outputId = "DT")
              ),
              shinydashboardPlus::box(
                title = "Data wrangling",
                width = 12,
                status = "navy",
                icon = icon("scissors"),
                actionButton(inputId = "showUpdateModule", label = i18n_shiny$t("Update Data")),
                actionButton(inputId = "showFilterModule", label = i18n_shiny$t("Filter Data")),
                actionButton(inputId = "showTransformModule", label = i18n_shiny$t("Transform Data")),
                actionButton(inputId = "showReorderModule", label = i18n_shiny$t("Reorder Data")), actionButton(inputId = "showExportModule", label = i18n_shiny$t("Export Data"))
              )
            )
          ),
          ### Import panel (temporary, remove after data upload)
          fluidRow(
            id = "importModule",
            tabsetPanel(
              type = "pills",
              id = "importTabset",
              tabPanel( # File (Default)
                title = i18n_shiny$t("Files"),
                shinycssloaders::withSpinner(
                  uiOutput(outputId = "datamods_import_file"),
                )
              ),
              tabPanel( # URL
                title = i18n_shiny$t("URL"),
                br(),
                # Examples
                fluidRow(
                  column(
                    width = 4,
                    shinyWidgets::actionBttn( # example data load
                      inputId = "exampleURL",
                      label = i18n_shiny$t("Load Example data"),
                      style = "bordered",
                      size = "sm",
                      block = TRUE,
                      color = "royal"
                    )
                  ),
                  column(
                    width = 4,
                    shinyWidgets::actionBttn( # example data load
                      inputId = "exampleR",
                      label = i18n_shiny$t("Load Boston: Regression"),
                      style = "bordered",
                      size = "sm",
                      block = TRUE,
                      color = "royal"
                    )
                  ),
                  column(
                    width = 4,
                    shinyWidgets::actionBttn( # example data load
                      inputId = "exampleC",
                      label = i18n_shiny$t("Load Boston: Classification"),
                      style = "bordered",
                      size = "sm",
                      block = TRUE,
                      color = "royal"
                    )
                  )
                ),
                br(),
                uiOutput(outputId = "datamods_import_url")
              ),
              tabPanel( # Google Sheet
                title = i18n_shiny$t("Google Sheet"),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    shinyWidgets::actionBttn( # example data load
                      inputId = "exampleSheet",
                      label = i18n_shiny$t("Load Example Sheet"),
                      style = "bordered",
                      size = "sm",
                      block = TRUE,
                      color = "success"
                    )
                  )
                ),
                br(),
                uiOutput(outputId = "datamods_import_googlesheets")
              ),
              tabPanel( # datatoys
                id = "tabPanelDatatoys",
                title = "Datatoys",
                h4(HTML(paste0(
                  i18n_shiny$t("Example Dataset from"), " ",
                  tags$a("datatoys", href = "https://statgarten.github.io/datatoys/", target = "_blank")
                ))),
                shinycssloaders::withSpinner(
                  uiOutput(outputId = "exampleDataset")
                )
              )
            )
          ),

          ### EDA panel
          conditionalPanel(
            condition = 'input.module == "EDA"',
            style = "margin-left: -15px; margin-right: -15px;",
            shinydashboardPlus::box(
              title = "EDA Panel",
              width = 12,
              status = "orange",
              div(
                tabsetPanel(
                  type = "pills",
                  id = "edaTabset",
                  tabPanel(
                    title = i18n_shiny$t("Dataset Description"),
                    column(
                      width = 8,
                      h4("Data Structure"),
                      verbatimTextOutput(outputId = "dataStructure")
                    ),
                    column(
                      width = 4,
                      fluidRow(
                        column(
                          width = 6,
                          uiOutput(outputId = "dataDimension")
                        ),
                        column(
                          width = 6,
                          uiOutput(outputId = "missingData")
                        )
                      ),
                      hr(),
                      fluidRow(
                        style = "margin-top: 1em;",
                        column(
                          width = 6,
                          selectInput( # Options
                            inputId = "format",
                            label = i18n_shiny$t("Document format"),
                            choices = c("PDF", "HTML", "Word", "Dashboard", "Paper"),
                            selected = "PDF"
                          )
                        ),
                        column(
                          width = 6,
                          downloadButton(
                            outputId = "downloadReport",
                            style = "margin-top:25px; width: 100%"
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = i18n_shiny$t("Correlation"),
                    plotOutput(outputId = "corplot")
                  ),
                  tabPanel(
                    title = i18n_shiny$t("Variables"),
                    reactableOutput(outputId = "reactOutput")
                  ),
                  tabPanel(
                    title = i18n_shiny$t("Distribution"),
                    fluidPage(
                      mod_distributionModule_ui("distModule_1")
                    )
                  )
                )
              )
            )
          ),

          ### Vis panel
          conditionalPanel(
            condition = 'input.module == "Vis"',
            style = "margin-left: -15px; margin-right: -15px;",
            shinydashboardPlus::box(
              title = "Vis Panel",
              width = 12,
              status = "orange",
              div(
                tabsetPanel(
                  type = "pills",
                  id = "visTabset",
                  tabPanel(
                    title = "General",
                    shinyWidgets::checkboxGroupButtons(
                      inputId = "aes",
                      label = i18n_shiny$t("Aesthetic options"),
                      choices = c("fill", "color", "size", "shape", "facet", "facet_row", "facet_col"),
                      selected = c("fill", "color", "size", "facet"),
                      justified = TRUE,
                      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                    ),
                    uiOutput(outputId = "esquisse_ui2")
                  ),
                  tabPanel(
                    title = "Map",
                    mod_mapVisModule_ui("mapVisModule_1", i18n = i18n_shiny)
                  ),
                  tabPanel(
                    title = "Pair",
                    box_height = "4em",
                    mod_pairModule_ui("pairModule_1")
                  )
                )
              )
            )
          ),
          ## Stat Panel
          conditionalPanel(
            condition = 'input.module == "Stat"',
            fluidRow(
              tabsetPanel(
                type = "pills",
                id = "statTabset",
                tabPanel(
                  title = "Table 1", # Not translate
                  fluidRow(
                    column( # Result Area
                      width = 9,
                      reactableOutput(outputId = "tableOne")
                    ),
                    column(
                      width = 3,
                      selectInput( # Options
                        inputId = "tableOneStrata",
                        label = i18n_shiny$t("Group by"),
                        choices = NULL,
                        selected = NULL
                      ),
                      actionButton( # Main action
                        inputId = "generateTable",
                        label = i18n_shiny$t("generate Table"),
                        style = "font-weight: bold;background: #3EC70B;color: white; width: 100%"
                      )
                    )
                  )
                ),
                tabPanel(
                  title = i18n_shiny$t("PCA"),
                  mod_pcaModule_ui("pcaModule_1")
                ),
                tabPanel(
                  title = i18n_shiny$t("Decision tree"),
                  mod_treeModule_ui("treeModule_1")
                ),
                tabPanel(
                  title = i18n_shiny$t("Linear regression"),
                  box_height = "5em",
                  mod_mlrModule_ui("mlrModule_1")
                ),
                tabPanel(
                  title = i18n_shiny$t("K-means cluster"),
                  mod_kmsModule_ui("kmsModule_1")
                ),
                tabPanel(
                  title = i18n_shiny$t("Group metrics"),
                  mod_groupStatModule_ui("groupStatModule_1")
                )
              )
            )
          ),
          ## ML Panel
          conditionalPanel(
            condition = 'input.module == "ML"',
            fluidRow(
              tabsetPanel(
                type = "pills",
                id = "mlTabset",
                tabPanel(
                  title = "Data Setup",
                  mod_ttSplitModule_ui(id = "ttSplitModule_1")
                ),
                tabPanel(
                  title = "Modeling",
                  mod_modelingModule_ui("modelingModule_1")
                )
              )
            )
          )
        )
      ),
      controlbar = dashboardControlbar(disable = TRUE),
      footer = NULL
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
      app_title = "statgarten" # chrome title
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
