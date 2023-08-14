#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import datamods
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @import esquisse
#' @import shiny.i18n
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box dashboardHeader dashboardSidebar dashboardPage dashboardFooter dashboardControlbar descriptionBlock dropdownBlock
#' @importFrom reactable reactableOutput
#' @importFrom board mod_distributionModule_ui
#' @importFrom colorpen mod_mapVisModule_ui mod_pairModule_ui mod_mosaicModule_ui
#' @importFrom soroban mod_pcaModule_ui mod_treeModule_ui mod_groupStatModule_ui mod_mlrModule_ui mod_kmsModule_ui
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
        ),
        dropdownBlock(
          id = 'external',icon = icon('arrow-up-right-from-square'),
          title = 'More Application',
          badgeStatus = NULL,
          actionButton(
            inputId = 'jsmodule',
            label = 'jsmodule',
            icon = icon('stethoscope'),
            width = '100%',
            style = '
            background: #F59B38;
            color: #fff;
            border: 0;
            border-radius: 0;
            width : 100%;',
            onclick ="window.open('https://openstat.ai/app/basic/', '_blank')"
          )
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
                fluidPage(
                  column(
                    width = 8,
                    style = "border-right: dotted 1px black",
                    uiOutput(outputId = "datamods_import_file")
                  ),
                  column(
                    width = 4,
                    h4("Example Files"),
                    actionButton(
                      inputId = "exampleA",
                      class = "exampleButton",
                      label = i18n_shiny$t("Titanic (Kaggle)"),
                      onclick = "window.open('https://www.kaggle.com/competitions/titanic/data', '_blank')"
                    ),
                    actionButton(
                      inputId = "exampleB",
                      class = "exampleButton",
                      label = i18n_shiny$t("Iris (UCI)"),
                      onclick = "window.open('https://archive.ics.uci.edu/ml/datasets/iris', '_blank')"
                    ),
                    actionButton(
                      inputId = "exampleC",
                      class = "exampleButton",
                      label = i18n_shiny$t("Palmerpenguins (LTER) "),
                      onclick = "window.open('https://github.com/allisonhorst/palmerpenguins/', '_blank')"
                    ),
                    actionButton(
                      inputId = "exampleD",
                      class = "exampleButton",
                      label = i18n_shiny$t("Healthcare (NHIS Korea)"),
                      onclick = "window.open('https://github.com/jinseob2kim/R-skku-biohrs/tree/main/data, '_blank')"
                    )
                  )
                )
              ),
              tabPanel( # URL
                title = i18n_shiny$t("URL"),
                fluidPage(
                  column(
                    width = 8,
                    style = "border-right: dotted 1px black",
                    uiOutput(outputId = "datamods_import_url")
                  ),
                  column(
                    width = 4,
                    h4("Example URL"),
                    actionButton(
                      inputId = "exampleURL",
                      class = "exampleButton",
                      label = i18n_shiny$t("Healthcare (NHIS Korea)")
                    ),
                    actionButton(
                      inputId = "exampleR",
                      class = "exampleButton",
                      label = i18n_shiny$t("Boston House Price: Regression")
                    ),
                    actionButton(
                      inputId = "exampleC",
                      class = "exampleButton",
                      label = i18n_shiny$t("Boston House Price: Classification"),
                    )
                  )
                )
              ),
              tabPanel( # Google Sheet
                title = i18n_shiny$t("Google Sheet"),
                fluidPage(
                  column(
                    width = 8,
                    style = "border-right: dotted 1px black",
                    uiOutput(outputId = "datamods_import_googlesheets")
                  ),
                  column(
                    width = 4,
                    h4("Example Sheet"),
                    actionButton(
                      inputId = "exampleSheet",
                      class = "exampleButton",
                      label = i18n_shiny$t("Online Retail Dataset")
                    )
                  )
                )
              ),
              tabPanel( # datatoys
                title = "Datatoys",
                fluidPage(
                  column(
                    h4("Import Public data"),
                    width = 8,
                    style = "border-right: dotted 1px black",
                    uiOutput(outputId = "exampleDataset")
                  ),
                  column(
                    width = 4,
                    h4("More Info"),
                    actionButton(
                      inputId = "datatoysActionButton",
                      class = "exampleButton",
                      label = "Datatoys",
                      onclick = "window.open('https://www.statgarten.com/datatoys/, '_blank')"
                    ),
                    actionButton(
                      inputId = "datagokrActionButton",
                      class = "exampleButton",
                      label = "Data.go.kr",
                      onclick = "window.open('https://www.data.go.kr/, '_blank')"
                    )
                  )
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
                            choices = c("PDF", "HTML", "Word", "Dashboard", "PPT", "Paper"),
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
                      ),
                      # conditionalPanel(
                      #  "input.format == 'Paper' ",
                      fluidRow(
                        style = "margin-top: 1em;",
                        column(
                          width = 4,
                          textInput(
                            inputId = "report.name",
                            label = "name",
                            placeholder = "John Doe"
                          )
                        ),
                        column(
                          width = 4,
                          textInput(
                            inputId = "report.org",
                            label = "organization",
                            placeholder = "Statgarten"
                          )
                        ),
                        column(
                          width = 4,
                          textInput(
                            inputId = "report.location",
                            label = "location",
                            placeholder = "Seoul, Korea"
                          )
                        )
                      ),
                      fluidRow(
                        style = "margin-top: 1em",
                        column(
                          width = 6,
                          textInput(
                            inputId = "report.email",
                            label = "email",
                            placeholder = "JohnDoe@mail.com"
                          )
                        ),
                        column(
                          width = 6,
                          textInput(
                            inputId = "report.team",
                            label = "team",
                            placeholder = "Part of Data"
                          )
                        )
                      )
                      # )
                    )
                  ),
                  tabPanel(
                    title = i18n_shiny$t("Correlation"),
                    column(
                      width = 8,
                      style = "border-right: dotted 1px black",
                      h4("Correlation between data"),
                      plotOutput(outputId = "corplot"),
                      br()
                    ),
                    column(
                      width = 4,
                      h4("Options"),
                      sliderInput(
                        inputId = "corSize",
                        label = "Height of plot",
                        min = 400,
                        max = 1000, step = 50, value = 400,
                        ticks = FALSE,
                        width = "100%"
                      )
                    )
                  ),
                  tabPanel(
                    title = i18n_shiny$t("Variables"),
                    br(),
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
                    mod_pairModule_ui("pairModule_1")
                  ),
                  tabPanel(
                    title = "Mosaic",
                    mod_mosaicModule_ui("mosaicModule_1")
                  )
                )
              )
            )
          ),
          ## Stat Panel
          conditionalPanel(
            condition = 'input.module == "Stat"',
            style = "margin-left: -15px; margin-right: -15px;",
            shinydashboardPlus::box(
              title = "Stat Panel",
              width = 12,
              status = "orange",
              tabsetPanel(
                type = "pills",
                id = "statTabset",
                tabPanel(
                  title = "Table 1", # Not translate
                  fluidRow(
                    column( # Result Area
                      width = 8,
                      style = "border-right: dotted 1px black",
                      h4("Table 1"),
                      reactableOutput(outputId = "tableOne")
                    ),
                    column(
                      width = 4,
                      br(),
                      h4(
                        "❓ Table 1 presents descriptive statistics of baseline characteristics of the data stratified by specific variable"
                      ),
                      h4(
                        "only Factor column will be available as strata"
                      ),
                      selectInput( # Options
                        inputId = "tableOneStrata",
                        label = i18n_shiny$t("Strata"),
                        choices = NULL,
                        selected = NULL
                      ),
                      actionButton( # Main action
                        inputId = "generateTable",
                        label = i18n_shiny$t("generate Table"),
                        style = "font-weight: bold; width: 100%"
                      )
                    )
                  )
                ),
                tabPanel(
                  title = i18n_shiny$t("PCA"),
                  mod_pcaModule_ui("pcaModule_1")
                ),
                tabPanel(
                  title = i18n_shiny$t("Regression tree"),
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
            style = "margin-left: -15px; margin-right: -15px;",
            shinydashboardPlus::box(
              title = "ML Panel",
              width = 12,
              status = "orange",
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
