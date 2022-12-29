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
          paste0(
            '<a href="https://github.com/statgarten/", target = "_blank">',
            "Statgarten",
            # i18n_shiny$t("Statgarten"),
            "</a>"
          )
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
              ),
              shinydashboardPlus::box(
                title = "Data wrangling",
                width = 12,
                status = "maroon",
                icon = icon("scissors"),
                actionButton(inputId = "showUpdateModule", label = i18n_shiny$t("Update Data")), ## Update
                actionButton(inputId = "showFilterModule", label = i18n_shiny$t("Filter Data")), ## Filter
                actionButton(inputId = "showTransformModule", label = i18n_shiny$t("Transform Data")), ## Transform
                actionButton(inputId = "showReorderModule", label = i18n_shiny$t("Reorder Data")), ## Reorder
                actionButton(inputId = "showExportModule", label = i18n_shiny$t("Export Data")) ## Export
              )
            )
          ),
          ### Import panel (temporary, remove after data upload)
          div(
            id = "importModule",
            tabsetPanel(
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
                      style = "material-flat",
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
                      style = "material-flat",
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
                      style = "material-flat",
                      size = "sm",
                      block = TRUE,
                      color = "royal"
                    )
                  )
                ),
                uiOutput(outputId = "datamods_import_url")
              ),
              tabPanel( # Google Sheet
                title = i18n_shiny$t("Google Sheet"),
                uiOutput(outputId = "datamods_import_googlesheets")
              ),
              tabPanel( # datatoys
                id = "tabPanelDatatoys",
                title = "Datatoys",
                h4(HTML(paste0(
                  i18n_shiny$t("Example Dataset from"), " ",
                  tags$a("datatoys", href = "https://statgarten.github.io/datatoys/")
                ))),
                shinycssloaders::withSpinner(
                  uiOutput(outputId = "exampleDataset")
                )
              )
            )
          ),

          ### Vis panel
          conditionalPanel(
            condition = 'input.module == "Vis"',
            verticalTabsetPanel(
              contentWidth = 11,
              color = "#37E2D5", # SKY
              # #37E2D5 SKY
              # #FBCB0A Yellow
              # #C70A80 Red
              verticalTabPanel(
                title = "General",
                box_height = "4em",
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
              verticalTabPanel(
                title = "Map",
                box_height = "4em",
                mod_mapVisModule_ui("mapVisModule_1", i18n = i18n_shiny)
              )
            )
          ),

          ### EDA panel
          conditionalPanel(
            condition = 'input.module == "EDA"',
            verticalTabsetPanel(
              contentWidth = 11,
              color = "#37E2D5",
              # #37E2D5 SKY
              # #FBCB0A Yellow
              # #C70A80 Red
              verticalTabPanel(
                title = i18n_shiny$t("Dataset Description"),
                box_height = "5em",
                fluidRow(
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
                        uiOutput(outputId = "dataDimension"),
                      ),
                      column(
                        width = 6,
                        uiOutput(outputId = "missingData"),
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
                          style = "font-weight: bold;background: #3EC70B;color: white; width: 100%; margin-top:25px"
                        )
                      )
                    )
                  )
                )
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Correlation"),
                box_height = "4em",
                plotOutput(outputId = "corplot")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Variables"),
                box_height = "4em",
                reactableOutput(outputId = "reactOutput")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Distribution"),
                box_height = "4em",
                mod_distributionModule_ui("distModule_1")
              )
            )
          ),
          ## Stat Panel
          conditionalPanel(
            condition = 'input.module == "Stat"',
            verticalTabsetPanel(
              contentWidth = 11,
              color = "#3742fa", # Blue
              # #37E2D5 SKY
              # #FBCB0A Yellow
              # #C70A80 Red
              verticalTabPanel(
                title = "Table 1", # Not translate
                box_height = "4em",
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
              verticalTabPanel(
                title = i18n_shiny$t("PCA"),
                box_height = "4em",
                mod_pcaModule_ui("pcaModule_1")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Decision tree"),
                box_height = "5em",
                mod_treeModule_ui("treeModule_1")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Linear regression"),
                box_height = "5em",
                mod_mlrModule_ui("mlrModule_1")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("K-means cluster"),
                box_height = "5em",
                mod_kmsModule_ui("kmsModule_1")
              ),
              verticalTabPanel(
                title = i18n_shiny$t("Group metrics"),
                box_height = "5em",
                mod_groupStatModule_ui("groupStatModule_1")
              )
            )
          ),
          ## ML Panel
          conditionalPanel(
            condition = 'input.module == "ML"',
            verticalTabsetPanel(
              contentWidth = 11,
              color = "#37E2D5", # SKY
              # #37E2D5 SKY
              # #FBCB0A Yellow
              # #C70A80 Red
              verticalTabPanel(
                title = "Data Setup",
                box_height = "5em",
                mod_ttSplitModule_ui(id = "ttSplitModule_1")
              ),
              verticalTabPanel(
                title = "Modeling",
                box_height = "4em",
                mod_modelingModule_ui("modelingModule_1")
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
