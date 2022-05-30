#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny dplyr tidyr
#' @importFrom shinyjs hidden useShinyjs
#' @importFrom DT DTOutput
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box dashboardSidebar dashboardPage dashboardFooter dashboardControlbar
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "HeaderTitle", # chrome tab name
        controlbarIcon = icon("gear", verify_fa = FALSE),
        leftUi = tagList(
          div(
            selectInput(
              inputId = "module",
              label = NULL,
              choices = c("Import", "Vis"),
              selected = "Import",
              width = "10em"
            ),
            style = "margin-bottom: -11.5px; text-align: center; font-weight: bold;"
          )
        )
      ),

      # SIDE MODULE
      sidebar = dashboardSidebar(
        minified = FALSE,
        htmlOutput("Logo", style = "text-align: center; margin-bottom:3em; margin-top:3em;"),
        conditionalPanel(
          condition = 'input.module == "Import"',
          shinyjs::hidden(
            div(
              id = "SideBox",
              boxUI(
                title = "Filter",
                mod_filterModule_ui("filterModule_1")
                # filterUI("filterModule")
              ),
              boxUI(
                title = "Subset",
                subsetUI("subsetModule")
              ),
              boxUI(
                title = "Mutate",
                mutateUI("mutateModule")
              ),
              boxUI(
                title = "Clean",
                cleanUI("cleanModule")
              ),
              boxUI(
                title = "Split",
                splitUI("splitModule")
              ),
              boxUI(
                title = "!Reshape (not)",
                p("Not Implemented")
              ),
              boxUI(
                title = "Export",
                exportUI("exportModule")
              )
            )
          )
        ),
        conditionalPanel(
          condition = 'input.module == "Vis"',
          p("Not Implemented")
        )
        # OUTRO
      ),
      body = dashboardBody(
        fluidPage(
          useShinyjs(),
          p(
            id = "desc",
            "Description Text will be here"
          ),
          fileInput(
            inputId = "fileInputID",
            label = "FileInput Label",
            accept = c(".csv", ".tsv", ".sas7bdat", ".sas7bcat", ".sav", ".dta", ".xls", ".xlsx", ".rda", ".rds", ".rdata"),
            # csv: Column separated
            # tsv, tab: Tab separated
            # sas7bdat: SAS file
            # sav: SPSS file
            # dta: Stata file
            # xls, xlsx: Excel file
            # rda, rds, rdata: Robject file
            buttonLabel = "Button Label",
            placeholder = "Place Holder"
          ),
          DT::DTOutput(
            outputId = "DT"
          )
        )
      ),
      controlbar = dashboardControlbar(
        p("Control Bar")
      ),
      footer = dashboardFooter(
        left = "Left Content",
        right = actionButton(
          inputId = "Outro",
          label = "Github / Manual",
          style = "margin: auto; width: 100%",
          onclick = "window.open('https://github.com/statgarten', '_blank')",
          icon = icon("github")
        )
      ),
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
      app_title = "door2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
