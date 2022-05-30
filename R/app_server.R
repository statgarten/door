#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom haven read_sas read_sav read_dta
#' @importFrom readxl read_xls read_xlsx
#' @importFrom readr read_rds
#' @noRd
app_server <- function(input, output, session) {
  src <- "https://github.com/rstudio/shiny/blob/main/man/figures/logo.png?raw=true"
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })

  inputData <- reactiveVal(NULL)

  observeEvent(input$fileInputID, {
    file <- input$fileInputID

    ext <- file$datapath |>
      tools::file_ext() |>
      tolower()

    req(file)

    # validate(need(ext == "csv", "Please upload a csv File"))
    # already filtered in fileinput @accepts

    shinyjs::hide(id = "desc", anim = TRUE, animType = "slide")
    shinyjs::hide(id = "fileInputID", anim = TRUE, animType = "fade")

    shinyjs::show(id = "LoadButton", anim = TRUE, animType = "slide")
    shinyjs::show(id = "LoadTest", anim = TRUE, animType = "fade")

    shinyjs::show(id = "SideBox")

    if (ext == "csv") {
      file$datapath |>
        read.csv() |>
        inputData()
    }
    if (ext == "tsv") {
      file$datapath |>
        read.csv(sep = "\t")
      inputData()
    }
    if (ext == "sas7bdat" || ext == "sas7bcat") {
      file$datapath |>
        haven::read_sas() |>
        inputData()
    }
    if (ext == "sav") {
      file$datapath |>
        haven::read_sav() |>
        inputData()
    }
    if (ext == "dta") {
      file$datapath |>
        haven::read_dta() |>
        inputData()
    }
    if (ext == "xls") {
      file$datapath |>
        readxl::read_xls() |>
        inputData()
    }
    if (ext == "xlsx") {
      file$datapath |>
        readxl::read_xlsx() |>
        inputData()
    }
    if (ext == "rds") {
      file$datapath |>
        readr::read_rds() |>
        inputData()
    }
    if (ext == "rda" || ext == "rdata") {
      file$datapath |>
        load() |>
        inputData()
    }

    output$DT <- inputData() |>
      getDT() |>
      DT::renderDT()
  })

  mod_filterModule_server("filterModule_1", inputData)
  # filterServer(id = "filterModule", inputData)

  subsetServer(id = "subsetModule", inputData)

  mutateServer(id = "mutateModule", inputData)

  cleanServer(id = "cleanModule", inputData)

  splitServer(id = "splitModule", inputData)

  exportServer(id = "exportModule", inputData)
  # Your application server logic
}
