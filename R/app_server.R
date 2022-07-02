#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom haven read_sas read_sav read_dta
#' @importFrom readxl read_xls read_xlsx
#' @importFrom readr read_rds
#' @importFrom reactable renderReactable
#' @importFrom shinydashboardPlus descriptionBlock
#' @importFrom GGally ggcorr
#' @noRd
app_server <- function(input, output, session) {
  src <- "www/statgarten.png"
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })

  inputData <- reactiveVal(NULL)

  # EDA Plot
  ggobj <- reactiveVal(NULL) # relation scatter chart
  distobj <- reactiveVal(NULL) # variable histogram
  distobj2 <- reactiveVal(NULL) # variable pie chart
  uiobj <- reactiveVal(NULL) # variable quantitle box
  columnTypes <- reactiveVal(NULL)

  observeEvent(input$fileInputID, {
    file <- input$fileInputID

    ext <- file$datapath |>
      tools::file_ext() |>
      tolower()

    req(file)

    shinyjs::hide(id = "desc")
    shinyjs::hide(id = "fileInputID")

    shinyjs::show(id = "showAll")

    shinyjs::show(id = "ImportBox")
    shinyjs::show(id = "EDABox")

    if (ext == "csv") { file$datapath |> read.csv() |> inputData() }
    if (ext == "tsv") { file$datapath |> read.csv(sep = "\t") |> inputData() }
    if (ext == "sas7bdat" || ext == "sas7bcat") { file$datapath |> haven::read_sas() |> inputData() }
    if (ext == "sav") { file$datapath |> haven::read_sav() |> inputData() }
    if (ext == "dta") { file$datapath |> haven::read_dta() |> inputData() }
    if (ext == "xls") { file$datapath |> readxl::read_xls() |> inputData() }
    if (ext == "xlsx") { file$datapath |> readxl::read_xlsx() |> inputData() }
    if (ext == "rds") { file$datapath |> readr::read_rds() |> inputData() }
    if (ext == "rda" || ext == "rdata") { file$datapath |> load() |> inputData() }

    # define column types
    tt <- inputData() %>%
      dplyr::summarise_all(class) %>%
      tidyr::gather(class)

    tt2 <- tt %>% pull(value)
      names(tt2) <- tt %>% pull(class)
    columnTypes(tt2)
    rm(tt, tt2)

    ## Main table

    output$DT <-
      inputData() |>
      getDT(all = TRUE, columnGroups = columnTypes()) |>
      reactable::renderReactable()

    ## EDA

    # getexc
    exc <- which(!columnTypes() %in% c('numeric'))
    if(length(exc) == 0) {exc <- NULL}

    obj <- board::brief(inputData(), exc = exc)
    obj$unif <- ifelse(obj$unif, "True", NA)
    obj$uniq <- ifelse(obj$uniq, "True", NA)

    EDAres <- data.frame(
      Name = obj$names,
      Cardinality = obj$cards,
      Zero = obj$zeros,
      Missing = obj$miss,
      isUniform = obj$unif,
      isUnique = obj$uniq
    )

    output$corplot <- renderPlot(ggcorr(obj$cors))

    output$dataDimension <- renderUI(
      descriptionBlock(
        header = paste0(obj$desc$nrow, " X ", obj$desc$ncol),
        numberIcon = icon("expand"),
        number = "Data Dimension",
        marginBottom = FALSE
      )
    )

    output$missingData <- renderUI(
      descriptionBlock(
        header = paste0(obj$desc$missingCellCount, "(", obj$desc$missingCellRatio, "%)"),
        numberIcon = icon("question"),
        number = "Missing Data",
        marginBottom = FALSE
      )
    )

    updateSelectInput(
      inputId = "variableSelect",
      label = "variable",
      choices = colnames(inputData()),
      selected = NULL
    )

    output$reactOutput <- renderReactable(
      reactable(
        EDAres
      )
    )

    output$corplot2 <- renderPlot(ggobj())

    output$distplot <- renderPlot(distobj())
    output$distplot2 <- renderPlot(distobj2())

    output$distBox <- renderUI(uiobj())
  })

  opened <- reactiveVal(NULL)

  observeEvent(input$ImportFunction, {
    opened(input$ImportFunction)
  })

  observeEvent(input$EDAFunction, {
    opened(input$EDAFunction)
  })


  ## Import

  mod_filterModule_server("filterModule_1", inputData, opened)

  mod_subsetModule_server("subsetModule_1", inputData, opened)

  mod_mutateModule_server("mutateModule_1", inputData, opened)

  mod_cleanModule_server("cleanModule_1", inputData, opened)

  mod_splitModule_server("splitModule_1", inputData, opened)

  mod_reshapeModule_server("reshapeModule_1", inputData, opened)

  mod_exportModule_server("exportModule_1", inputData)

  ## EDA

  mod_briefModule_server("briefModule_1", inputData, opened)

  mod_relationModule_server("relationModule_1", inputData, ggobj, opened)

  mod_variableModule_server("variableModule_1", inputData, opened, distobj, distobj2, uiobj)

  # Your application server logic

  observeEvent(input$showAll, {
    # only shows input data exists
    if (is.null(inputData())) {
      return()
    }

    if (input$showAll == TRUE) {
      output$DT <-
        inputData() |>
        getDT(all = TRUE, columnGroups = columnTypes()) |>
        reactable::renderReactable()
    }
    if (input$showAll == FALSE) {
      output$DT <-
        inputData() |>
        getDT(columnGroups = columnTypes()) |>
        reactable::renderReactable()
    }
  })
}
