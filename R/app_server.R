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
#' @import datamods
#' @noRd
app_server <- function(input, output, session) {
  src <- "www/statgarten.png"
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })


  # import
  data_rv <- reactiveValues(data = NULL)
  inputData <- reactiveVal(NULL)
  columnTypes <- reactiveVal(NULL)

  # EDA Plot
  ggobj <- reactiveVal(NULL) # relation scatter chart
  distobj <- reactiveVal(NULL) # variable histogram
  distobj2 <- reactiveVal(NULL) # variable pie chart
  uiobj <- reactiveVal(NULL) # variable quantitle box

  output$corplot2 <- renderPlot(ggobj())
  output$distplot <- renderPlot(distobj())
  output$distplot2 <- renderPlot(distobj2())
  output$distBox <- renderUI(uiobj())


  # Vis
  plotlyobj <- reactiveVal(NULL)
  output$plot <- renderPlotly(plotlyobj())

  from_file <- import_file_server(
    id = "importModule_1",
    read_fns = list(
      tsv = function(file){ read.csv(file$datapath, sep = '\t') },
      sas7bcat = function(file){ haven::read_sas(file$datapath) },
      dta = function(file){ haven::read_dta(file$datapath) },
      rda = function(file){ load(file$datapath) },
      rdata = function(file){ load(file$datapath) }
    )
  )

  observeEvent(from_file$data(), {
    data_rv$data <- from_file$data()
    data_rv$name <- from_file$name()
    inputData(data_rv$data)

    shinyjs::hide(id = "desc")
    shinyjs::hide(id = 'importModule')
    shinyjs::show(id = 'updateModule')
  })

  observeEvent(input$hideupdateModule,{
    shinyjs::hide(id = 'updateModule')
  })

  observeEvent(input$`visModule_e-settings`, {
    showModal(modal_settings(aesthetics = input$aesthetics))
  })

  observeEvent(data_rv$data, {

    # define column types
    columnTypes <- defineColumnTypes(data_rv$data)

    # print(columnTypes()) Confirmed

    ## EDA

    # getexc

    exc <- which(!columnTypes() %in% c('numeric'))
    if(length(exc) == 0) {exc <- NULL}

    obj <- board::brief(
      inputData = inputData(),
      exc = exc
    )
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
      choices = colnames(
        inputData()
      ),
      selected = NULL
    )

    output$reactOutput <- renderReactable(
      reactable(
        EDAres
      )
    )

    esquisse_server(id = 'visModule_e', data_rv = data_rv, import_from = NULL)

  })


  ## Main table

  # need to check compatiblity with getDT

  # output$DT <-
  #   imported$data() |>
  #   #inputData() |>
  #   getDT(all = TRUE, columnGroups = columnTypes()) |>
  #   reactable::renderReactable()


  output$DT <- reactable::renderReactable({
    data <- req(data_rv$data)
    reactable::reactable(
      data,
      defaultColDef = reactable::colDef(header = function(value) {
        classes <- tags$div(style = "font-style: italic; font-weight: normal; font-size: small;", get_classes(data[, value, drop = FALSE]))
        tags$div(title = value, value, classes)}
      ),
      columns = list(),
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE
    )
  })

  ## update module

  updated_data <- update_variables_server(
    id = "updateModule_1",
    data = reactive(data_rv$data),
    height = "300px"
  )

  observeEvent(updated_data(), {
    data_rv$data <- updated_data()
    inputData(data_rv$data)
  })

  ## Open and function change

  opened <- reactiveVal(NULL)

  observeEvent(input$ImportFunction, {
    opened(input$ImportFunction)
  })

  observeEvent(input$EDAFunction, {
    opened(input$EDAFunction)
  })

  observeEvent(input$VisFunction, {
    opened(input$VisFunction)
  })

  ## Import

  observeEvent(inputData(), {
    data_rv$data <- inputData()
  })

  mod_filterModule_server(
    id = "filterModule_1",
    inputData = inputData,
    opened = opened
  )

  mod_subsetModule_server(
    id = "subsetModule_1",
    inputData = inputData,
    opened = opened
  )

  mod_mutateModule_server(
    id = "mutateModule_1",
    inputData = inputData,
    opened = opened
  )

  mod_cleanModule_server("cleanModule_1", inputData, opened)

  mod_splitModule_server(
    id = "splitModule_1",
    inputData = inputData,
    opened =  opened
  )

  mod_reshapeModule_server("reshapeModule_1", inputData, opened)

  mod_exportModule_server("exportModule_1", inputData)

  ## Vis

  mod_visModule_server(
    id = "visModule_1",
    inputData = inputData,
    opened = opened,
    plotlyobj = plotlyobj
  )



  ## EDA

  # mod_briefModule_server("briefModule_1", inputData, opened)
  #
  mod_relationModule_server("relationModule_1", inputData, ggobj, opened)

  mod_variableModule_server("variableModule_1", inputData, opened, distobj, distobj2, uiobj)

  # Your application server logic

}

genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

get_classes <- function(data) {
  classes <- lapply(
    X = data,
    FUN = function(x) {
      paste(class(x), collapse = ", ")
    }
  )
  unlist(classes, use.names = FALSE)
}

#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
as_out <- function(x, return_class = c("data.frame", "data.table", "tbl_df")) {
  if (is.null(x))
    return(NULL)
  return_class <- match.arg(return_class)
  is_sf <- inherits(x, "sf")
  x <- if (identical(return_class, "data.frame")) {
    as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    as.data.table(x)
  } else {
    as_tibble(x)
  }
  if (is_sf)
    class(x) <- c("sf", class(x))
  return(x)
}

defineColumnTypes <- function(data){
  tt <-
    data %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather(class)

  tt2 <- tt %>% pull(value)
  names(tt2) <- tt %>% pull(class)
  return(reactive(tt2))
}

modal_settings <- function(aesthetics = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = tagList(
      i18n("Visualization settings"),
      tags$button(
        ph("x"),
        title = i18n("Close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal"
      )
    ),
    tags$label(
      i18n("Select aesthetics to be used to build a graph:"),
      `for` = ns("aesthetics"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"),
      i18n("Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms."),
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("aesthetics"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("fill:"), i18n("fill color for shapes")),
        tagList(tags$b("color:"), i18n("color points and lines")),
        tagList(tags$b("size:"), i18n("size of the points")),
        tagList(tags$b("shape:"), i18n("shape of the points")),
        tagList(tags$b("weight:"), i18n("frequency weights")),
        tagList(tags$b("group:"), i18n("identifies series of points with a grouping variable")),
        tagList(tags$b("ymin:"), i18n("used in ribbons charts with ymax to display an interval between two lines")),
        tagList(tags$b("ymax:"), i18n("used in ribbons charts with ymin to display an interval between two lines")),
        tagList(tags$b("facet:"), i18n("create small multiples")),
        tagList(tags$b("facet row:"), i18n("create small multiples by rows")),
        tagList(tags$b("facet col:"), i18n("create small multiples by columns"))
      ),
      choiceValues = c("fill", "color", "size", "shape", "weight", "group", "ymin", "ymax", "facet", "facet_row", "facet_col"),
      selected = aesthetics %||% c("fill", "color", "size", "facet"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL
  )
}
