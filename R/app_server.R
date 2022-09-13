#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom haven read_sas read_sav read_dta
#' @importFrom readxl read_xls read_xlsx
#' @importFrom readr read_rds
#' @importFrom reactable renderReactable
#' @importFrom shinydashboardPlus descriptionBlock
#' @importFrom GGally ggcorr
#' @importFrom plotly renderPlotly
#' @importFrom shinyjs hide show
#' @importFrom tibble as_tibble
#' @importFrom shinyglide glide screen
#' @import datamods
#' @import rmarkdown

#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2) # file upload size 30mb
  # calling the translator sent as a golem option

  ## Directory (Translation / Img file)
  app_dir <- system.file(package = "door")

  # i18n
  i18n_shiny <- golem::get_golem_options(which = "translator")
  i18n_shiny$set_translation_language("en")

  i18n_r <- reactive({ i18n_shiny })

  # guideButton render
  observeEvent(input$module, {
    output$guideButton <- renderUI({
      actionButton(
        inputId = paste0(input$module, 'Guide'),
        label = NULL,
        icon = icon('info')
      )
    })
  })

  # Guide
  # Default is in view module
  observeEvent(input$VisGuide, { # Vis
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer = NULL,
        size = 'xl',
        shinyglide::glide( # Default Guide
          controls_position = 'bottom',
          screen(
            br(),
            tags$img(src="www/img/guide.004.png", style = 'display: block;margin-left: auto;margin-right: auto;width: 50%;')
          ),
          screen(
            br(),
            tags$img(src="www/img/guide.005.png", style = 'display: block;margin-left: auto;margin-right: auto;width: 50%;')
          ),
          screen(
            br(),
            tags$img(src="www/img/guide.006.png", style = 'display: block;margin-left: auto;margin-right: auto;width: 50%;')
          )
        )
      )
    )
  })

  ## import Panel

  # datatoy load data
  observeEvent(input$loadExample, {
    eval(parse(text = paste0("data_rv$data <- datatoys::", input$datatoy)))
    data_rv$name <- input$datatoy
    inputData(data_rv$data)
  })

  # EXAMPLE IMPORT IN URL
  observeEvent(input$exampleURL, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/door/raw/main/example_g1e.xlsx"
    )
  })

  observeEvent(input$exampleR, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/goophi/raw/main/data/boston_r.csv"
    )
  })

  observeEvent(input$exampleC, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/goophi/raw/main/data/boston_c.csv"
    )
  })

  # update
  observeEvent(input$showUpdateModule, {
    showModal(modalDialog(
      id = 'updateModal',
      h3(i18n_shiny$t("Update data")),
      ui2(
        id = "updateModule_1" # removed unneccessary part
      ),
      actionButton(
        inputId = "updateModule_1-validate",
        label = tagList(
          phosphoricons::ph("arrow-circle-right"),
          i18n_shiny$t("Apply changes")
        ),
        width = "100%"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # export
  observeEvent(input$showExportModule, {
    showModal(modalDialog(
      h3(i18n_shiny$t("Export data")),
      mod_exportModule_ui(id = "exportModule_1"),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # filter
  observeEvent(input$showFilterModule, {
    showModal(modalDialog(
      h3(i18n_shiny$t("Filter data")),
      datamods::filter_data_ui(id = "filterModule_2", show_nrow = FALSE),
      actionButton(
        inputId = "applyFilter",
        label = tagList(
          phosphoricons::ph("arrow-circle-right"),
          i18n_shiny$t("Apply changes")
        ),
        width = "100%"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # transform
  observeEvent(input$showTransformModule, {
    showModal(modalDialog(
      h3(i18n_shiny$t("Transform data")),
      tabsetPanel(
        id = "transformPanel",
        tabPanel(
          title = i18n_shiny$t("Round"),
          mod_roundModule_ui("roundModule_1")
        ),
        tabPanel( # Log2 / Log / Log10
          title = i18n_shiny$t("Log"),
          mod_logModule_ui("logModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Replace"),
          mod_replaceModule_ui("replaceModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("ETC"),
          mod_etcModlue_ui("etcModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Binarize"),
          mod_binarizeModule_ui("binModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Split"),
          mod_splitModule_ui(id = "splitModule_1")
        )
      ),
      actionButton(
        inputId = "applyTransform",
        label = tagList(
          phosphoricons::ph("arrow-circle-right"),
          i18n_shiny$t("Apply changes")
        ),
        width = "100%"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # reorder
  observeEvent(input$showReorderModule, {
    showModal(modalDialog(
      h3(i18n_shiny$t("Reorder data")),
      mod_reorderModule_ui(id = "reorderModule_1"),
      actionButton(
        inputId = "applyReorder",
        label = tagList(
          phosphoricons::ph("arrow-circle-right"),
          i18n_shiny$t("Apply changes")
        ),
        width = "100%"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # change language
  observeEvent(input$lang, {
    req(input$lang)

    ## Custom
    shiny.i18n::update_lang(session, input$lang)
    i18n_r()$set_translation_language(input$lang)


    ## Datatoys
    require(datatoys, quietly = TRUE)
    if(input$lang == 'en'){
      output$exampleDataset <- renderUI({

        Choices <- c(
          'accident',
          "airport",
          "bloodTest",
          "busStation",
          "carInspection",
          "childAbuse",
          "crime",
          "crimePlace",
          "elevator",
          "fire",
          "fireStation",
          "foodBank",
          "foodNutrients",
          "gasStation",
          "globalBusiness",
          "gyeonggiER",
          "hospitalInfo",
          "housingPrice",
          "karaoke",
          "legalDong",
          "medicalCheckup",
          "medicine",
          "nationalPension",
          "necessariesPrice",
          "odaIndex",
          "odaKR",
          "odaNews",
          "openData",
          "petNames",
          "pharmacyInfo",
          "pollution",
          "postOffice",
          "restaurant",
          "scholarship",
          "seoulER",
          "tuition"
        )

        tagList(
          shinyWidgets::pickerInput(
            inputId = "datatoy",
            label = NULL,
            choices = Choices,
            choicesOpt = list(
              subtext = sapply(Choices, function(i) {
                eval(parse(
                  text = paste0("paste0( nrow(datatoys::", i, '), " x ", ncol(datatoys::', i, ") )")
                ))
              })
            ),
            selected = NULL
          ),
          actionButton(inputId = "loadExample", label = i18n_shiny$t("Load Example data")),
        )
      })
    }

    if(input$lang == 'ko'){
      output$exampleDataset <- renderUI({

        Choices <- c(
          "사망교통사고 정보"='accident',
          "전세계 공항정보"="airport",
          "2014-15 혈액검사 데이터"="bloodTest",
          "전국 버스 정류장 위치정보"= "busStation",
          "자동차검사소 정보"="carInspection",
          "아동학대 신고정보"="childAbuse",
          "범죄 발생 지역별 통계"="crime",
          "범죄 발생 장소별 통계"="crimePlace",
          "국내 승강기 보유 현황"="elevator",
          "화재통계"="fire",
          "전국 소방서 정보"="fireStation",
          "2021 전국푸드뱅크 기부자 통계"="foodBank",
          "식품영양성분 데이터베이스"="foodNutrients",
          "전국 주유소 등록현황"="gasStation",
          "해외진출기업 정보"="globalBusiness",
          "응급의료기관 및 응급의료지원센터 현황"="gyeonggiER",
          "병의원 기본정보"="hospitalInfo",
          "2021 공동주택 공시가격 정보"="housingPrice",
          "단란주점 영업 정보"="karaoke",
          "법정동 정보"="legalDong",
          "일반건강검진결과"="medicalCheckup",
          "의약품 주성분 정보" = "medicine",
          "국민연금사업장 정보"="nationalPension",
          "생필품가격 정보"="necessariesPrice",
          "협력국 개발지표 및 ODA 지원 실적"="odaIndex",
          "소득수준별 ODA 실적통계"="odaKR",
          "국가별 개발협력동향정보"="odaNews",
          "공공데이터포털 목록개방현황"="openData",
          "반려동물 이름 통계"="petNames",
          "약국 기본정보" = "pharmacyInfo",
          "축산오염원조사정보"="pollution",
          "우체국 정보"="postOffice",
          "맛집 정보"="restaurant",
          "2020년도 장학금 수혜현황"="scholarship",
          "응급실 위치 정보"="seoulER",
          "장학금 정보"="tuition"
        )

        tagList(
          shinyWidgets::pickerInput(
            inputId = "datatoy",
            label = NULL,
            choices = Choices,
            choicesOpt = list(
              subtext = sapply(Choices, function(i) {
                eval(parse(
                  text = paste0("paste0( nrow(datatoys::", i, '), " x ", ncol(datatoys::', i, ") )")
                ))
              })
            ),
            selected = NULL
          ),
          actionButton(inputId = "loadExample", label = i18n_shiny$t("Load Example data")),
        )
      })


    }

    # Datamods
    datamods::set_i18n(paste0(app_dir, "/app/www/translations/", input$lang, ".csv"))

    # re-render file module
    output$datamods_import_file <- renderUI({
      datamods::import_file_ui(
        id = "importModule_1",
        preview_data = FALSE,
        file_extensions = c(
          ".csv", ".dta", ".fst", ".rda", ".rds",
          ".rdata", ".sas7bcat", ".sas7bdat",
          ".sav", ".tsv", ".txt", ".xls", ".xlsx"
        )
      )
    })

    output$esquisse_ui2 <- renderUI({
      esquisse_ui(
        id = "visModule_e",
        header = FALSE,
        controls = c("labs", "parameters", "appearance", "code")
      )
    })

    # re-render url module
    output$datamods_import_url <- renderUI({
      datamods::import_url_ui(id = "importModule_2")
    })
    # re-render googlesheet module
    output$datamods_import_googlesheets <- renderUI({
      datamods::import_googlesheets_ui(id = "importModule_3")
    })
  })

  require(tibble, quietly = TRUE)

  ## reactive data declare

  # import
  data_rv <- reactiveValues(data = NULL)
  inputData <- reactiveVal(NULL)
  columnTypes <- reactiveVal(NULL)

  # ML
  data_ml <- reactiveValues(train = NULL, test = NULL)
  trainData <- reactiveVal(NULL)
  testData <- reactiveVal(NULL)
  models_list <- reactiveVal(list())

  # Stats

  # Reports
  rmarkdownParams <- reactiveVal(NULL)

  # EDA Plot
  distobj <- reactiveVal(NULL) # variable histogram
  distobj2 <- reactiveVal(NULL) # variable pie chart
  uiobj <- reactiveVal(NULL) # variable quantitle box

  output$distplot <- renderPlot(distobj())
  output$distplot2 <- renderPlot(distobj2())
  output$distBox <- renderUI(uiobj())

  # Vis Panel
  plotlyobj <- reactiveVal(NULL)
  output$plot <- renderPlotly(plotlyobj())

  mod_mapVisModule_server("mapVisModule_1", inputData, i18n = i18n_r, lang = reactive({input$lang}))

  # Stat Panel
  mod_pcaModule_server("pcaModule_1", inputData)
  mod_treeModule_server("treeModule_1", inputData)
  mod_kmsModule_server("kmsModule_1", inputData)
  mod_mlrModule_server("mlrModule_1", inputData)
  mod_groupStatModule_server("groupStatModule_1", inputData)


  ## after data uploaded

  # from file
  from_file <- import_file_server(
    id = "importModule_1",
    read_fns = list(
      tsv = function(file) {
        read.csv(file$datapath, sep = "\t")
      },
      sas7bcat = function(file) {
        haven::read_sas(file$datapath)
      },
      dta = function(file) {
        haven::read_dta(file$datapath)
      },
      rda = function(file) {
        load(file$datapath)
      },
      rdata = function(file) {
        load(file$datapath)
      }
    )
  )

  observeEvent(from_file$data(), {
    data_rv$data <- from_file$data()
    data_rv$name <- from_file$name()
    inputData(data_rv$data)
  })

  # from url
  from_url <- import_url_server( id = "importModule_2" )

  observeEvent(from_url$data(), {
    data_rv$data <- from_url$data()
    data_rv$name <- from_url$name()
    inputData(data_rv$data)
  })

  # from google sheet
  from_gs <- import_googlesheets_server( id = "importModule_3" )

  observeEvent(from_gs$data(), {
    data_rv$data <- from_gs$data()
    data_rv$name <- from_gs$name()
    inputData(data_rv$data)
  })

  # stat: table one
  observeEvent(input$generateTable, {
    req(input$generateTable)

    output$tableOne <- renderReactable({
      data <- inputData()
      reactable::reactable(
        jstable::CreateTableOneJS(
          vars = setdiff(colnames(data), input$tableOneStrata),
          data = data,
          strata = input$tableOneStrata
        )$table
      )
    })
  })

  # Data loaded
  observeEvent(data_rv$data, {
    inputData(data_rv$data) # set data

    # shinyjs::enable(id = "moduleSelector")
    shinyjs::hide(id = 'importModule')

    output$moduleSelector <- renderUI({
      shinyWidgets::radioGroupButtons(
        inputId = "module",
        label = NULL,
        choices = c("Vis", "EDA", "Stat", "ML"),
        selected = "Vis",
        individual = FALSE,
        size = "lg", # xs (v), sm (v), normal (v), lg (v)
        width = "100%",
        # direction = 'vertical', NOPE
        justified = TRUE
      )
    })

    # tableone update
    updateSelectInput(
      inputId = "tableOneStrata",
      label = "Group by",
      choices = colnames(data_rv$data),
      selected = NULL
    )

    # esquisse render
    output$esquisse_ui2 <- renderUI({
      esquisse_ui(
        id = "visModule_e",
        header = FALSE,
        controls = c("labs", "parameters", "appearance", "code")
      )
    })

    esquisse_server(
      id = "visModule_e",
      data_rv = data_rv,
      default_aes = reactive(input$aes),
      import_from = NULL
    )

    ## NOTE: double ui2 declare not work

    # Module -> Body
    show(id = "viewModule")

    # define column types
    columnTypes <- defineColumnTypes(data_rv$data)

    ## EDA
    obj <- board::brief(inputData = inputData())
    obj$unif <- ifelse(obj$unif, "True", NA)
    obj$uniq <- ifelse(obj$uniq, "True", NA)

    rmarkdownParams <<- do.call("reactiveValues", obj)

    EDAres <- data.frame(
      Name = obj$names,
      UniqueValues = obj$cards,
      Zero = obj$zeros,
      Missing = obj$miss,
      isUniform = obj$unif,
      isUnique = obj$uniq
    )

    if (!is.null(obj$cors)) {
      output$corplot <- renderPlot(GGally::ggcorr(obj$cors))
    } # if is not null, draw correlation plot

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
        EDAres,
        bordered = TRUE,
        compact = TRUE,
        striped = TRUE
      )
    )
  })

  ## Main table (View)

  output$DT <- reactable::renderReactable({
    data <- req(data_rv$data)
    if (nrow(data) > 1000) {
      data <- rbind(head(data, 500), tail(data, 500))
    }
    reactable::reactable(
      data,
      defaultColDef = reactable::colDef(header = function(value) {
        classes <- tags$div(style = "font-style: italic; font-weight: normal; font-size: small;", get_classes(data[, value, drop = FALSE]))
        tags$div(title = value, value, classes)
      }),
      columns = list(),
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE
    )
  })

  # table on import (realtime updates)
  output$table <- reactable::renderReactable({
    reactable::reactable(data_rv$data)
  })

  ## update module

  updated_data <- update_variables_server(
    id = "updateModule_1",
    data = reactive(data_rv$data),
    height = "400px"
  )

  observeEvent(updated_data(), {
    data_rv$data <- updated_data() # reactive
    inputData(data_rv$data) # then use isolated
  })

  ## filter module

  res_filter <- datamods::filter_data_server(
    id = "filterModule_2",
    data = reactive(data_rv$data)
  )

  observeEvent(input$applyFilter, {
    data_rv$data <- res_filter$filtered() # reactive
    inputData(data_rv$data) # then use isolated
  })

  ### TRANSFORMS

  ## round module

  res_round <- mod_roundModule_server(
    id = "roundModule_1",
    inputData = reactive(data_rv$data)
  )

  ## log Module

  res_log <- mod_logModule_server(
    id = "logModule_1",
    inputData = reactive(data_rv$data)
  )

  ## replace Module

  res_replace <- mod_replaceModule_server(
    id = "replaceModule_1",
    inputData = reactive(data_rv$data)
  )

  ## binarize Module

  res_binary <- mod_binarizeModule_server(
    id = "binModule_1",
    inputData = reactive(data_rv$data)
  )

  ## etc Module

  res_trans <- mod_etcModlue_server(
    id = "etcModule_1",
    inputData = reactive(data_rv$data)
  )

  ## Split Module

  res_split <- mod_splitModule_server(
    id = "splitModule_1",
    inputData = reactive(data_rv$data)
  )

  ## transform apply

  observeEvent(input$applyTransform, {
    if (input$transformPanel == "Round") {
      data_rv$data <- res_round()
    }
    if (input$transformPanel == "Log") {
      data_rv$data <- res_log()
    }
    if (input$transformPanel == "Replace") {
      data_rv$data <- res_replace()
    }
    if (input$transformPanel == "Binarize") {
      data_rv$data <- res_binary()
    }
    if (input$transformPanel == "ETC") {
      data_rv$data <- res_trans()
    }
    inputData(data_rv$data) # then use isolated
  })


  observeEvent(input$applySplit, {
    data_rv$data <- res_split() # reactive
    inputData(data_rv$data) # then use isolated
  })


  ## reorder Module

  res_reorder <- mod_reorderModule_server(
    id = "reorderModule_1",
    inputData = reactive(data_rv$data)
  )

  observeEvent(input$applyReorder, {
    data_rv$data <- res_reorder() # reactive
    inputData(data_rv$data) # then use isolated
  })

  ## observeEvent

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

  mod_reshapeModule_server("reshapeModule_1", inputData, opened)

  mod_exportModule_server("exportModule_1", inputData)

  ## Vis

  ## EDA

  mod_variableModule_server("variableModule_1", inputData, distobj, distobj2, uiobj)

  ## ML

  ### split
  splitresult <- mod_ttSplitModule_server(
    id = "ttSplitModule_1",
    inputData = reactive(data_rv$data)
  )

  # required?
  observeEvent(input$applyML, {
    data_ml$train <- splitresult()$train # reactive
    trainData(data_ml$train) # then use isolated

    data_ml$test <- splitresult()$test # reactive
    testData(data_ml$test) # then use isolated
  })


  ### preprocess
  # processresult <- mod_preprocessModule_server(
  #   id = "preprocessModule_1",
  #   splitresult = reactive(splitresult)
  # )

  ### model
  models_list <- mod_modelingModule_server(
    id = "modelingModule_1",
    splitresult = splitresult,
    # processresult = processresult,
    models_list = models_list
  )

  ## Report

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report",
        sep = ".",
        switch(input$format,
          PDF = "pdf",
          HTML = "html",
          Word = "docx"
        )
      )
    },
    content = function(file) {

      # src <- normalizePath('report.Rmd')
      # owd <- setwd(tempdir())

      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd', overwrite = TRUE)

      setwd(app_sys())

      out <- rmarkdown::render(
        params = list(
          inputData = data_rv$data
        ),
        input = "report.Rmd",
        output_format = switch(input$format,
          PDF = pdf_document(),
          HTML = html_document(),
          Word = word_document()
        )
      )
      file.rename(out, file)
    }
  )
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
  if (is.null(x)) {
    return(NULL)
  }
  return_class <- match.arg(return_class)
  is_sf <- inherits(x, "sf")
  x <- if (identical(return_class, "data.frame")) {
    as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    as.data.table(x)
  } else {
    as_tibble(x)
  }
  if (is_sf) {
    class(x) <- c("sf", class(x))
  }
  return(x)
}

defineColumnTypes <- function(data) {
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
      i18n_shiny$t("Visualization settings"),
      tags$button(
        ph("x"),
        title = i18n_shiny$t("Close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal"
      )
    ),
    tags$label(
      i18n_shiny$t("Select aesthetics to be used to build a graph:"),
      `for` = ns("aesthetics"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"),
      i18n_shiny$t("Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms."),
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("aesthetics"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("fill:"), i18n_shiny$t("fill color for shapes")),
        tagList(tags$b("color:"), i18n_shiny$t("color points and lines")),
        tagList(tags$b("size:"), i18n_shiny$t("size of the points")),
        tagList(tags$b("shape:"), i18n_shiny$t("shape of the points")),
        tagList(tags$b("weight:"), i18n_shiny$t("frequency weights")),
        tagList(tags$b("group:"), i18n_shiny$t("identifies series of points with a grouping variable")),
        tagList(tags$b("ymin:"), i18n_shiny$t("used in ribbons charts with ymax to display an interval between two lines")),
        tagList(tags$b("ymax:"), i18n_shiny$t("used in ribbons charts with ymin to display an interval between two lines")),
        tagList(tags$b("facet:"), i18n_shiny$t("create small multiples")),
        tagList(tags$b("facet row:"), i18n_shiny$t("create small multiples by rows")),
        tagList(tags$b("facet col:"), i18n_shiny$t("create small multiples by columns"))
      ),
      choiceValues = c("fill", "color", "size", "shape", "weight", "group", "ymin", "ymax", "facet", "facet_row", "facet_col"),
      selected = aesthetics %||% c("fill", "color", "size", "facet"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL
  )
}

#' @import datamods
ui2 <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "datamods-update",
    style = "overflow-y: auto; overflow-x: hidden;",
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
