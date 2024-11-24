#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom haven read_sas read_sav read_dta read_xpt
#' @importFrom readxl read_xls read_xlsx
#' @importFrom readr read_rds
#' @importFrom reactable renderReactable colDef
#' @importFrom jstable CreateTableOneJS
#' @importFrom shinydashboardPlus descriptionBlock
#' @importFrom GGally ggcorr
#' @importFrom dplyr select
#' @importFrom plotly renderPlotly
#' @importFrom shinyjs hide show disable enable
#' @importFrom tibble as_tibble
#' @importFrom shinyglide glide screen
#' @importFrom board mod_distributionModule_server
#' @importFrom colorpen mod_mapVisModule_server
#' @importFrom colorpen mod_pairModule_server mod_mosaicModule_server
#' @importFrom shinyalert shinyalert
#'
#' @importFrom scissor mod_roundModule_server mod_roundModule_ui
#' @importFrom scissor mod_etcModule_server mod_etcModule_ui
#' @importFrom scissor mod_logModule_ui mod_logModule_server
#' @importFrom scissor mod_replaceModule_ui mod_replaceModule_server
#' @importFrom scissor mod_binarizeModule_ui mod_binarizeModule_server
#' @importFrom scissor mod_splitModule_ui mod_splitModule_server
#' @importFrom scissor mod_reorderModule_ui mod_reorderModule_server
#' @importFrom scissor mod_exportModule_ui mod_exportModule_server
#' @importFrom scissor mod_gsubModule_ui mod_gsubModule_server
#' @importFrom scissor mod_createModule_ui mod_createModule_server
#' @importFrom soroban mod_treeModule_server
#' @importFrom soroban mod_pcaModule_server
#' @importFrom soroban mod_groupStatModule_server
#' @importFrom soroban mod_mlrModule_server
#' @importFrom soroban mod_kmsModule_server
#'
#' @import datamods
#' @import rmarkdown
#' @importFrom DBI dbConnect dbWriteTable dbListTables dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite

#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2) # file upload size 30mb
  # calling the translator sent as a golem option

  # bookmark
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)

  ## Directory (Translation / Img file)
  app_dir <- system.file(package = "door")

  # i18n
  i18n_shiny <- golem::get_golem_options(which = "translator")
  i18n_shiny$set_translation_language("en")

  i18n_r <- reactive({
    i18n_shiny
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
  tuned_results_list <- reactiveVal(list())

  # Stats

  # Reports
  rmarkdownParams <- reactiveVal(NULL)

  # guideButton render
  observeEvent(input$module, {
    output$guideButton <- renderUI({
      actionButton(
        inputId = paste0(input$module, "Guide"),
        label = NULL,
        icon = icon("info")
      )
    })
  })

  # Guide

  # Default Guide
  observeEvent(input$defaultGuide, {
    showModal(modalDialog(
      shinyglide::glide(
        controls_position = "bottom",
        screen(
          h2("Statgarten user guide"),
          hr(),
          h4("Statgarten aims user to analysis data easily"),
          h4("with utilize R and Shiny as Web page"),
          h4("get more information in ", a("site", href = "https://www.statgarten.com/statgarten", target = "_blank"))
        ),
        screen(
          h2("1. Load data"),
          hr(),
          h4("You can use your own file (from PC)"),
          h4("or URL of data (from online)"),
          h4("or public data from", tags$a("datatoys", href = "https://github.com/statgarten/datatoys", target = "_blank"), "package", )
        ),
        screen(
          h2("2. Data Profile"),
          hr(),
          h4("After upload, in ", tags$b("EDA"), " tab"),
          h4("You can see data descriptions."),
          h4("Check them to further wrangling job")
        ),
        screen(
          h2("3. Data Wrangling"),
          hr(),
          h4("in above panel ", tags$b("Data wrangling")),
          h4("You may wrangle data like Select, Filter, Transform"),
          h4("Also, after ", tags$b("Apply changes"), "you may see data changed in table")
        ),
        screen(
          h2("4. Data Visualize"),
          hr(),
          h4("with ", tags$b("Vis"), " tab"),
          h4("You can generate plots with data"),
          h4("which can be customize and download Image"),
          h4("or ", tags$b("export R code"), " to draw plot")
        ),
        screen(
          h2("5. Stat and ML"),
          hr(),
          h4("This topic is considered as Advanced function so see external manual"),
          h4("Manual for ", a("Stat", href = "https://www.statgarten.com/soroban/articles/usage-groupStat-Module.html", target = "_blank")),
          h4("Manual for ", a("ML", href = "https://github.com/statgarten/stove/tree/main/quarto-doc", target = "_blank"))
        )
      )
    ))
  })

  # Default is in view module
  observeEvent(input$VisGuide, { # Vis
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer = NULL,
        size = "xl",
        shinyglide::glide( # Default Guide
          controls_position = "bottom",
          screen(
            br(),
            tags$img(src = "www/img/guide.004.png", style = "display: block;margin-left: auto;margin-right: auto;width: 50%;")
          ),
          screen(
            br(),
            tags$img(src = "www/img/guide.005.png", style = "display: block;margin-left: auto;margin-right: auto;width: 50%;")
          ),
          screen(
            br(),
            tags$img(src = "www/img/guide.006.png", style = "display: block;margin-left: auto;margin-right: auto;width: 50%;")
          )
        )
      )
    )
  })

  ## import Panel

  # EXAMPLE IMPORT IN URL
  observeEvent(input$exampleURL, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/door/raw/main/example_g1e.xlsx"
    )
  })

  observeEvent(input$exampleReg, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/stove/raw/main/data/boston_r.csv"
    )
  })

  observeEvent(input$exampleCla, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_2-link",
      value = "https://github.com/statgarten/stove/raw/main/data/boston_c.csv"
    )
  })

  # Example import in google sheet
  observeEvent(input$exampleSheet, {
    updateTextInputIcon(
      session = session,
      inputId = "importModule_3-link",
      value = "https://docs.google.com/spreadsheets/d/1Fl_Lec7hZYRgPrLjrRf5kghJ1lDFP20wdXE0JA_H4DM/edit#gid=1751490021"
    )
  })

  # update
  observeEvent(input$showUpdateModule, {
    showModal(modalDialog(
      id = "updateModal",
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
          mod_etcModule_ui("etcModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Binarize"),
          mod_binarizeModule_ui("binModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Split"),
          mod_splitModule_ui(id = "splitModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Subtext"),
          mod_gsubModule_ui("gsubModule_1")
        ),
        tabPanel(
          title = i18n_shiny$t("Create"),
          mod_createModule_ui("createModule_1")
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
    shiny.i18n::update_lang(language = input$lang, session)
    i18n_r()$set_translation_language(input$lang)


    ## Datatoys

    if (input$lang == "en") {
      output$exampleDataset <- renderUI({
        # 22 version
        Choices <- c(
          "Kcalendar",
          "accident",
          "airport",
          "artmuseum",
          "bike",
          "birthRate",
          "bloodTest",
          "budget2023",
          "busStation",
          "busyMetro",
          "carInspection",
          "cheerUp",
          "childAbuse",
          "cinema",
          "coolCenter",
          "crime",
          "crimePlace",
          "drunkdrive",
          "earthShelter",
          "election2020",
          "elevator",
          "farmGIS",
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
          "liquor",
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
          "population",
          "postOffice",
          "restaurant",
          "scholarship",
          "seoulCivic",
          "seoulCulture",
          "seoulER",
          "seoulLibrary",
          "socialCenter",
          "tuition",
          "warmingCenter",
          "weather2020"
        )

        tagList(
          shinyWidgets::pickerInput(
            inputId = "datatoy",
            label = NULL,
            choices = Choices,
            selected = NULL
          ),
          actionButton(
            inputId = "loadExample",
            label = i18n_shiny$t("Import data"),
            icon = icon("arrow-alt-circle-right")
          ),
        )
      })
    }

    if (input$lang == "ko") {
      output$exampleDataset <- renderUI({
        Choices <- c(
          "한국천문연구원 특일 정보" = "Kcalendar",
          "사망교통사고 정보" = "accident",
          "전세계 공항정보" = "airport",
          "서울시립미술관 소장품 정보" = "artmuseum",
          "자전거편의시설" = "bike",
          "통계청 시도/인구동태건수 및 동태율" = "birthRate",
          "2014-15 혈액검사 데이터" = "bloodTest",
          "기획재정부 연도별 세출 및 지출 예산현황" = "budget2023",
          "전국 버스 정류장 위치정보" = "busStation",
          "지하철혼잡도정보" = "busyMetro",
          "자동차검사소 정보" = "carInspection",
          "사회적 약자를 위한 위로의 글" = "cheerUp",
          "아동학대 신고정보" = "childAbuse",
          "영화관상영관인허가정보" = "cinema",
          "무더위쉼터 현황" = "coolCenter",
          "범죄 발생 지역별 통계" = "crime",
          "범죄 발생 장소별 통계" = "crimePlace",
          "음주운전 적발 기록 현황" = "drunkdrive",
          "지진실내구호소 현황" = "earthShelter",
          "국회의원선거 개표결과 정보" = "election2020",
          "국내 승강기 보유 현황" = "elevator",
          "가축사육업 로컬데이터" = "farmGIS",
          "화재통계" = "fire",
          "전국 소방서 정보" = "fireStation",
          "2021 전국푸드뱅크 기부자 통계" = "foodBank",
          "식품영양성분 데이터베이스" = "foodNutrients",
          "전국 주유소 등록현황" = "gasStation",
          "해외진출기업 정보" = "globalBusiness",
          "응급의료기관 및 응급의료지원센터 현황" = "gyeonggiER",
          "병의원 기본정보" = "hospitalInfo",
          "2021 공동주택 공시가격 정보" = "housingPrice",
          "단란주점 영업 정보" = "karaoke",
          "법정동 정보" = "legalDong",
          "주류관련 통계" = "liquor",
          "일반건강검진결과" = "medicalCheckup",
          "의약품 주성분 정보" = "medicine",
          "국민연금사업장 정보" = "nationalPension",
          "생필품가격 정보" = "necessariesPrice",
          "협력국 개발지표 및 ODA 지원 실적" = "odaIndex",
          "소득수준별 ODA 실적통계" = "odaKR",
          "국가별 개발협력동향정보" = "odaNews",
          "공공데이터포털 목록개방현황" = "openData",
          "반려동물 이름 통계" = "petNames",
          "약국 기본정보" = "pharmacyInfo",
          "축산오염원조사정보" = "pollution",
          "인구총조사" = "population",
          "우체국 정보" = "postOffice",
          "맛집 정보" = "restaurant",
          "2020년도 장학금 수혜현황" = "scholarship",
          "2022년 서울 시민생활 데이터" = "seoulCivic",
          "문화공간정보" = "seoulCulture",
          "응급실 위치 정보" = "seoulER",
          "공공도서관 현황정보" = "seoulLibrary",
          "사회복지시설 정보" = "socialCenter",
          "장학금 정보" = "tuition",
          "한파쉼터현황" = "warmingCenter",
          "농업 종관기상 데이터" = "weather2020"
        )

        tagList(
          shinyWidgets::pickerInput(
            inputId = "datatoy",
            label = NULL,
            choices = Choices,
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
        title = h4("Import file"),
        preview_data = FALSE,
        file_extensions = c(
          ".csv", ".dta", ".fst", ".rda", ".rds",
          ".rdata", ".sas7bcat", ".sas7bdat",
          ".sav", ".tsv", ".txt", ".xls", ".xlsx",
          ".xml", ".json", ".sqlite"
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


  # Vis Panel
  plotlyobj <- reactiveVal(NULL)
  output$plot <- renderPlotly(plotlyobj())

  mod_mapVisModule_server("mapVisModule_1", inputData, i18n = i18n_r, lang = reactive({
    input$lang
  }))

  mod_pairModule_server(id = "pairModule_1", inputData)

  mod_mosaicModule_server(id = "mosaicModule_1", inputData)

  # Stat Panel
  mod_pcaModule_server("pcaModule_1", inputData)
  mod_treeModule_server("treeModule_1", inputData)
  mod_kmsModule_server("kmsModule_1", inputData)
  mod_mlrModule_server("mlrModule_1", inputData)
  mod_groupStatModule_server("groupStatModule_1", inputData)

  # not use add to main data feature
  # res_inserted <- mod_groupStatModule_server("groupStatModule_1", inputData)
  # observeEvent(input$add, {
  #   data_rv$data <- res_inserted() # reactive
  #   inputData(data_rv$data) # then use isolated
  # })

  # XML handler
  xml_to_dataframe <- function(path) {
    xml <- xml2::read_xml(path)
    nodeset <- xml2::xml_children(xml)
    result <- c()
    for (i in 1:length(nodeset)) {
      x <- nodeset[i]
      tmp <- xml2::xml_text(xml2::xml_children(x))
      names(tmp) <- xml2::xml_name(xml2::xml_children(x))
      result <- rbind(result, tmp)
    }

    rownames(result) <- NULL

    return(tibble::as_tibble(result))
  }

  ## after data uploaded

  # Rdata load function
  loadRData <- function(fileName) {
    # loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }

  # from file
  from_file <- import_file_server(
    id = "importModule_1",
    read_fns = list(
      tsv = function(file) {
        read.csv(file, sep = "\t")
      },
      sas7bcat = function(file) {
        haven::read_xpt(file) # not use file$datapath
      },
      dta = function(file) {
        haven::read_dta(file)
      },
      rda = function(file) {
        # loadRData(file)

        readRDS(file)
      },
      rdata = function(file) {
        # loadRData(file)
        readRDS(file)
      },
      xml = function(file) {
        xml_to_dataframe(file)
      },
      json = function(file) {
        jsonlite::fromJSON(file)
      },
      sqlite = function(file) {
        con <- dbConnect(SQLite(), file)
        tableName <- dbListTables(con)
        res <- dbGetQuery(con, paste0("SELECT * FROM ", tableName))
        dbDisconnect(con)
        return(res)
      }
    )
  )

  observeEvent(from_file$data(), {
    data_rv$data <- from_file$data()
    data_rv$name <- from_file$name()
    inputData(data_rv$data)
  })

  # from url
  from_url <- import_url_server(id = "importModule_2")

  observeEvent(from_url$data(), {
    data_rv$data <- from_url$data()
    data_rv$name <- from_url$name()

    inputData(data_rv$data)
  })

  # from google sheet
  from_gs <- import_googlesheets_server(id = "importModule_3")

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

      strata <- NULL
      if (input$tableOneStrata != "NULL") strata <- input$tableOneStrata

      tbl <- jstable::CreateTableOneJS(
        vars = setdiff(colnames(data), strata),
        data = data,
        strata = strata
      )$table

      tbl <- tbl[, -c(ncol(tbl) - 1, ncol(tbl))] # remove test / sig column

      tbl[, 1] <- rownames(tbl)
      rownames(tbl) <- NULL
      reactable(
        rownames = FALSE,
        tbl,
        defaultColDef = colDef(
          headerClass = "my-header",
          align = "right"
        ),
        rowClass = "my-row",
        bordered = TRUE,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        columns = list(
          p = colDef(
            name = "P value",
            style = function(value) {
              value <- gsub(" ", "", value) # remove empty space
              color <- "#1e3799"
              if (value < 0.05) {
                color <- "#eb2f06"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          level = colDef(
            align = "center",
            style = "font-weight: bold; border-right: solid 0.5em #785330"
          )
        )
      )
    })
  })

  # datatoy load data
  observeEvent(input$loadExample, {
    githubURL <- paste0("https://github.com/statgarten/datatoys/raw/main/data/", input$datatoy, ".rda")
    datatoyFile <- tempfile()
    download.file(githubURL, datatoyFile)
    load(datatoyFile)
    unlink(datatoyFile)

    eval(parse(text = paste0("data_rv$data <- ", input$datatoy)))
    eval(parse(text = paste0("data_rv$name <- '", input$datatoy, "'")))
    inputData(data_rv$data)
  })

  # Data loaded
  observeEvent(data_rv$data, {
    inputData(data_rv$data) # set data
    # shinyjs::hide(id = "defaultGuide")
    # shinyjs::enable(id = "moduleSelector")
    shinyjs::hide(id = "importModule")

    output$moduleSelector <- renderUI({
      shinyWidgets::radioGroupButtons(
        inputId = "module",
        label = NULL,
        choices = c("EDA", "Vis", "Stat", "ML"),
        selected = "EDA",
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
      label = "Strata",
      choices = names(Filter(is.factor, data_rv$data)),
      selected = numeric(0)
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

    observeEvent(input$corSize, { # change correlation Plot Size
      req(input$corSize)
      if (!is.null(obj$cors)) {
        output$corplot <- renderPlot(GGally::ggcorr(obj$cors), height = input$corSize)
      } # if is not null, draw correlation plot
    })

    output$dataStructure <- renderPrint({
      str(inputData())
    })

    output$dataDimension <- renderUI({
      descriptionBlock( # data dimension
        header = paste0(obj$desc$nrow, " X ", obj$desc$ncol),
        numberIcon = icon("expand"),
        number = i18n_shiny$t("Data Dimension"),
        marginBottom = FALSE
      )
    })

    output$missingData <- renderUI({
      descriptionBlock( # missing data
        header = paste0(obj$desc$missingCellCount, "(", obj$desc$missingCellRatio, "%)"),
        numberIcon = icon("question"),
        number = i18n_shiny$t("Missing Data"),
        marginBottom = FALSE
      )
    })

    updateSelectInput(
      inputId = "variableSelect",
      label = "variable",
      choices = colnames(
        inputData()
      ),
      selected = NULL
    )

    output$reactOutput <- renderReactable({
      reactable(
        EDAres,
        defaultColDef = colDef(headerClass = "my-header"),
        columns = list(
          Name = colDef(align = "center", style = "font-weight: bold; border-right: solid 0.5em #785330"),
          UniqueValues = colDef(name = "Unique Values", align = "center"),
          Zero = colDef(name = "Zero Values", align = "center"),
          Missing = colDef(name = "Missing Values", align = "center"),
          isUniform = colDef(name = "Uniformly distributed", align = "center"),
          isUnique = colDef(name = "Unique values", align = "center")
        ),
        rowClass = "my-row",
        bordered = TRUE,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })
  })

  ## Main table (View)

  output$DT <- reactable::renderReactable({
    data <- req(data_rv$data)
    if (nrow(data) > 100000) {
      data <- rbind(head(data, 50000), tail(data, 50000))
    }

    # XML Handler
    if (!is.null(rownames(data))) rownames(data) <- NULL

    reactable::reactable(
      data,
      defaultColDef = reactable::colDef(
        header = function(value) {
          classes <- tags$div(style = "font-style: italic; font-weight: normal; font-size: small;", get_classes(data[, value, drop = FALSE]))
          tags$div(title = value, value, classes)
        },
        headerClass = "my-header"
      ),
      columns = list(),
      rowClass = "my-row",
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE
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

  res_trans <- mod_etcModule_server(
    id = "etcModule_1",
    inputData = reactive(data_rv$data)
  )

  ## Split Module

  res_split <- mod_splitModule_server(
    id = "splitModule_1",
    inputData = reactive(data_rv$data)
  )

  ## Subtext Module

  res_subtext <- mod_gsubModule_server(
    id = "gsubModule_1",
    inputData = reactive(data_rv$data)
  )

  ## Create Module
  res_create <- mod_createModule_server(
    id = "createModule_1",
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

    if (input$transformPanel == "Split") {
      data_rv$data <- res_split()
    }

    if (input$transformPanel == "Subtext") {
      data_rv$data <- res_subtext()
    }

    if (input$transformPanel == "Create") {
      data_rv$data <- res_create()
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

  ## Import

  observeEvent(inputData(), {
    data_rv$data <- inputData()
  })

  mod_exportModule_server("exportModule_1", inputData)

  ## Vis

  ## EDA
  mod_distributionModule_server("distModule_1", inputData)

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

  mms <- mod_modelingModule_server(
    id = "modelingModule_1",
    splitresult = splitresult,
    # processresult = processresult,
    models_list = models_list,
    tuned_results_list = tuned_results_list
  )
  models_list <- mms$models_list
  tuned_results_list <- mms$tuned_results_list

  ## Report

  observeEvent(input$format, {
    if (input$format == "Paper") {
      if (nzchar(input$report.name) && !grepl("^\\s*$", input$report.name)) {
        shinyjs::enable(id = "downloadReport")
      } else {
        shinyjs::disable(id = "downloadReport")
      }
    } else {
      shinyjs::enable(id = "downloadReport")
    }
  })

  observeEvent(input$report.name, {
    if (input$format == "Paper") {
      if (nzchar(input$report.name) && !grepl("^\\s*$", input$report.name)) {
        shinyjs::enable(id = "downloadReport")
      } else {
        shinyjs::disable(id = "downloadReport")
      }
    }
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(
        switch(input$format,
          PDF = "my-report",
          HTML = "my-report",
          Word = "my-report",
          Dashboard = "my-dashboard",
          PPT = "my-report",
          Paper = "my-paper"
        ),
        sep = ".",
        switch(input$format,
          PDF = "pdf",
          HTML = "html",
          Word = "docx",
          Dashboard = "html",
          PPT = "pptx",
          Paper = "pdf"
        )
      )
    },
    content = function(file) {
      # src <- normalizePath('report.Rmd')
      # owd <- setwd(tempdir())

      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd', overwrite = TRUE)

      out <- rmarkdown::render(
        input = switch(input$format,
          PDF = paste0(app_sys(), "/rmarkdown/report-pdf.rmd"), # rmd
          HTML = paste0(app_sys(), "/rmarkdown/report-html.rmd"), # rmd
          Word = paste0(app_sys(), "/rmarkdown/report-word.rmd"),
          Dashboard = paste0(app_sys(), "/rmarkdown/report-dashboard.rmd"),
          PPT = paste0(app_sys(), "/rmarkdown/report-ppt.rmd"),
          Paper = paste0(app_sys(), "/rmarkdown/arxiv/arxiv.rmd")
        ),
        output_format = switch(input$format,
          PDF = pdf_document(
            toc = TRUE,
            toc_depth = 3,
            number_sections = TRUE,
            highlight = "zenburn"
          ),
          HTML = html_document(
            includes = includes(
              # in_header = paste0(app_sys(), "/header.html"),
              after_body = paste0(app_sys(), "/footer.html")
            ),
            toc = TRUE,
            toc_depth = 3,
            toc_float = list(
              collapsed = FALSE,
              smooth_scroll = FALSE
            ),
            number_sections = TRUE,
            theme = "sandstone",
            highlight = "zenburn"
          ),
          Word = word_document(),
          Dashboard = flexdashboard::flex_dashboard(orientation = "rows", vertical_layout = "scroll"),
          PPT = powerpoint_presentation(),
          Paper = rticles::arxiv_article()
        ),
        params = list(
          inputData = data_rv$data, # REQUIRED
          title = "Statgarten Data Profile Report",
          authorName = input$report.name,
          abstract = "This data profiling report provides a high-level summary and analysis of your data. The report includes topics such as the form and examples of your data, data quality, including checking for missing data and errors, conditions and correlations of variables, outliers, and missing values. We hope you'll utilize this report to gain deeper insights and make more data-driven, strategic decisions.",
          department = input$report.team,
          affiliation = input$report.org,
          location = input$report.location,
          email = input$report.email
          # vec.len= 4
          # positive= NA
          # negative= NA
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
