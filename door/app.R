library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyjs)
library(dplyr)
library(tidyr)

getDT <- function(inputData) {
  datatable(
    rbind(
      inputData |> head(5),
      inputData |> tail(5)
    ),
    rownames = FALSE,
    editable = FALSE,
    extensions = "Buttons",
    selection = "none",
    options = list(
      ordering = FALSE,
      dom = "trB",
      buttons = c(
        "copy",
        "csv",
        "excel",
        "pdf"
      ),
      columnDefs = list(
        list(
          className = "dt-head-center",
          targets = "_all"
        )
      ), # center-align
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        # nodes() X
        # containers() X
        # body() X
        "$(this.api().table().header()).css({'background-color': '#2c3c75', 'color': '#fff'});",
        "}"
      ) # header color
    )
  ) %>%
    DT::formatStyle(
      columns = names(inputData),
      target = "row",
      #  backgroundColor = '#212121',
      #  color = '#fff',
      `border-top` = "0px",
      `text-align` = "right"
    )
}

boxUI <- function(title, elem) {
  shinydashboardPlus::box(
    title = title,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    status = "navy",
    solidHeader = TRUE,
    gradient = TRUE,
    # boxToolSize = 'xs',
    background = "gray",
    elem
  )
}

filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("loadFilterColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    shinyjs::disabled(
      selectInput(
        inputId = ns("filterColumn"),
        label = "filterSelectLabel",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectInput(
        inputId = ns("filterOperator"),
        label = "filterOpeartorLabel",
        choices = c(">", ">=", "<", "<=", "==", "!="),
        selected = NULL,
        multiple = FALSE
      ),
      textInput(
        inputId = ns("filterVariable"),
        label = "filterVariableLabel"
      ),
      actionButton(
        inputId = ns("filterButton"),
        label = "filter",
        icon = icon("angle-down")
      )
    )
  )
}

filterServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadFilterColumn, {
      shinyjs::enable(id = "filterColumn")
      shinyjs::enable(id = "filterOperator")
      shinyjs::enable(id = "filterVariable")
      shinyjs::enable(id = "filterButton")

      updateSelectizeInput(
        session,
        inputId = "filterColumn",
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$filterButton, {
      eval(parse(
        text =
          paste0(
            "inputData(inputData() %>% ",
            "filter(", input$filterColumn, input$filterOperator, input$filterVariable, "))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
      updateSelectizeInput(
        session,
        inputId = "filterColumn",
        label = "filterSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })
  })
}

subsetUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("loadSubsetColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    selectInput(
      inputId = ns("subsetColumn"),
      label = "subsetSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    actionButton(
      inputId = ns("subsetButton"),
      label = "subset",
      icon = icon("angle-down")
    )
  )
}

subsetServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadSubsetColumn, {
      updateSelectizeInput(
        session,
        inputId = "subsetColumn",
        label = "subsetSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$subsetButton, {
      eval(parse(
        text =
          paste0(
            "inputData( inputData() %>% ",
            "select(-", input$subsetColumn, "))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

mutateUI <- function(id) {
  ns <- NS(id)
  tagList(
    # load column
    actionButton(
      inputId = ns("loadMutateColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),

    # which column
    selectInput(
      inputId = ns("mutateColumn"),
      label = "mutateSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    # option
    selectInput(
      inputId = ns("mutateOperator"),
      label = "mutateOpeartorLabel",
      choices = c("Round", "Log", "Sart", "Min-Max", "Normal", "Remove"),
      selected = NULL,
      multiple = FALSE
    ),

    #
    textInput(
      inputId = ns("mutateVariable"),
      label = "mutateVariableLabel",
    ),

    # apply button
    actionButton(
      inputId = ns("mutateButton"),
      label = "mutate",
      icon = icon("angle-down")
    )
  )
}

mutateServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadMutateColumn, {
      updateSelectizeInput(
        session,
        inputId = "mutateColumn",
        label = "mutateSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$mutateButton, {
      if (input$mutateOperator == "Round") {
        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(",
              input$mutateColumn, " = round(", input$mutateColumn, ", ", input$mutateVariable, ")) )"
            )
        ))
      }

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

cleanUI <- function(id) {
  ns <- NS(id)
  tagList(
    ## LOAD COLUMNS
    actionButton(
      inputId = ns("loadCleanColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),

    ## <SELECT> column names
    selectInput(
      inputId = ns("cleanColumn"),
      label = "cleanSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    ## Operation Option: Remove / Replace
    selectInput(
      inputId = ns("cleanOperator"),
      label = "cleanOpeartorLabel",
      choices = c("Remove", "Replace"),
      selected = NULL,
      multiple = FALSE
    ),

    ## Remove / Replace Keyword: null, [userInput]
    textInput(
      inputId = ns("cleanVariable"),
      label = "cleanVariableLabel",
    ),
    textInput(
      inputId = ns("cleanKeyword"),
      label = "cleanKeywordLabel",
    ),

    ## <Button> Clean
    actionButton(
      inputId = ns("cleanButton"),
      label = "clean",
      icon = icon("angle-down")
    )
  )
}

cleanServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadCleanColumn, {
      updateSelectizeInput(
        session,
        inputId = "cleanColumn",
        label = "cleanSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$cleanButton, {
      if (input$cleanOperator == "Remove") {
        # data $ column -> filter(which is not .)

        if (input$cleanVariable == "NA") {
          eval(parse(
            text =
              paste0(
                "inputData( inputData() %>% ",
                "filter(!is.na(", input$cleanColumn, ")))"
              )
          ))
        } else {
          eval(parse(
            text =
              paste0(
                "inputData( inputData() %>% ",
                "filter(!grepl(", input$cleanVariable, ", ", input$cleanColumn, ")))"
              )
          ))
        }
      }

      if (input$cleanOperator == "Replace") {
        keyword <- ifelse(is.null(input$cleanKeyword), "", input$cleanKeyword)

        eval(parse(
          text =
            paste0(
              "inputData( inputData() %>% ",
              "mutate(", input$cleanColumn, " = ",
              "ifelse(", input$cleanColumn, " == ", input$cleanVariable, ",", keyword, ",", input$cleanColumn, ")))"
            )
        ))
      }

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

splitUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("loadSplitColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    selectInput(
      inputId = ns("splitColumn"),
      label = "splitSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),

    # keyword
    textInput(
      inputId = ns("splitkeyword"),
      label = "splitKeywordLabel"
    ),

    # colnameA
    textInput(
      inputId = ns("splitA"),
      label = "colA"
    ),
    # colnameB
    textInput(
      inputId = ns("splitB"),
      label = "colB"
    ),
    actionButton(
      inputId = ns("splitButton"),
      label = "split",
      icon = icon("angle-down")
    )
  )
}

splitServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadSplitColumn, {
      updateSelectizeInput(
        session,
        inputId = "splitColumn",
        label = "splitSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$splitButton, {
      eval(parse(
        text =
          paste0(
            "inputData( inputData() %>% ",
            "tidyr::separate(", input$splitColumn, ", sep = '", input$splitkeyword, "', into = c('", input$splitA, "','", input$splitB, "'), fill = 'right'))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}

exportUI <- function(id){
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("exportFilename"),
      label = "filename"
    ),
    selectInput(
      inputId = ns("exportOption"),
      label = "file extension",
      selected = ".csv",
      multiple = FALSE,
      choices = c(".csv", ".rda", ".sqlite", ".xlsx")
    ),
    downloadButton(
      outputId = ns("exportButton"),
      label = "export"
    )
  )
}

exportServer <- function(id, inputData){
  moduleServer(id, function(input, output, session){
    output$exportButton <- downloadHandler(
      filename = function() {
        if (input$exportFilename == "") {
          return(paste0("downloaded", input$exportOption))
        }
        return(paste0(input$exportFilename, input$exportOption))
      },
      content = function(con) {
        if (input$exportOption == ".csv") {
          write.csv(inputData(), file = con, row.names = FALSE)
        }
        if (input$exportOption == ".rda") {
          save(inputData(), file = con)
        }
        if (input$exportOption == ".sqlite") {
          # need to implement
        }
        if (input$exportOption == ".xlsx") {
          # need to implement
        }
      }
    )
  })
}

ui <- dashboardPage(
  # HEAD
  # skin = 'midnight',
  header = shinydashboardPlus::dashboardHeader(
    title = "HeaderTitle", # chrome tab name
    controlbarIcon = icon("gear", verify_fa = FALSE),
    leftUi = tagList(
      div(
        selectInput("module", label = NULL, choices = c("Import", "Vis"), selected = "Import", width = "10em"),
        style = "margin-bottom: -11.5px; text-align: center; font-weight: bold;"
      )
    )
    # fixed = TRUE
  ),

  # SIDE MODULE
  sidebar = dashboardSidebar(
    minified = FALSE,
    htmlOutput("Logo", style = "text-align: center; margin-bottom:3em; margin-top:3em;"),
    conditionalPanel(
      condition = 'input.module == "Import"',

      # FILTER BOX

      shinyjs::hidden(
        div(
          id = "SideBox",
          boxUI(
            title = "Filter",
            filterUI("filterModule")
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
            title = '!Reshape (not)',
            p('Not Implemented')
          ),
          boxUI(
            title = 'Export',
            exportUI('exportModule')
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
    useShinyjs(),
    fluidPage(
      p(
        id = "desc",
        "Description Text will be here"
      ),
      fileInput(
        inputId = "fileInputID",
        label = "FileInput Label",
        accept = c(".csv"),
        buttonLabel = "Button Label",
        placeholder = "Place Holder"
      ),
      DTOutput(
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

server <- function(input, output, session) {
  src <- "https://github.com/rstudio/shiny/blob/main/man/figures/logo.png?raw=true"
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })

  inputData <- reactiveVal(NULL)

  observeEvent(input$fileInputID, {
    file <- input$fileInputID
    ext <- tools::file_ext(file$datapath)

    req(file)

    validate(need(ext == "csv", "Please upload a csv File"))

    shinyjs::hide(id = "desc", anim = TRUE, animType = "slide")
    shinyjs::hide(id = "fileInputID", anim = TRUE, animType = "fade")

    shinyjs::show(id = "LoadButton", anim = TRUE, animType = "slide")
    shinyjs::show(id = "LoadTest", anim = TRUE, animType = "fade")

    shinyjs::show(id = "SideBox")

    inputData(read.csv(file$datapath))

    output$DT <- renderDT(
      getDT(inputData())
    )
  })

  filterServer(id = "filterModule", inputData)

  subsetServer(id = "subsetModule", inputData)

  mutateServer(id = "mutateModule", inputData)

  cleanServer(id = "cleanModule", inputData)

  splitServer(id = "splitModule", inputData)

  exportServer(id = 'exportModule', inputData)
  
}

# Run the application
shinyApp(ui = ui, server = server)
