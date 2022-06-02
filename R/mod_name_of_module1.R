#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_name_of_module1_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' name_of_module1 Server Functions
#'
#' @noRd
mod_name_of_module1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")



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

exportUI <- function(id) {
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

#'
#' @importFrom RSQLite dbWriteTable dbConnect dbDriver dbDisconnect
#'
exportServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
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
          saveRDS(inputData, file = con)
        }
        if (input$exportOption == ".sqlite") {
          RSQLite::dbWriteTable(
            conn = RSQLite::dbConnect(
              RSQLite::dbDriver("SQLite"),
              dbname = con
            ),
            name = "data1",
            value = inputData()
          )
          RSQLite::dbDisconnect()
          #con <- dbConnect(drv=RSQLite::SQLite(), dbname="SQLITE_FILENAME")
          #data <- dbGetQuery(conn=con, statement = "SELECT * FROM 'data1'")
        }
        if (input$exportOption == ".xlsx") {
          writexl::write_xlsx(inputData(), path = con)
        }
      }
    )
  })
}
