library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyjs)
library(dplyr)

ui <- dashboardPage(
  # HEAD
  dashboardHeader(disable = TRUE),

  # SIDE MODULE
  dashboardSidebar(
    # disable = TRUE
    # LOGO
    htmlOutput("Logo", style = "text-align: center; margin-bottom:3em;"),

    # FILTER BOX
    shinyjs::hidden(
      div(
        id = "SideBox",
        shinydashboardPlus::box(
          title = "Filter",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          status = "navy",
          solidHeader = TRUE,
          gradient = TRUE,
          # boxToolSize = 'xs',
          background = "gray",
          actionButton(
            inputId = "loadFilterColumn",
            label = "Load Variables",
            icon = icon("check")
          ),
          shinyjs::disabled(
            selectInput(
              inputId = "filterColumn",
              label = "filterSelectLabel",
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            ),
            selectInput(
              inputId = "filterOperator",
              label = "filterOpeartorLabel",
              choices = c(">", ">=", "<", "<=", "==", "!="),
              selected = NULL,
              multiple = FALSE
            ),
            textInput(
              inputId = "filterVariable",
              label = "filterVariableLabel"
            ),
            actionButton(
              inputId = "filterButton",
              label = "filter",
              icon = icon("angle-down")
            )
          )
        ),
        shinydashboardPlus::box(
          title = "Subset",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          status = "navy",
          solidHeader = TRUE,
          gradient = TRUE,
          # boxToolSize = 'xs',
          background = "gray",
          actionButton(
            inputId = "loadSubsetColumn",
            label = "Load Variables",
            icon = icon("check")
          ),
          selectInput(
            inputId = "subsetColumn",
            label = "subsetSelectLabel",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          ),
          actionButton(
            inputId = "subsetButton",
            label = "subset",
            icon = icon("angle-down")
          )
        ),
        shinydashboardPlus::box(
          title = "Mutate",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          status = "navy",
          solidHeader = TRUE,
          gradient = TRUE,
          # boxToolSize = 'xs',
          background = "gray",

          # load column
          actionButton(
            inputId = "loadMutateColumn",
            label = "Load Variables",
            icon = icon("check")
          ),

          # which column
          selectInput(
            inputId = "mutateColumn",
            label = "mutateSelectLabel",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          ),

          # option
          selectInput(
            inputId = "mutateOperator",
            label = "mutateOpeartorLabel",
            choices = c("Round", "Log", "Sart", "Min-Max", "Normal", "Remove"),
            selected = NULL,
            multiple = FALSE
          ),

          #
          textInput(
            inputId = "mutateVariable",
            label = "mutateVariableLabel",
          ),

          # apply button
          actionButton(
            inputId = "mutateButton",
            label = "mutate",
            icon = icon("angle-down")
          )
        ),
        box(
          title = "Clean",
          footer = "footer TEXT",
          solidHeader = TRUE,
          background = "teal",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12
        ),
        box(
          title = "Split",
          footer = "footer TEXT",
          solidHeader = TRUE,
          background = "teal",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12
        ),
        box(
          title = "Reshape",
          footer = "footer TEXT",
          solidHeader = TRUE,
          background = "teal",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12
        ),
        box(
          title = "Export",
          footer = "footer TEXT",
          solidHeader = TRUE,
          background = "teal",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12
        )
      )
    ),

    # OUTRO
    actionButton(
      inputId = "Outro",
      label = "Github / Manual",
      style = "margin: auto; width: 90%",
      onclick = "window.open('https://github.com/statgarten', '_blank')",
      icon = icon("github")
    )
  ),
  dashboardBody(
    useShinyjs(),
    fluidPage(
      p(
        id = "desc",
        "Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer eleifend libero eleifend egestas faucibus. Ut venenatis vitae lorem id pulvinar. Phasellus accumsan lectus eu magna hendrerit, ac elementum ex vestibulum. Sed eget facilisis est, quis volutpat mi. Vestibulum metus odio, sollicitudin non venenatis at, dignissim eleifend turpis. Aenean porta porta augue sed pulvinar. Praesent at metus leo. Maecenas consequat luctus odio, sagittis dapibus lectus ultricies vitae. In euismod gravida enim, in tristique felis finibus non. Morbi ac velit sed arcu dignissim ornare. Ut justo velit, lobortis nec purus sed, volutpat commodo nisi. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Maecenas lacinia metus eu nisl aliquet convallis."
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
      ),

      # EXPORT MODULE
      shinyjs::hidden(
        actionButton(
          inputId = "ExportButton",
          label = "ExportButtonLabel",
          icon = icon("download")
        ),
        textOutput("ExportTest")
      ),

      # LOAD TO OTHER MODULE
      shinyjs::hidden(
        actionButton(
          inputId = "LoadButton",
          label = "LoadButtonLabel",
          icon = icon("share")
        ),
        textOutput("LoadTest")
      )
    )
  )
)

server <- function(input, output, session) {
  src <- "https://github.com/rstudio/shiny/blob/main/man/figures/logo.png?raw=true"
  output$Logo <- renderText({
    c('<img width = "100" src="', src, '">')
  })

  inputData <- NULL

  observeEvent(input$fileInputID, {
    file <- input$fileInputID
    ext <- tools::file_ext(file$datapath)

    req(file)

    validate(need(ext == "csv", "Please upload a csv File"))

    shinyjs::hide(id = "desc", anim = TRUE, animType = "slide")
    shinyjs::hide(id = "fileInputID", anim = TRUE, animType = "fade")

    shinyjs::show(id = "ExportButton", anim = TRUE, animType = "slide")
    shinyjs::show(id = "ExportTest", anim = TRUE, animType = "fade")

    shinyjs::show(id = "LoadButton", anim = TRUE, animType = "slide")
    shinyjs::show(id = "LoadTest", anim = TRUE, animType = "fade")

    shinyjs::show(id = "SideBox")

    inputData <<- read.csv(file$datapath)

    output$DT <- renderDT(
      rbind(head(inputData, 5), tail(inputData, 5))
    )
  })

  observeEvent(input$ExportButton, {
    output$ExportTest <- renderText("Export Button Clicked")
    shinyjs::delay(2000, output$ExportTest <- renderText(""))
  })

  observeEvent(input$LoadButton, {
    output$LoadTest <- renderText("Load Button Clicked")
    shinyjs::delay(2000, output$LoadTest <- renderText(""))
  })

  observeEvent(input$loadFilterColumn, {
    shinyjs::enable(id = "filterColumn")
    shinyjs::enable(id = "filterOperator")
    shinyjs::enable(id = "filterVariable")
    shinyjs::enable(id = "filterButton")

    updateSelectizeInput(
      session,
      inputId = "filterColumn",
      label = "filterSelectLabel",
      choices = colnames(inputData),
      server = TRUE
    )
  })

  observeEvent(input$loadMutateColumn, {
    updateSelectizeInput(
      session,
      inputId = "mutateColumn",
      label = "mutateSelectLabel",
      choices = colnames(inputData),
      server = TRUE
    )
  })


  observeEvent(input$filterButton, {
    eval(parse(
      text =
        paste0(
          "inputData <<- inputData %>% ",
          "filter(", input$filterColumn, operator, input$filterVariable, ")"
        )
    ))

    output$DT <- renderDT(
      rbind(head(inputData, 5), tail(inputData, 5))
    )

    updateSelectizeInput(
      session,
      inputId = "filterColumn",
      label = "filterSelectLabel",
      choices = colnames(inputData),
      server = TRUE
    )
  })

  observeEvent(input$mutateButton, {
    if (input$mutateOperator == "Round") {
      eval(parse(
        text =
          paste0(
            "inputData <<- inputData %>% ",
            "mutate(",
            input$mutateColumn, " = round(", input$mutateColumn, ", ", input$mutateVariable, "))"
          )
      ))
    }

    output$DT <- renderDT(
      rbind(head(inputData, 5), tail(inputData, 5))
    )
  })

  observeEvent(input$loadSubsetColumn, {
    updateSelectizeInput(
      session,
      inputId = "subsetColumn",
      label = "subsetSelectLabel",
      choices = colnames(inputData),
      server = TRUE
    )
  })

  observeEvent(input$subsetButton, {
    eval(parse(
      text =
        paste0(
          "inputData <<- inputData %>% ",
          "select(-", input$subsetColumn, ")"
        )
    ))

    output$DT <- renderDT(
      rbind(head(inputData, 5), tail(inputData, 5))
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
