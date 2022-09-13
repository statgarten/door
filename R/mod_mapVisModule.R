#' mapVisModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
mod_mapVisModule_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column( # Result Area
        width = 9,
        leafletOutput(outputId = ns("mymap"))
      ),
      column( # Options
        width = 3,
        fluidRow(
          style = 'margin:auto',
          selectInput(ns("x"), label = "", choices = NULL, width = '100%'),
          selectInput(ns("y"), label = "", choices = NULL, width = '100%'),
          sliderInput(
            ns("radius"),
            label = i18n$t("marker size"),
            min = 1, max = 10, value = 5, step = 1, ticks = FALSE,
            width = '100%'
          ),
          checkboxInput(
            ns("cluster"),
            label = i18n$t("Group marker"),
            width = '100%'
          ),
          div(
            id = ns("div"),
            selectInput(ns("color"), "", choices = NULL, width = '100%'),
            sliderInput(
              ns("opacity"),
              label = i18n$t("alpha"),
              min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE,
              width = '100%'
            )
          ),
          actionButton( # Main action
            ns("draw"),
            label = tagList(
              phosphoricons::ph("arrow-circle-right"),
              i18n$t("Draw")
            ),
            style = 'font-weight: bold;background: #3EC70B;color: white;',
            width = "100%"
          )
        )
      )
    )
  )
}

#' mapVisModule Server Functions
#'
#' @noRd
mod_mapVisModule_server <- function(id, inputData, i18n, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(input$cluster, {
      if (input$cluster) {
        shinyjs::hide(id = "div")
      } else {
        shinyjs::show(id = "div")
      }
    })

    observeEvent(list(inputData(), lang()), {
      data <- inputData()

      updateSelectizeInput(
        session,
        inputId = "x",
        label = i18n()$t("longitude column (X)"),
        choices = names(Filter(is.numeric, data)), # Numeric only
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "y",
        label = i18n()$t("latitude column (Y)"),
        choices = names(Filter(is.numeric, data)), # Numeric only
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "color",
        label = i18n()$t("color column"),
        choices = union(names(Filter(is.numeric, data)), names(Filter(is.factor, data))),
        server = TRUE,
        selected = NULL
      )
    })

    observeEvent(input$draw, {
      data <- inputData()
      m <- generateMap(
        x <- data[[input$x]],
        y <- data[[input$y]],
        colorVariable = data[[input$color]],
        fillOpacity = input$opacity,
        radius = input$radius,
        cluster = input$cluster
      )
      output$mymap <- renderLeaflet(m)
    })
  })
}

## To be copied in the UI
# mod_mapVisModule_ui("mapVisModule_1")

## To be copied in the server
# mod_mapVisModule_server("mapVisModule_1")

generateMap <- function(x, y, colorVariable = NULL, radius = 10, fillOpacity = 0.5, cluster = FALSE) {
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  if (is.factor(colorVariable)) { # factor
    palettes <- gg_color_hue(length(unique(colorVariable)))
    pal <- colorFactor(palettes, domain = unique(colorVariable))
  } else {
    if (is.numeric(colorVariable)) { # numeric
      pal <- colorNumeric("RdYlBu", domain = NULL)
    }
  }

  if (cluster) {
    return(
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          lng = x,
          lat = y,
          color = pal(colorVariable),
          stroke = FALSE,
          radius = radius,
          fillOpacity = fillOpacity,
          clusterOptions = markerClusterOptions()
        )
    )
  }

  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lng = x,
      lat = y,
      color = pal(colorVariable),
      stroke = FALSE,
      radius = radius,
      fillOpacity = fillOpacity
    ) %>%
    addLegend(
      pal = pal,
      values = colorVariable
    )
}
