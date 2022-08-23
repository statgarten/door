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
mod_mapVisModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(outputId = ns("mymap")),
    selectInput(ns("x"), "x", choices = NULL),
    selectInput(ns("y"), "y", choices = NULL),
    selectInput(ns("color"), "color", choices = NULL),
    numericInput(ns("opacity"), "alpha", min = 0, max = 1, value = 0.5, step = 0.1),
    numericInput(ns("radius"), "size", min = 1, max = 10, value = 5, step = 1),
    checkboxInput(ns("cluster"), "cluster"),
    actionButton(ns("draw"), "draw")
  )
}

#' mapVisModule Server Functions
#'
#' @noRd
mod_mapVisModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)


    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        session,
        inputId = "x",
        label = "x",
        choices = c(colnames(data)),
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "y",
        label = "y",
        choices = c(colnames(data)),
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "color",
        label = "color",
        choices = c(colnames(data)),
        server = TRUE,
        selected = NULL
      )
    })

    observeEvent(input$draw, {
      data <- inputData()
      m <- generateMap(
        x <- data[[input$x]],
        y <- data[[input$y]],
        colorVariable = as.factor(data[[input$color]]),
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
  }

  if (is.numeric(colorVariable)) { # numeric
    pal <- colorNumeric("RdYlBu", domain = NULL)
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
    )
}
