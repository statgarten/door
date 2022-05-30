#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom DT datatable formatStyle
#' @importFrom shinydashboardPlus box
#' @importFrom htmlwidgets JS
#' @noRd
#' @export

getDT <- function(inputData) {
  DT::datatable(
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
      ),
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#2c3c75', 'color': '#fff'});",
        "}"
      )
    )
  ) %>%
    DT::formatStyle(
      columns = names(inputData),
      target = "row",
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
    background = "gray",
    elem
  )
}
