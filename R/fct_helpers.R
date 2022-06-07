#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom DT datatable formatStyle
#' @importFrom shinydashboardPlus box
#' @importFrom htmlwidgets JS
#' @importFrom reactable reactable
#' @noRd
#' @export

getDT <- function(inputData, all = FALSE) {

  ### reactable Trial
  if (!all) {
    return(
      reactable(
        rbind(
          inputData |> head(5),
          inputData |> tail(5)
        ),
        defaultColDef = colDef(
          align = "right",
          headerClass = "my-header"
        )
        # footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
        ,
        defaultPageSize = 10,
        minRows = 10,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50),
        compact = TRUE,
        # outlined = TRUE,
        paginationType = "simple",
        showPageInfo = FALSE,
        highlight = TRUE,
        # theme = reactableTheme(
        #     headerStyle = list(
        #
        #     )
        # )
      )
    )
  }
  return(
    reactable(
      inputData,
      defaultColDef = colDef(
        align = "right",
        headerClass = "my-header"
      )
      # footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
      ,
      defaultPageSize = 10,
      minRows = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50),
      compact = TRUE,
      # outlined = TRUE,
      paginationType = "simple",
      showPageInfo = FALSE,
      highlight = TRUE,
      # theme = reactableTheme(
      #     headerStyle = list(
      #
      #     )
      # )
    )
  )
}

boxUI <- function(title, elem, id = NULL) {
  shinydashboardPlus::box(
    title = title,
    collapsible = TRUE,
    collapsed = TRUE,
    width = 12,
    status = "navy",
    solidHeader = TRUE,
    gradient = TRUE,
    background = "gray",
    id = id,
    elem
  )
}

#'
#'
#'
minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#'
#'
#'
normalize <- function(x) {
  (x - mean(x)) / sd(x)
}
