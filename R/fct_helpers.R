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

getDT <- function(inputData, all = FALSE) {

  if(all){
    return(
      DT::datatable(
        inputData,
        rownames = FALSE,
        editable = FALSE,
        selection = "none",
        # extension = 'Scroller',
        options = list(
          ordering = FALSE,
          deferRender = TRUE,
          scrollY = TRUE,
          scrollX = TRUE,
          # scroller = TRUE,
          dom = "tlpr",
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
    )
  }

  DT::datatable(
    rbind(
      inputData |> head(5),
      inputData |> tail(5)
    ),
    rownames = FALSE,
    editable = FALSE,
    selection = "none",
    options = list(
      ordering = FALSE,
      dom = "tr",
      deferRender = TRUE,
      scrollY = TRUE,
      scrollX = TRUE,
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
    id = paste0(title, '_box'),
    elem
  )
}

#'
#'
#'
minmax <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#'
#'
#'
normalize <- function(x){
  (x - mean(x)) / sd(x)
}
