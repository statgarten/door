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
  return(
    reactable(
      inputData,
      defaultColDef = colDef(
        align = 'right',
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
  ###

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
minmax <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#'
#'
#'
normalize <- function(x){
  (x - mean(x)) / sd(x)
}
