#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom DT datatable formatStyle
#' @importFrom shinydashboardPlus box
#' @importFrom htmlwidgets JS
#' @importFrom reactable reactable colGroup
#' @noRd
#' @export

getDT <- function(inputData, all = FALSE, columnGroups = NULL) {
  if (!is.null(columnGroups)) {
    cg <- list()

    tpl <- function(i) {
      if (i == "numeric") {
        return(
          colGroup(
            name = unname(i),
            columns = names(i),
            headerStyle =
              "text-transform: uppercase;
            color: #310D45;
            background-color: #9F5AF6;
            border-radius: 1.25rem;
            font-size:1.1em;
            -webkit-border-radius: 1.25rem;
            -moz-border-radius: 1.25rem;
            -webkit-box-shadow: none;
            -moz-box-shadow: none;
            -box-shadow: none;"
          )
        )
      }
      if (i == "character") {
        return(
          colGroup(
            name = unname(i),
            columns = names(i),
            headerStyle = "text-transform: uppercase;
            color: #310D45;
            background-color: #EBA882;
            border-radius: 1.25rem;
            font-size:1.1em;
            -webkit-border-radius: 1.25rem;
            -moz-border-radius: 1.25rem;
            -webkit-box-shadow: none;
            -moz-box-shadow: none;
            -box-shadow: none;"
          )
        )
      }
      if (i == "factor") {
        return(
          colGroup(
            name = unname(i),
            columns = names(i),
            headerStyle = "text-transform: uppercase;
            color: #310D45;
            background-color: #E57DB9;
            border-radius: 1.25rem;
            font-size:1.1em;
            -webkit-border-radius: 1.25rem;
            -moz-border-radius: 1.25rem;
            -webkit-box-shadow: none;
            -moz-box-shadow: none;
            -box-shadow: none;"
          )
        )
      }
      if (i == "date") {
        return(
          colGroup(
            name = unname(i),
            columns = names(i),
            headerStyle = "text-transform: uppercase;
            color: #310D45;
            background-color: #53AD87;
            border-radius: 1.25rem;
            font-size:1.1em;
            -webkit-border-radius: 1.25rem;
            -moz-border-radius: 1.25rem;
            -webkit-box-shadow: none;
            -moz-box-shadow: none;
            -box-shadow: none;"
          )
        )
      } else {
        (
          print(i)
        )
      }
    }

    for (i in 1:length(columnGroups)) {
      cg[[i]] <- tpl(columnGroups[i])
    }

    columnGroups <- cg
  }

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
        ),
        columnGroups = columnGroups,
        # footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
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
      ),
      # footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
      columnGroups = columnGroups,
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
