#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import shiny.i18n
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL, uiPattern = "/", ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      translator = shiny.i18n::Translator$new(
        translation_json_path = paste0(system.file(package = "door"), "/app/www/translations/translation.json")
      )
    )
  )
}
