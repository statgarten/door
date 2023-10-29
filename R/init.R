#' Install statgarten dependencies.
#'
#' @description Install statgarten dependencies, datatoys, board, scissor, colorpen, soroban, stove
#' @return an HTML list
#'
#' @examples
#' \dontrun{
#'   init()
#' }
#' @importFrom remotes install_github
#' @export

init <- function() {
  # Install datatoys, board, scissor, colorpen, soroban, stove
  remotes::install_github("statgarten/datatoys")
  remotes::install_github("statgarten/board")
  remotes::install_github("statgarten/scissor")
  remotes::install_github("statgarten/colorpen")
  remotes::install_github("statgarten/soroban")
  remotes::install_github("statgarten/stove")
}
