#' normalModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normalModule_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' normalModule Server Functions
#'
#' @noRd 
mod_normalModule_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_normalModule_ui("normalModule_1")
    
## To be copied in the server
# mod_normalModule_server("normalModule_1")
