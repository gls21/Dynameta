#' Launch Dynameta 
#' 
#' Launches the Dynameta shiny app 
#' 
#' @return Shiny application object
#' @examples \dontrun{launch_Dynameta()}
#' @import shiny
#' @export
launch_Dynameta <- function() {
  
  # Get app directory 
  appDir <- base::system.file("inst", "Dynameta_app", package = "Dynameta")
  
  # Run app
  shiny::runApp(appDir, display.mode = "normal")
}

