#' Launch Dynameta 
#' 
#' Launches the Dynameta shiny app 
#' 
#' @return Shiny application object
#' @examples \dontrun{launch_Dynameta()}
#' @import shiny
#' @export
launch_Dynameta <- function() {
  
  # Get app directory - don't include "inst" here as doesn't exist when you install the package from github
  appDir <- base::system.file("Dynameta_app", package = "Dynameta")
  
  # To test app before pushing to git:
  #runApp("inst/Dynameta_app/", display.mode = "normal")
  
  # Run app
  shiny::runApp(appDir, display.mode = "normal")
}

