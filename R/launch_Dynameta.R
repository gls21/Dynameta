#' Launch Dynameta 
#' 
#' Launches the Dynameta shiny app 
#' 
#' @return shiny application object
#' 
#' @examples \dontrun{launch_Dynameta()}
#' 
#' @import shiny


# wrapper for shiny::shinyApp()
launch_Dynameta <- function() {
  
  shiny::shinyAppDir(ui = Dynameta_UI, server = Dynameta_Server)
}

# Also need runApp? - Don't think so as shinyApp will work and launch the app 
# as long as it is the last thing called by the function. If doesn't work, may 
# have to use runApp(). 

# Also may need to change it to shinyAppDir() function over shinyApp()
# as not sure this will work, because shinyApp() is usually for when the server and ui 
# are in the same script