# wrapper for shiny::shinyApp()

launchApp <- function() {
  
  # Not sure if this is meant to go here 
  # Get the UKCEH theme
  devtools::source_url("https://github.com/NERC-CEH/UKCEH_shiny_theming/blob/main/theme_elements.R?raw=TRUE")
  
  shinyApp(ui = Dynameta_UI, server = Dynameta_Server)
}

# Also need runApp?