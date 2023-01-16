library(shinytest)

test_that("Running a model with Dynameta app works", {
  
  # Don't run these tests on the CRAN build servers
  testthat::skip_on_cran()
  
  # Get app directory 
  appdir <- base::system.file("Dynameta_app", package = "Dynameta")
  
  # Run the app test
  # Use compareImages=FALSE: Should screenshots be compared? It can be useful to set this to FALSE
  # when the expected results were taken on a different platform from the one currently being used to test the application. 
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
  
})