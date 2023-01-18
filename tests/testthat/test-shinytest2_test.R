# File: tests/testthat/test-shinytest2_test.R

library(shinytest2)

test_that("Dynameta app works", {
  
  # Don't run these tests on the CRAN build servers
  testthat::skip_on_cran()
  
  # Get app directory 
  appdir <- base::system.file("Dynameta_app", package = "Dynameta")
  
  # Run the app test
  shinytest2::test_app(appdir)
  
})




  

