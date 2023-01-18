library(shinytest2)

test_that("{shinytest2} recording: shinytest2_test", {
  app <- AppDriver$new(name = "shinytest2_test", height = 1076, width = 1832)
  app$expect_values(output = "sample_sizes_overview")
})
