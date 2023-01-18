library(shinytest2)

test_that("{shinytest2} recording: shinytest2_test", {
  app <- AppDriver$new(name = "shinytest2_test", height = 1076, width = 1832)
  app$set_inputs(map_center = c(27.421875, 28.3043806829628), allow_no_input_binding_ = TRUE)
  app$set_inputs(threat = "9 Pollution")
  app$expect_values(output = "threat_details_table")
})
