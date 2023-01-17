app <- ShinyDriver$new("../../")
app$snapshotInit("shinytest_test")

app$setInputs(map_center = c(27.421875, 28.3043806829628), allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(threat = "9 Pollution")
app$snapshot()
