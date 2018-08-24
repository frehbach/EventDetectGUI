require(shinytest)

app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("test_csvLoad")
app$uploadFile(inputFile = system.file("/extdata/test_station_b.csv",package="EventDetectGUI"))
app$setInputs(csvSelectColumns = TRUE)
app$setInputs(csvColumnCheckBox = c("B_COND_VAL", "B_PLNT_PRES_OP", "B_PLNT_TURB_VAL"))
app$snapshot(list(output = "outDataHead"))
