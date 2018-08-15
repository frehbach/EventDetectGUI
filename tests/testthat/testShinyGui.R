context("Shiny GUI Interactive Tests")

runGUITests <- function(testNames){
    require("EventDetectGUI")
    require("tools")

    for(testName in testNames){
        cat(paste("\nStarting a test:", testName))
        shinytest::expect_pass(shinytest::testApp(".",testName))
    }
}


runGUITests(c("test_csvLoad"))
