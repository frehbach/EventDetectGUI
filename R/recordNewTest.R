recordNewTest <- function(){
    requireNamespace("testthat")
    requireNamespace("shinytest")

    shinytest::recordTest("tests/testthat")
}
