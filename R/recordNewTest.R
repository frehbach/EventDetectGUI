#' Record a new GUI test sequence
#'
#' Simply run 'recordNewTest()' and the test creation GUI will pop up.
#'
#' @keywords internal
recordNewTest <- function(){
    requireNamespace("testthat")
    requireNamespace("shinytest")

    shinytest::recordTest("tests/testthat")
}
