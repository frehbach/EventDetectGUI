#' runGUI
#'
#' Run the starting command of the EventDetectGUI. Opens the graphical shiny application through which the user
#' can acess the algorithms and visualizations.
#'
#' @usage
#' runGUI()
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @import XML
#' @import plotly
#' @import shinyBS
#' @importFrom tools Rd_db
#'
#' @export
runGUI <- function() {
    appDir <- system.file(package = "EventDetectGUI")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `EventDetectGUI`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
