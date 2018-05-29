#' runOwosGUI
#'
#' Run the starting command of the owosGUI. Opens the graphical shiny application through which the user
#' can acess the algorithms and visualizations.
#'
#' @usage
#' runOwosGUI()
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
runOwosGUI <- function() {
    appDir <- system.file(package = "owosgui")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `owosgui`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
