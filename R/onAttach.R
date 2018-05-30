#' Runs on package attach
#'
#' The package startup message is defined here. It shall give a clue on how to
#' start the owosGUI and which suggests might want to be installed.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    packageStartupMessage("*** Welcome to the owosgui package ***\n\n
                          There is only one command that you will need to use the owosgui:\n
                          runOwosGUI()\n\n
                          ")
}
